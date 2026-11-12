(ns hyperopen.wallet.smart-account
  "Smart-contract accounts (e.g. a Safe on HyperEVM).

   HyperCore only accepts 65-byte ECDSA signatures and recovers the account
   from them, so a contract account cannot sign `approveAgent` or any other
   user-signed action. It can, however, send HyperEVM transactions, and the
   CoreWriter system contract turns those into HyperCore actions for the
   sending contract.

   - `detect!` reads the connected address's code on HyperEVM. Code that is
     not an EIP-7702 delegation (`0xef0100…`) marks a smart account; a
     delegated EOA can still sign, so it keeps the normal flow.
   - `submit-action!` sends a user-signed exchange action as a CoreWriter
     transaction from the account and then waits until HyperCore shows the
     effect. The transaction hash is never trusted: a Safe answers
     `eth_sendTransaction` with a Safe transaction hash or a simulated one,
     and the transaction may only execute once the owners have signed it. A
     CoreWriter call can also succeed on HyperEVM while HyperCore rejects
     the action."
  (:require [clojure.string :as str]
            [hyperopen.account.context :as account-context]
            [hyperopen.funding.infrastructure.wallet-rpc :as wallet-rpc]
            [hyperopen.hyperevm.domain.chain :as chain]
            [hyperopen.hyperevm.domain.core-writer :as core-writer]
            [hyperopen.hyperevm.infrastructure.rpc :as rpc]))

(def wallet-chain-config
  (assoc chain/mainnet
         :network-label "HyperEVM"
         :verify-switch? true))

(def default-poll-ms 3000)

(def default-timeout-ms
  "Safe owners sign and execute on their own time; keep waiting a while
   before telling the user the action hasn't been applied yet."
  (* 15 60 1000))

(def not-applied-message
  (str "Your smart account sent the transaction, but Hyperliquid hasn't applied it yet. "
       "It takes effect once the account's owners sign and execute it; check your balances "
       "before trying again."))

;; --- detection -------------------------------------------------------------------

(defn- normalize
  [address]
  (some-> address str str/trim str/lower-case not-empty))

(defn contract-code?
  "Whether `code` (an `eth_getCode` result) is a contract rather than an EOA
   or an EIP-7702 delegated EOA."
  [code]
  (let [code* (some-> code str str/trim str/lower-case)]
    (boolean (and (seq code*)
                  (not= "0x" code*)
                  (not (re-matches #"^0x0*$" code*))
                  (not (str/starts-with? code* "0xef0100"))))))

(defn smart-account?
  "Whether the connected wallet is a detected smart-contract account."
  [state]
  (let [{:keys [address kind]} (get-in state [:wallet :smart-account])]
    (and (= :contract kind)
         (some? address)
         (= address (normalize (get-in state [:wallet :address]))))))

(defn- cached-kind
  [state address]
  (let [entry (get-in state [:wallet :smart-account])]
    (when (= (normalize address) (:address entry))
      (:kind entry))))

(defn may-be-smart-account?
  "False when `address` is known not to be a smart account we would send
   for: not the connected wallet, or already detected as an EOA."
  [state address]
  (let [address* (normalize address)]
    (and (some? address*)
         (= address* (normalize (get-in state [:wallet :address])))
         (not= :eoa (cached-kind state address*)))))

(defn- get-code!
  [address]
  (rpc/rpc-request! {} "eth_getCode" [address "latest"]))

(defn detect!
  "Resolve whether `address` is a smart account, reading its HyperEVM code
   once per address and caching the answer under `[:wallet :smart-account]`.
   A failed read resolves false and is not cached."
  ([store address]
   (detect! store address {}))
  ([store address {:keys [get-code-fn] :or {get-code-fn get-code!}}]
   (let [address* (normalize address)]
     (if-let [kind (and address* (cached-kind @store address*))]
       (js/Promise.resolve (= :contract kind))
       ;; Only the connected wallet can be a smart account we send for.
       (if-not (and address*
                    (= address* (normalize (get-in @store [:wallet :address]))))
         (js/Promise.resolve false)
         (-> (js/Promise.resolve (get-code-fn address*))
             (.then (fn [code]
                      (let [kind (if (contract-code? code) :contract :eoa)]
                        (when (= address* (normalize (get-in @store [:wallet :address])))
                          (swap! store assoc-in [:wallet :smart-account]
                                 {:address address* :kind kind}))
                        (= :contract kind))))
             ;; Unknown is treated as an EOA: its signature request fails
             ;; visibly, whereas a CoreWriter transaction would not.
             (.catch (fn [_] false))))))))

(defn install-detection-watch!
  "Detect the connected wallet's account kind whenever its address changes,
   so views (e.g. the HyperEVM gas check) know about a smart account before
   the first action."
  [store]
  (add-watch store ::detection
             (fn [_ _ old-state new-state]
               (let [address (get-in new-state [:wallet :address])]
                 (when (and (seq address)
                            (not= address (get-in old-state [:wallet :address])))
                   (detect! store address)))))
  (when-let [address (not-empty (get-in @store [:wallet :address]))]
    (detect! store address)))

;; --- confirmation ------------------------------------------------------------------

(defn- number-of
  [value]
  (let [n (js/parseFloat (str value))]
    (when-not (js/isNaN n) n)))

(defn- same-amount?
  [a b]
  (let [a* (number-of a)
        b* (number-of b)]
    (boolean (and a* b* (<= (js/Math.abs (- a* b*))
                            (max 1e-9 (* 1e-6 (js/Math.abs b*))))))))

(defn- wei->hype
  [wei]
  (some-> (number-of wei) (/ 1e8)))

(defn- entry-key
  [entry]
  (or (:hash entry) (pr-str entry)))

(defn- new-entries
  "Entries of `response` (a vector of `{:hash :delta}`) not in `baseline`."
  [response baseline]
  (let [seen (set (map entry-key baseline))]
    (remove #(contains? seen (entry-key %)) (when (sequential? response) response))))

(defn- new-delta?
  "A predicate over `(response baseline)` that holds once a new ledger entry
   whose delta satisfies `match?` appears."
  [match?]
  (fn [response baseline]
    (boolean (some (comp match? :delta) (new-entries response baseline)))))

(defn- token-name
  [wire-id]
  (some-> wire-id str (str/split #":" 2) first str/upper-case))

(defn- spot-total
  [response token-index]
  (or (some (fn [{:keys [token total]}]
              (when (= token token-index) (number-of total)))
            (:balances response))
      0))

(defn- confirmation
  "How to tell from the info API that HyperCore applied this very `action`
   for `owner`: a request and a predicate over `(response baseline)`.
   Ledger-based checks look for a NEW entry matching the action's type,
   amount and destination, so an unrelated change does not count."
  [action owner now-ms]
  (let [ledger {:type "userNonFundingLedgerUpdates"
                :user owner
                :startTime (- now-ms (* 24 60 60 1000))}]
    (case (:type action)
      "approveAgent"
      (let [agent (normalize (:agentAddress action))]
        {:request {:type "userRole" :user agent}
         :applied? (fn [response _]
                     (and (= "agent" (:role response))
                          (= owner (normalize (get-in response [:data :user])))))})

      "usdClassTransfer"
      {:request ledger
       :applied? (new-delta? (fn [{:keys [type usdc toPerp]}]
                               (and (= "accountClassTransfer" type)
                                    (= (true? (:toPerp action)) (true? toPerp))
                                    (same-amount? usdc (:amount action)))))}

      "sendAsset"
      {:request ledger
       :applied? (new-delta? (fn [{:keys [type destination token amount]}]
                               (and (contains? #{"send" "spotTransfer"} type)
                                    (= (normalize (:destination action)) (normalize destination))
                                    (= (token-name (:token action)) (token-name token))
                                    (same-amount? amount (:amount action)))))}

      ("cDeposit" "cWithdraw")
      {:request ledger
       :applied? (new-delta? (fn [{:keys [type isDeposit amount]}]
                               (and (= "cStakingTransfer" type)
                                    (= (= "cDeposit" (:type action)) (true? isDeposit))
                                    (same-amount? amount (wei->hype (:wei action))))))}

      "tokenDelegate"
      {:request {:type "delegatorHistory" :user owner}
       :applied? (new-delta? (fn [{:keys [delegate]}]
                               (and (= (normalize (:validator action))
                                       (normalize (:validator delegate)))
                                    (= (true? (:isUndelegate action)) (true? (:isUndelegate delegate)))
                                    (same-amount? (:amount delegate) (wei->hype (:wei action))))))}

      "approveBuilderFee"
      (let [wanted (some-> (core-writer/percent->decibps (:maxFeeRate action)) str number-of)]
        {:request {:type "maxBuilderFee" :user owner :builder (normalize (:builder action))}
         :applied? (fn [response _]
                     (boolean (and wanted (number-of response) (>= (number-of response) wanted))))})

      "hyperEvmToCore"
      {:request {:type "spotClearinghouseState" :user owner}
       ;; The credit must cover the deposited amount; HyperEVM -> Core
       ;; moves charge no HyperCore fee.
       :applied? (fn [response baseline]
                   (let [amount (or (number-of (:amount action)) js/Infinity)]
                     (>= (- (spot-total response (:tokenIndex action))
                            (spot-total baseline (:tokenIndex action)))
                         (* amount (- 1 1e-6)))))}

      nil)))

(def ^:private baseline-attempts 3)

(defn- read-baseline!
  "The confirmation request's answer before anything is sent. Retried a few
   times; rejects when HyperCore can't be read, so nothing is sent that we
   could not confirm."
  [post-info! request]
  (letfn [(attempt [n]
            (-> (post-info! request)
                (.then (fn [response]
                         (if (some? response)
                           response
                           (throw (js/Error. "empty response")))))
                (.catch (fn [err]
                          (if (< n baseline-attempts)
                            (attempt (inc n))
                            (throw (ex-info "Couldn't read your account from Hyperliquid. Try again in a moment."
                                            {::baseline true :cause err})))))))]
    (attempt 1)))

(defn- sleep!
  [ms]
  (js/Promise. (fn [resolve _] (js/setTimeout resolve ms))))

(defn- poll-until-applied!
  [{:keys [post-info! now-ms-fn sleep-fn poll-ms timeout-ms]} {:keys [request applied?]} baseline]
  (let [deadline (+ (now-ms-fn) timeout-ms)]
    (letfn [(attempt []
              (-> (sleep-fn poll-ms)
                  (.then (fn [_] (-> (post-info! request) (.catch (fn [_] ::unavailable)))))
                  (.then (fn [response]
                           (cond
                             (and (not= ::unavailable response)
                                  (applied? response baseline))
                             true

                             (>= (now-ms-fn) deadline)
                             false

                             :else
                             (attempt))))))]
      (attempt))))

;; --- submit ------------------------------------------------------------------------

(defn- error-message
  [err]
  (str/trim (str (or (some-> err ex-message)
                     (some-> err (aget "message"))
                     err))))

(defn- user-rejected?
  [err]
  (or (= 4001 (some-> err (aget "code")))
      (= 4001 (:code (ex-data err)))
      (re-find #"(?i)user (rejected|denied)" (error-message err))))

(defn- failure
  [message]
  {:status "err" :response message})

(defn submit-action!
  "Send user-signed exchange `action` for smart account `owner` through
   CoreWriter and resolve when HyperCore applied it:

       {:status \"ok\" :tx-hash hash}
       {:status \"err\" :response message}

   Never rejects. `deps`: `:provider-fn`, `:post-info!` (keywordized
   response), `:send-transaction!`, `:ensure-wallet-chain!`, `:now-ms-fn`,
   `:sleep-fn`, `:poll-ms`, `:timeout-ms`, `:on-sent!` `(tx-hash)`."
  [store owner action {:keys [provider-fn post-info! send-transaction! ensure-wallet-chain!
                              now-ms-fn on-sent!]
                       :or {send-transaction! wallet-rpc/send-transaction!
                            ensure-wallet-chain! wallet-rpc/ensure-wallet-chain!
                            now-ms-fn #(js/Date.now)}
                       :as deps}]
  (let [deps* (merge {:sleep-fn sleep! :poll-ms default-poll-ms :timeout-ms default-timeout-ms}
                     deps
                     {:now-ms-fn now-ms-fn})
        owner* (normalize owner)
        state @store
        now (now-ms-fn)]
    (try
      (let [call (core-writer/exchange-action->call
                  action
                  {:owner owner*
                   :spot-meta (get-in state [:spot :meta])
                   :sub-accounts (account-context/owned-subaccount-addresses state)
                   :now-ms now})
            check (confirmation action owner* now)
            provider (provider-fn)]
        (-> (read-baseline! post-info! (:request check))
            (.then (fn [baseline]
                     (-> (ensure-wallet-chain! provider wallet-chain-config)
                         (.then (fn [_]
                                  (send-transaction! provider owner*
                                                     {:to (:to call)
                                                      :data (:data call)
                                                      :value "0x0"
                                                      :chainId (:chain-id chain/mainnet)})))
                         (.then (fn [tx-hash]
                                  ;; Sent: from here on the outcome is applied
                                  ;; or not-yet-applied, never a plain failure.
                                  (try (when (fn? on-sent!) (on-sent! tx-hash)) (catch :default _ nil))
                                  (-> (poll-until-applied! deps* check baseline)
                                      (.then (fn [applied?]
                                               (if applied?
                                                 {:status "ok" :tx-hash tx-hash}
                                                 (failure not-applied-message))))))))))
            (.catch (fn [err]
                      (failure (cond
                                 (::baseline (ex-data err)) (ex-message err)
                                 (user-rejected? err) "You rejected the transaction in your wallet."
                                 :else (str "Your smart account couldn't send the transaction: "
                                            (error-message err))))))))
      (catch :default err
        (js/Promise.resolve
         (failure (if (core-writer/refused? err)
                    (ex-message err)
                    (str "Couldn't build the smart-account transaction: " (error-message err)))))))))

;; --- HyperEVM -> HyperCore from a smart account ------------------------------------

(def queued-step-message
  (str "Part of this transfer is already queued in your smart account. Execute or reject "
       "it there, then reload before trying again."))

(defn- not-applied-result
  [step hashes message]
  {:status "err" :error message :step step :kind :no-hash
   :tx-hash (peek hashes) :hashes hashes})

(defn submit-evm-to-core!
  "Send the HyperEVM transactions of a `hyperEvmToCore` move from smart
   account `owner`, without waiting for receipts between them (a Safe queues
   or batches them, and the hashes it returns may not be on chain yet), then
   wait until HyperCore credits the move. Resolves like
   `hyperevm-submit/submit-hyperevm-to-core!`:

       {:status \"ok\" :txHash :hashes}
       {:status \"err\" :error :step :kind :no-hash :tx-hash :hashes}
                                     ; sent, not applied (yet): may have been sent
       {:status \"err\" :error :step :kind :hashes}

   `deps`: `:provider-fn`, `:post-info!`, `:transfer-plan` `(from action)`,
   `:read-allowance!` `(token owner spender)`, `:allowance-covers?`, plus
   the optional ones of `submit-action!`."
  [owner action {:keys [on-step!]}
   {:keys [provider-fn post-info! transfer-plan read-allowance! allowance-covers?
           send-transaction! ensure-wallet-chain! now-ms-fn]
    :or {send-transaction! wallet-rpc/send-transaction!
         ensure-wallet-chain! wallet-rpc/ensure-wallet-chain!
         now-ms-fn #(js/Date.now)}
    :as deps}]
  (let [deps* (merge {:sleep-fn sleep! :poll-ms default-poll-ms :timeout-ms default-timeout-ms}
                     deps
                     {:now-ms-fn now-ms-fn})
        owner* (normalize owner)
        report! (fn [step event]
                  (try (when (fn? on-step!) (on-step! step event)) (catch :default _ nil)))
        hashes (atom [])
        step* (atom :switch-network)
        check (confirmation (assoc action :type "hyperEvmToCore") owner* (now-ms-fn))
        plan (try (transfer-plan owner* action) (catch :default _ nil))]
    (if-not (and (seq plan) (every? :tx plan))
      (js/Promise.resolve {:status "err" :error "This transfer can't be encoded."
                           :step nil :kind :invalid :hashes []})
      (-> (js/Promise.all
           #js [(read-baseline! post-info! (:request check))
                (if (some :optional? plan)
                  (-> (read-allowance! (:tokenAddress action) owner* (:spender action))
                      (.then (fn [allowance] (allowance-covers? allowance (:units action))))
                      (.catch (fn [_] false)))
                  (js/Promise.resolve false))])
          (.then (fn [[baseline approved?]]
                   (when approved? (report! :approve :skipped))
                   (let [provider (provider-fn)
                         items (if approved? (remove :optional? plan) plan)]
                     (-> (ensure-wallet-chain! provider wallet-chain-config)
                         (.then (fn [_]
                                  (report! :switch-network :done)
                                  (reduce (fn [chain {:keys [step tx]}]
                                            (.then chain
                                                   (fn [_]
                                                     (reset! step* step)
                                                     (report! step :active)
                                                     (-> (send-transaction!
                                                          provider owner*
                                                          (select-keys tx [:to :data :value :chainId]))
                                                         (.then (fn [hash]
                                                                  (swap! hashes conj hash)
                                                                  (report! step :confirming)
                                                                  hash))))))
                                          (js/Promise.resolve nil)
                                          items)))
                         (.then (fn [last-hash]
                                  (-> (poll-until-applied! deps* check baseline)
                                      (.then (fn [applied?]
                                               (if applied?
                                                 (do (doseq [{:keys [step]} items] (report! step :done))
                                                     {:status "ok" :txHash last-hash :hashes @hashes})
                                                 ;; The hash may never reach the chain (a
                                                 ;; queued Safe transaction), so the
                                                 ;; receipt-based resolver can't settle it.
                                                 ;; `:no-hash` marks the run "may have been
                                                 ;; sent" and blocks another move until a
                                                 ;; reload, so it can't be sent twice.
                                                 (not-applied-result @step* @hashes
                                                                     not-applied-message)))))))))))
          (.catch (fn [err]
                    (if (seq @hashes)
                      ;; An earlier transaction (e.g. the approve) is already
                      ;; queued in the account: never offer a plain retry.
                      (not-applied-result @step* @hashes queued-step-message)
                      (let [rejected? (user-rejected? err)]
                        (report! @step* :failed)
                        {:status "err"
                         :error (cond
                                  (::baseline (ex-data err)) (ex-message err)
                                  rejected? "You rejected the transaction in your wallet."
                                  :else (str "Your smart account couldn't send the transaction: "
                                             (error-message err)))
                         :step @step*
                         :kind (if rejected? :rejected :wallet)
                         :hashes @hashes}))))))))
