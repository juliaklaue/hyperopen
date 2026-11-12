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

(defn- confirmation
  "How to tell from the info API that HyperCore applied `action` for
   `owner`: a request and a predicate over `(response baseline)`."
  [action owner now-ms]
  (case (:type action)
    "approveAgent"
    (let [agent (normalize (:agentAddress action))]
      {:request {:type "userRole" :user agent}
       :applied? (fn [response _]
                   (and (= "agent" (:role response))
                        (= owner (normalize (get-in response [:data :user])))))})

    "hyperEvmToCore"
    {:request {:type "spotClearinghouseState" :user owner}
     :applied? (fn [response baseline] (not= response baseline))}

    "tokenDelegate"
    {:request {:type "delegatorHistory" :user owner}
     :applied? (fn [response baseline] (not= response baseline))}

    "approveBuilderFee"
    {:request {:type "maxBuilderFee" :user owner :builder (normalize (:builder action))}
     :applied? (fn [response baseline] (not= response baseline))}

    {:request {:type "userNonFundingLedgerUpdates"
               :user owner
               :startTime (- now-ms (* 24 60 60 1000))}
     :applied? (fn [response baseline] (not= response baseline))}))

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
        (-> (post-info! (:request check))
            (.catch (fn [_] nil))
            (.then (fn [baseline]
                     (-> (ensure-wallet-chain! provider wallet-chain-config)
                         (.then (fn [_]
                                  (send-transaction! provider owner*
                                                     {:to (:to call)
                                                      :data (:data call)
                                                      :value "0x0"
                                                      :chainId (:chain-id chain/mainnet)})))
                         (.then (fn [tx-hash]
                                  (when (fn? on-sent!) (on-sent! tx-hash))
                                  (-> (poll-until-applied! deps* check baseline)
                                      (.then (fn [applied?]
                                               (if applied?
                                                 {:status "ok" :tx-hash tx-hash}
                                                 (failure not-applied-message)))))))
                         (.catch (fn [err]
                                   (failure (if (user-rejected? err)
                                              "You rejected the transaction in your wallet."
                                              (str "Your smart account couldn't send the transaction: "
                                                   (error-message err)))))))))))
      (catch :default err
        (js/Promise.resolve
         (failure (if (core-writer/refused? err)
                    (ex-message err)
                    (str "Couldn't build the smart-account transaction: " (error-message err)))))))))

;; --- HyperEVM -> HyperCore from a smart account ------------------------------------

(defn submit-evm-to-core!
  "Send the HyperEVM transactions of a `hyperEvmToCore` move from smart
   account `owner`, without waiting for receipts between them (a Safe queues
   or batches them, and the hashes it returns may not be on chain yet), then
   wait until HyperCore credits the move. Resolves like
   `hyperevm-submit/submit-hyperevm-to-core!`:

       {:status \"ok\" :txHash :hashes}
       {:status \"pending\" :txHash :step :hashes}   ; sent, not applied yet
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
        report! (fn [step event] (when (fn? on-step!) (on-step! step event)))
        hashes (atom [])
        step* (atom :switch-network)
        check (confirmation {:type "hyperEvmToCore"} owner* (now-ms-fn))
        plan (try (transfer-plan owner* action) (catch :default _ nil))]
    (if-not (and (seq plan) (every? :tx plan))
      (js/Promise.resolve {:status "err" :error "This transfer can't be encoded."
                           :step nil :kind :invalid :hashes []})
      (-> (js/Promise.all
           #js [(-> (post-info! (:request check)) (.catch (fn [_] nil)))
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
                                                 {:status "pending" :txHash last-hash
                                                  :step @step* :hashes @hashes}))))))))))
          (.catch (fn [err]
                    (let [rejected? (user-rejected? err)]
                      (report! @step* :failed)
                      {:status "err"
                       :error (if rejected?
                                "You rejected the transaction in your wallet."
                                (str "Your smart account couldn't send the transaction: "
                                     (error-message err)))
                       :step @step*
                       :kind (if rejected? :rejected :wallet)
                       :hashes @hashes})))))))
