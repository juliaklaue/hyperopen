(ns hyperopen.wallet.smart-account-test
  (:require [cljs.test :refer-macros [async deftest is testing]]
            [hyperopen.hyperevm.domain.core-writer :as core-writer]
            [hyperopen.wallet.smart-account :as smart-account]))

(def ^:private safe "0xd6500fea477a494c60972ad3fc820bcd0f9a2efd")
(def ^:private agent "0x1111111111111111111111111111111111111111")

(def ^:private spot-meta
  {:tokens [{:name "USDC" :index 0 :tokenId "0x6d1e7cde53ba9467b783cb7c530ce054" :weiDecimals 8}]})

(defn- store-for
  [address]
  (atom {:wallet {:address address :connected? true}
         :spot {:meta spot-meta}}))

(deftest contract-code-test
  (is (true? (smart-account/contract-code? "0x608060405234801561001057600080fd5b50")))
  (is (false? (smart-account/contract-code? "0x")))
  (is (false? (smart-account/contract-code? nil)))
  (is (false? (smart-account/contract-code? "0x0")))
  (is (false? (smart-account/contract-code?
               "0xef01001234567890123456789012345678901234567890"))
      "an EIP-7702 delegated EOA can still sign"))

(deftest detect-caches-per-connected-address-test
  (async done
    (let [store (store-for safe)
          calls (atom 0)
          get-code (fn [_] (swap! calls inc) (js/Promise.resolve "0x6080"))]
      (-> (smart-account/detect! store "0xD6500FeA477A494C60972Ad3fc820BCd0f9a2eFd"
                                 {:get-code-fn get-code})
          (.then (fn [smart?]
                   (is (true? smart?))
                   (is (smart-account/smart-account? @store))
                   (smart-account/detect! store safe {:get-code-fn get-code})))
          (.then (fn [smart?]
                   (is (true? smart?))
                   (is (= 1 @calls) "code is read once per address")
                   (smart-account/detect! store agent {:get-code-fn get-code})))
          (.then (fn [smart?]
                   (is (false? smart?) "an address other than the connected wallet is never sent for")
                   (is (= 1 @calls))))
          (.catch (fn [err] (is false (str err))))
          (.finally done)))))

(deftest detect-treats-a-failed-read-as-eoa-without-caching-test
  (async done
    (let [store (store-for safe)]
      (-> (smart-account/detect! store safe {:get-code-fn (fn [_] (js/Promise.reject (js/Error. "rpc down")))})
          (.then (fn [smart?]
                   (is (false? smart?))
                   (is (nil? (get-in @store [:wallet :smart-account])))))
          (.finally done)))))

(defn- fake-deps
  "Wallet and info fakes. `responses` is a vector of info responses served in
   order (the first is the baseline); the last one repeats."
  [{:keys [responses send-error]}]
  (let [sent (atom [])
        infos (atom [])
        queue (atom responses)]
    {:sent sent
     :infos infos
     :deps {:provider-fn (constantly #js {})
            :ensure-wallet-chain! (fn [_ config]
                                    (js/Promise.resolve (:chain-id config)))
            :send-transaction! (fn [_ from tx]
                                 (swap! sent conj [from tx])
                                 (if send-error
                                   (js/Promise.reject send-error)
                                   (js/Promise.resolve "0xhash")))
            :post-info! (fn [request]
                          (swap! infos conj request)
                          (let [[head & more] @queue]
                            (when (seq more) (reset! queue (vec more)))
                            (js/Promise.resolve head)))
            :sleep-fn (fn [_] (js/Promise.resolve nil))
            :poll-ms 0
            :timeout-ms 1000
            :now-ms-fn (constantly 5000)}}))

(deftest approve-agent-is-sent-through-core-writer-and-confirmed-test
  (async done
    (let [{:keys [sent infos deps]} (fake-deps {:responses [{:role "missing"}
                                                            {:role "missing"}
                                                            {:role "agent" :data {:user safe}}]})]
      (-> (smart-account/submit-action! (store-for safe) safe
                                        {:type "approveAgent" :agentAddress agent :agentName "dev"}
                                        deps)
          (.then (fn [result]
                   (is (= {:status "ok" :tx-hash "0xhash"} result))
                   (let [[from tx] (first @sent)]
                     (is (= safe from))
                     (is (= core-writer/core-writer-address (:to tx)))
                     (is (= "0x3e7" (:chainId tx)))
                     (is (= (core-writer/add-api-wallet-data
                             agent (core-writer/agent-name-with-expiry "dev" 5000))
                            (:data tx))))
                   (is (= {:type "userRole" :user agent} (first @infos)))
                   (is (= 3 (count @infos)) "baseline, then polled until applied")))
          (.finally done)))))

(deftest transfers-are-confirmed-by-a-new-ledger-entry-test
  (async done
    (let [unrelated {:hash "0xb" :delta {:type "send" :destination safe :token "USDC" :amount "5.0"}}
          wrong-direction {:hash "0xc" :delta {:type "accountClassTransfer" :usdc "0.3" :toPerp true}}
          ;; Perps -> Spot: the ledger signs the delta negative.
          ours {:hash "0xa" :delta {:type "accountClassTransfer" :usdc "-0.3" :toPerp false}}
          {:keys [infos deps]} (fake-deps {:responses [[]
                                                       [unrelated]
                                                       [unrelated wrong-direction]
                                                       [unrelated wrong-direction ours]]})]
      (-> (smart-account/submit-action! (store-for safe) safe
                                        {:type "usdClassTransfer" :amount "0.3" :toPerp false}
                                        deps)
          (.then (fn [result]
                   (is (= "ok" (:status result)))
                   (is (= "userNonFundingLedgerUpdates" (:type (first @infos))))
                   (is (= 4 (count @infos)) "unrelated ledger changes don't count")))
          (.finally done)))))

(deftest withdraw-to-evm-is-confirmed-by-a-matching-send-test
  (async done
    (let [system "0x2000000000000000000000000000000000000000"
          {:keys [deps]} (fake-deps {:responses [[]
                                                 [{:hash "0xa" :delta {:type "send" :destination system
                                                                       :token "USDC" :amount "0.2"}}]]})]
      (-> (smart-account/submit-action! (store-for safe) safe
                                        {:type "sendAsset" :destination system :sourceDex "spot"
                                         :destinationDex "spot" :amount "0.2" :fromSubAccount ""
                                         :token "USDC:0x6d1e7cde53ba9467b783cb7c530ce054"}
                                        deps)
          (.then (fn [result] (is (= "ok" (:status result)))))
          (.finally done)))))

(deftest nothing-is-sent-without-a-baseline-test
  (async done
    (let [{:keys [sent deps]} (fake-deps {:responses [[]]})
          attempt (fn [post-info!]
                    (smart-account/submit-action! (store-for safe) safe
                                                  {:type "usdClassTransfer" :amount "0.3" :toPerp false}
                                                  (assoc deps :post-info! post-info!)))]
      (-> (attempt (fn [_] (js/Promise.reject (js/Error. "down"))))
          (.then (fn [result]
                   (is (= "err" (:status result)))
                   (is (re-find #"Couldn't read your account" (:response result)))
                   (attempt (fn [_] (js/Promise.resolve {:status "err" :response "rate limited"})))))
          (.then (fn [result]
                   (is (re-find #"Couldn't read your account" (:response result))
                       "an HTTP error body is not a baseline")
                   (is (empty? @sent))))
          (.finally done)))))

(deftest not-applied-in-time-is-reported-without-claiming-failure-test
  (async done
    (let [now (atom 0)
          {:keys [deps]} (fake-deps {:responses [[]]})]
      (-> (smart-account/submit-action! (store-for safe) safe
                                        {:type "usdClassTransfer" :amount "0.3" :toPerp false}
                                        (assoc deps :now-ms-fn #(swap! now + 400)))
          (.then (fn [result]
                   (is (= {:status "err" :response smart-account/not-applied-message} result))))
          (.finally done)))))

(deftest refused-actions-never-reach-the-wallet-test
  (async done
    (let [{:keys [sent deps]} (fake-deps {:responses [[]]})]
      (-> (smart-account/submit-action! (store-for safe) safe
                                        {:type "withdraw3" :destination safe :amount "1"}
                                        deps)
          (.then (fn [result]
                   (is (= "err" (:status result)))
                   (is (re-find #"smart-contract account" (:response result)))
                   (is (empty? @sent))))
          (.finally done)))))

(deftest wallet-rejection-is-reported-test
  (async done
    (let [{:keys [deps]} (fake-deps {:responses [[]]
                                     :send-error (doto (js/Error. "User rejected the request.")
                                                   (aset "code" 4001))})]
      (-> (smart-account/submit-action! (store-for safe) safe
                                        {:type "usdClassTransfer" :amount "0.3" :toPerp true}
                                        deps)
          (.then (fn [result]
                   (testing "a rejected send is not reported as pending"
                     (is (= {:status "err" :response "You rejected the transaction in your wallet."}
                            result)))))
          (.finally done)))))

(deftest evm-to-core-sends-every-step-without-receipts-then-confirms-test
  (async done
    (let [{:keys [sent deps]} (fake-deps {:responses [{:balances []} {:balances []}
                                                      {:balances [{:coin "USDC" :token 0 :total "0.2"}]}]})
          steps (atom [])
          plan [{:step :approve :optional? true :tx {:to "0xusdc" :data "0xapprove" :chainId "0x3e7" :gas "0x1"}}
                {:step :deposit :tx {:to "0xcdw" :data "0xdeposit" :chainId "0x3e7"}}]]
      (-> (smart-account/submit-evm-to-core!
           safe
           {:type "hyperEvmToCore" :kind "usdcCoreDeposit" :tokenAddress "0xusdc" :spender "0xcdw"
            :units "200000" :amount "0.2" :tokenIndex 0}
           {:on-step! (fn [step event] (swap! steps conj [step event]))}
           (assoc deps
                  :transfer-plan (fn [_ _] plan)
                  :read-allowance! (fn [_ _ _] (js/Promise.resolve "0"))
                  :allowance-covers? (fn [_ _] false)))
          (.then (fn [result]
                   (is (= {:status "ok" :txHash "0xhash" :hashes ["0xhash" "0xhash"]} result))
                   (is (= [{:to "0xusdc" :data "0xapprove" :chainId "0x3e7"}
                           {:to "0xcdw" :data "0xdeposit" :chainId "0x3e7"}]
                          (mapv #(nth % 1) @sent))
                       "no gas fields: the account's owners execute")
                   (is (= [[:switch-network :done] [:approve :active] [:approve :confirming]
                           [:deposit :active] [:deposit :confirming] [:approve :done] [:deposit :done]]
                          @steps))))
          (.finally done)))))

(deftest evm-to-core-skips-a-covered-approve-and-reports-pending-test
  (async done
    (let [now (atom 0)
          {:keys [sent deps]} (fake-deps {:responses [{:balances []}]})
          plan [{:step :approve :optional? true :tx {:to "0xusdc" :data "0xapprove"}}
                {:step :deposit :tx {:to "0xcdw" :data "0xdeposit"}}]]
      (-> (smart-account/submit-evm-to-core!
           safe {:kind "usdcCoreDeposit"} {}
           (assoc deps
                  :now-ms-fn #(swap! now + 400)
                  :transfer-plan (fn [_ _] plan)
                  :read-allowance! (fn [_ _ _] (js/Promise.resolve "999999"))
                  :allowance-covers? (fn [_ _] true)))
          (.then (fn [result]
                   (is (= 1 (count @sent)))
                   (is (= {:status "err" :error smart-account/not-applied-message :step :deposit
                           :kind :no-hash :tx-hash "0xhash" :hashes ["0xhash"]}
                          result)
                       "may have been sent: blocks another move instead of waiting on a receipt")))
          (.finally done)))))

(deftest evm-to-core-stays-pending-when-a-later-step-fails-after-the-approve-test
  (async done
    (let [{:keys [deps]} (fake-deps {:responses [{:balances []}]})
          calls (atom 0)
          plan [{:step :approve :optional? true :tx {:to "0xusdc" :data "0xapprove"}}
                {:step :deposit :tx {:to "0xcdw" :data "0xdeposit"}}]]
      (-> (smart-account/submit-evm-to-core!
           safe {:kind "usdcCoreDeposit" :tokenIndex 0} {}
           (assoc deps
                  :send-transaction! (fn [_ _ _]
                                       (if (= 1 (swap! calls inc))
                                         (js/Promise.resolve "0xapprove-hash")
                                         (js/Promise.reject (doto (js/Error. "User rejected") (aset "code" 4001)))))
                  :transfer-plan (fn [_ _] plan)
                  :read-allowance! (fn [_ _ _] (js/Promise.resolve "0"))
                  :allowance-covers? (fn [_ _] false)))
          (.then (fn [result]
                   (is (= {:status "err" :error smart-account/queued-step-message :step :deposit
                           :kind :no-hash :tx-hash "0xapprove-hash" :hashes ["0xapprove-hash"]}
                          result)
                       "the queued approve is outstanding, so no plain retry is offered")))
          (.finally done)))))
