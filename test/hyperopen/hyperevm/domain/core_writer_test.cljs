(ns hyperopen.hyperevm.domain.core-writer-test
  "Golden vectors were produced independently with viem 2.57.7
   (`encodeFunctionData(sendRawAction, [concat([0x01, actionId(3 bytes),
   encodeAbiParameters(fields)])])`), the same encoding Zodiac Pilot's
   `@zodiac/clear-signing` CoreWriter helpers use, on 2026-11-12."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [hyperopen.hyperevm.domain.core-writer :as cw]))

(def ^:private add-api-wallet-vector
  "0x17938e13000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000a40100000900000000000000000000000011111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000002768797065726f70656e206162632076616c69645f756e74696c20313830303030303030303030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")

(def ^:private usd-class-transfer-vector
  "0x17938e130000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000004401000007000000000000000000000000000000000000000000000000000000000016e360000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000")

(def ^:private send-asset-vector
  "0x17938e13000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000c40100000d0000000000000000000000002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff00000000000000000000000000000000000000000000000000000000ffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001312d0000000000000000000000000000000000000000000000000000000000")

(def ^:private token-delegate-vector
  "0x17938e13000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000640100000300000000000000000000000033333333333333333333333333333333333333340000000000000000000000000000000000000000000000000000000005f5e100000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000")

(def ^:private staking-deposit-vector
  "0x17938e130000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000002401000004000000000000000000000000000000000000000000000000000000000ee6b28000000000000000000000000000000000000000000000000000000000")

(def ^:private staking-withdraw-vector
  "0x17938e130000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000002401000005000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000")

(def ^:private approve-builder-fee-vector
  "0x17938e13000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000440100000c000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000444444444444444444444444444444444444444400000000000000000000000000000000000000000000000000000000")

(def ^:private owner "0xd6500fea477a494c60972ad3fc820bcd0f9a2efd")
(def ^:private sub-account "0x5555555555555555555555555555555555555555")

(def ^:private spot-meta
  {:tokens [{:name "USDC" :index 0 :tokenId "0x6d1e7cde53ba9467b783cb7c530ce054" :weiDecimals 8}
            {:name "HYPE" :index 150 :tokenId "0x0d01dc56dcaaca66ad901c959b4011ec" :weiDecimals 8}]})

(def ^:private ctx
  {:owner owner
   :spot-meta spot-meta
   :sub-accounts [sub-account]
   :now-ms 1000})

(deftest encoders-match-viem-vectors-test
  (is (= add-api-wallet-vector
         (cw/add-api-wallet-data "0x1111111111111111111111111111111111111111"
                                 "hyperopen abc valid_until 1800000000000")))
  (is (= usd-class-transfer-vector (cw/usd-class-transfer-data 1500000 true)))
  (is (= send-asset-vector
         (cw/send-asset-data {:destination "0x2000000000000000000000000000000000000000"
                              :sub-account "0x0000000000000000000000000000000000000000"
                              :source-dex cw/spot-dex
                              :destination-dex cw/spot-dex
                              :token 0
                              :wei 20000000})))
  (is (= token-delegate-vector
         (cw/token-delegate-data "0x3333333333333333333333333333333333333334" 100000000 true)))
  (is (= staking-deposit-vector (cw/staking-deposit-data 250000000)))
  (is (= staking-withdraw-vector (cw/staking-withdraw-data 1)))
  (is (= approve-builder-fee-vector
         (cw/approve-builder-fee-data 10 "0x4444444444444444444444444444444444444444"))))

(deftest agent-name-gets-a-179-day-expiry-test
  (is (= "abc valid_until 15465601000" (cw/agent-name-with-expiry "abc" 1000)))
  (is (= "abc valid_until 5" (cw/agent-name-with-expiry "abc valid_until 5" 1000))
      "an existing expiry is kept")
  (is (= "" (cw/agent-name-with-expiry nil 1000)) "the unnamed wallet has no expiry"))

(deftest approve-agent-becomes-add-api-wallet-test
  (let [call (cw/exchange-action->call {:type "approveAgent"
                                        :agentAddress "0x1111111111111111111111111111111111111111"
                                        :agentName "hyperopen abc"}
                                       (assoc ctx :now-ms (- 1800000000000 cw/agent-validity-ms)))]
    (is (= cw/core-writer-address (:to call)))
    (is (= 9 (:action-id call)))
    (is (= add-api-wallet-vector (:data call)))))

(deftest usd-class-transfer-uses-six-decimals-test
  (is (= usd-class-transfer-vector
         (:data (cw/exchange-action->call {:type "usdClassTransfer" :amount "1.5" :toPerp true} ctx)))))

(deftest send-asset-to-evm-uses-token-wei-decimals-test
  (is (= send-asset-vector
         (:data (cw/exchange-action->call
                 {:type "sendAsset"
                  :destination "0x2000000000000000000000000000000000000000"
                  :sourceDex "spot"
                  :destinationDex "spot"
                  :token "USDC:0x6d1e7cde53ba9467b783cb7c530ce054"
                  :amount "0.2"
                  :fromSubAccount ""}
                 ctx)))))

(deftest send-asset-destinations-are-restricted-to-the-account-test
  (let [send (fn [destination & [overrides]]
               (cw/exchange-action->call (merge {:type "sendAsset"
                                                 :destination destination
                                                 :sourceDex ""
                                                 :destinationDex "spot"
                                                 :token "USDC:0x6d1e7cde53ba9467b783cb7c530ce054"
                                                 :amount "1"
                                                 :fromSubAccount ""}
                                                overrides)
                                         ctx))]
    (testing "allowed"
      (is (= 13 (:action-id (send owner))) "between its own balances")
      (is (= 13 (:action-id (send sub-account))) "to its own sub-account")
      (is (= 13 (:action-id (send "0x2222222222222222222222222222222222222222"
                                  {:token "HYPE" :sourceDex "spot"})))
          "HYPE back to HyperEVM"))
    (testing "refused"
      (doseq [destination ["0x9999999999999999999999999999999999999999"
                           ;; another token's system address is not this one's
                           "0x2000000000000000000000000000000000000001"
                           nil]]
        (let [err (try (send destination) nil (catch :default e e))]
          (is (cw/refused? err) (str "refuses " destination))))
      (is (cw/refused? (try (send owner {:fromSubAccount "0x9999999999999999999999999999999999999999"})
                            nil
                            (catch :default e e)))
          "unknown sub-account source")
      (is (cw/refused? (try (send owner {:sourceDex "xyz"}) nil (catch :default e e)))
          "named perp dexes are not mapped yet"))))

(deftest staking-and-delegation-pass-wei-through-test
  (is (= staking-deposit-vector
         (:data (cw/exchange-action->call {:type "cDeposit" :wei 250000000} ctx))))
  (is (= staking-withdraw-vector
         (:data (cw/exchange-action->call {:type "cWithdraw" :wei 1} ctx))))
  (is (= token-delegate-vector
         (:data (cw/exchange-action->call {:type "tokenDelegate"
                                           :validator "0x3333333333333333333333333333333333333334"
                                           :wei 100000000
                                           :isUndelegate true}
                                          ctx)))))

(deftest builder-fee-percent-becomes-decibps-test
  (is (= approve-builder-fee-vector
         (:data (cw/exchange-action->call {:type "approveBuilderFee"
                                           :maxFeeRate "0.01%"
                                           :builder "0x4444444444444444444444444444444444444444"}
                                          ctx)))))

(deftest actions-without-a-core-writer-route-are-refused-test
  (doseq [action [{:type "withdraw3" :destination owner :amount "1"}
                  {:type "usdSend" :destination owner :amount "1"}
                  {:type "spotSend" :destination owner :amount "1"}]]
    (is (cw/refused? (try (cw/exchange-action->call action ctx) nil (catch :default e e)))
        (:type action))))
