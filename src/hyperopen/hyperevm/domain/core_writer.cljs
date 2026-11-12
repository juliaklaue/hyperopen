(ns hyperopen.hyperevm.domain.core-writer
  "HyperCore actions sent from HyperEVM through the CoreWriter system
   contract, for accounts that cannot produce the EIP-712 signature a
   user-signed exchange action needs (smart-contract accounts such as a
   Safe).

   `sendRawAction(bytes)` carries a version byte (1), a 3-byte big-endian
   action id and the ABI-encoded fields. HyperCore acts on the call with the
   SENDING contract as the account, so a CoreWriter `sendAsset` to a token's
   system address always credits that same contract on HyperEVM.

   `exchange-action->call` translates the user-signed exchange actions the
   app builds into CoreWriter calls and refuses anything that would move
   funds to a different account: a `sendAsset` may only go to the token's
   system address (back to the account on HyperEVM), the account itself, or
   one of its own sub-accounts."
  (:require [clojure.string :as str]
            [hyperopen.hyperevm.domain.abi :as abi]
            [hyperopen.hyperevm.domain.chain :as chain]
            [hyperopen.hyperevm.domain.tokens :as tokens]
            [hyperopen.hyperevm.domain.units :as units]))

(def core-writer-address
  "0x3333333333333333333333333333333333333333")

(def send-raw-action-selector
  "`sendRawAction(bytes)`."
  "0x17938e13")

(def action-ids
  {:token-delegate 3
   :staking-deposit 4
   :staking-withdraw 5
   :usd-class-transfer 7
   :add-api-wallet 9
   :approve-builder-fee 12
   :send-asset 13})

(def spot-dex
  "CoreWriter's dex index for Spot (uint32 max). The default perp dex is 0."
  4294967295)

(def agent-validity-ms
  "Hyperliquid keeps a named API wallet for at most 180 days; register for
   179 so clock skew never makes the request invalid."
  (* 179 24 60 60 1000))

(def ^:private word-hex-length 64)

(def ^:private uint32-limit (js/BigInt "4294967296"))
(def ^:private uint64-limit (js/BigInt "18446744073709551616"))

;; --- ABI ----------------------------------------------------------------------

(defn- pad-left
  [hex]
  (str (apply str (repeat (- word-hex-length (count hex)) "0")) hex))

(defn- pad-right
  [hex]
  (let [remainder (js-mod (count hex) word-hex-length)]
    (if (zero? remainder)
      hex
      (str hex (apply str (repeat (- word-hex-length remainder) "0"))))))

(defn- refuse
  [message & [data]]
  (throw (ex-info message (merge {::refused true} data))))

(defn- uint-word
  [value limit field]
  (let [value* (units/to-bigint value)]
    (when-not (and value* (< value* limit))
      (refuse (str "Invalid " field " for a CoreWriter action.")))
    (pad-left (.toString value* 16))))

(defn- address-word
  [value field]
  (if-let [address (abi/normalize-address value)]
    (pad-left (subs address 2))
    (refuse (str "Invalid " field " address for a CoreWriter action."))))

(defn- bool-word
  [value]
  (pad-left (if (true? value) "1" "0")))

(defn- utf8-hex
  [text]
  (->> (.encode (js/TextEncoder.) text)
       (array-seq)
       (map #(.padStart (.toString % 16) 2 "0"))
       (apply str)))

(defn- dynamic-tail
  "Length word plus right-padded content for `bytes`/`string` hex."
  [hex]
  (str (pad-left (.toString (quot (count hex) 2) 16)) (pad-right hex)))

(defn- action-header
  [action-id]
  (str "01" (.padStart (.toString action-id 16) 6 "0")))

(defn send-raw-action-data
  "Calldata for `sendRawAction(header ++ fields)`; `fields-hex` is the ABI
   encoding of the action's fields without `0x`."
  [action-id fields-hex]
  (str send-raw-action-selector
       (pad-left "20")
       (dynamic-tail (str (action-header action-id) fields-hex))))

;; --- action encoders ------------------------------------------------------------

(defn add-api-wallet-data
  [agent-address name]
  (let [name-hex (utf8-hex (or name ""))]
    (send-raw-action-data (:add-api-wallet action-ids)
                          (str (address-word agent-address "agent")
                               (pad-left "40")
                               (dynamic-tail name-hex)))))

(defn usd-class-transfer-data
  [ntl to-perp?]
  (send-raw-action-data (:usd-class-transfer action-ids)
                        (str (uint-word ntl uint64-limit "amount")
                             (bool-word to-perp?))))

(defn send-asset-data
  [{:keys [destination sub-account source-dex destination-dex token wei]}]
  (send-raw-action-data (:send-asset action-ids)
                        (str (address-word destination "destination")
                             (address-word sub-account "sub-account")
                             (uint-word source-dex uint32-limit "source dex")
                             (uint-word destination-dex uint32-limit "destination dex")
                             (uint-word token uint64-limit "token")
                             (uint-word wei uint64-limit "amount"))))

(defn token-delegate-data
  [validator wei undelegate?]
  (send-raw-action-data (:token-delegate action-ids)
                        (str (address-word validator "validator")
                             (uint-word wei uint64-limit "amount")
                             (bool-word undelegate?))))

(defn staking-deposit-data
  [wei]
  (send-raw-action-data (:staking-deposit action-ids)
                        (uint-word wei uint64-limit "amount")))

(defn staking-withdraw-data
  [wei]
  (send-raw-action-data (:staking-withdraw action-ids)
                        (uint-word wei uint64-limit "amount")))

(defn approve-builder-fee-data
  [max-fee-rate builder]
  (send-raw-action-data (:approve-builder-fee action-ids)
                        (str (uint-word max-fee-rate uint64-limit "max fee rate")
                             (address-word builder "builder"))))

;; --- exchange action -> CoreWriter call ----------------------------------------

(def ^:private zero-address
  "0x0000000000000000000000000000000000000000")

(def ^:private valid-until-pattern
  #"(?:^|\s)valid_until\s+\d+$")

(defn agent-name-with-expiry
  "`name` with a ` valid_until <now + 179 days>` suffix, unless it already
   carries one. The unnamed API wallet (nil/blank name) cannot expire."
  [name now-ms]
  (let [name* (some-> name str str/trim not-empty)]
    (cond
      (nil? name*) ""
      (re-find valid-until-pattern name*) name*
      :else (str name* " valid_until " (+ now-ms agent-validity-ms)))))

(defn- dex-index
  [dex field]
  (case (some-> dex str str/trim str/lower-case)
    "" 0
    nil 0
    "spot" spot-dex
    (refuse (str "Transfers on the \"" dex "\" dex aren't available from a "
                 "smart-contract account yet.")
            {:field field})))

(defn- field
  "Read `k` from a map whose keys may be keywords or strings."
  [m k]
  (let [value (get m k)]
    (if (some? value) value (get m (name k)))))

(defn- spot-token
  "The spot token named by a `sendAsset` wire id (`NAME:0xtokenId`), from
   spot metadata `:tokens`."
  [spot-meta wire-id]
  (let [[token-name token-id] (str/split (str wire-id) #":" 2)
        name* (some-> token-name str/trim str/upper-case)
        id* (some-> token-id str/trim str/lower-case)]
    (some (fn [token]
            (when (and (= name* (some-> (field token :name) str str/upper-case))
                       (or (nil? id*)
                           (= id* (some-> (field token :tokenId) str str/lower-case))))
              {:index (field token :index)
               :name (field token :name)
               :wei-decimals (field token :weiDecimals)}))
          (field spot-meta :tokens))))

(defn system-address-for
  "The HyperCore -> HyperEVM bridge address of spot token `token`."
  [{:keys [index name]}]
  (if (= "HYPE" (some-> name str/upper-case))
    (:hype-system-address chain/mainnet)
    (tokens/system-address index)))

(defn- send-asset-call
  [{:keys [destination sourceDex destinationDex token amount fromSubAccount]}
   {:keys [owner spot-meta sub-accounts]}]
  (let [token* (or (spot-token spot-meta token)
                   (refuse (str "Unknown token " token ".")))
        wei (units/parse-units (str amount) (:wei-decimals token*))
        destination* (abi/normalize-address destination)
        owner* (abi/normalize-address owner)
        own-sub-accounts (set (keep abi/normalize-address sub-accounts))
        to-evm? (= destination* (system-address-for token*))
        from-sub-account (or (abi/normalize-address fromSubAccount) zero-address)]
    (when-not (and wei (pos? wei))
      (refuse "Enter an amount greater than zero."))
    (when-not (or to-evm?
                  (and (some? destination*) (= destination* owner*))
                  (contains? own-sub-accounts destination*))
      (refuse (str "A smart-contract account can only move funds back to "
                   "itself: to HyperEVM, between its own balances, or to its "
                   "own sub-accounts.")
              {:destination destination}))
    (when-not (or (= from-sub-account zero-address)
                  (contains? own-sub-accounts from-sub-account))
      (refuse "Unknown sub-account."))
    {:action-id (:send-asset action-ids)
     :data (send-asset-data {:destination destination*
                             :sub-account from-sub-account
                             :source-dex (dex-index sourceDex :source-dex)
                             :destination-dex (dex-index destinationDex :destination-dex)
                             :token (:index token*)
                             :wei wei})}))

(defn percent->decibps
  "`\"0.01%\"` -> 10 (tenths of a basis point)."
  [rate]
  (units/parse-units (str/replace (str rate) #"%\s*$" "") 3))

(defn exchange-action->call
  "The CoreWriter call `{:action-id :data :to}` for user-signed exchange
   `action`, or throws `ex-info` (`::refused`) with a message for the user.

   `ctx`: `:owner` (the smart account), `:spot-meta` (`[:spot :meta]`),
   `:sub-accounts` (the owner's sub-account addresses), `:now-ms`."
  [action {:keys [now-ms] :as ctx}]
  (let [call (case (:type action)
               "approveAgent"
               {:action-id (:add-api-wallet action-ids)
                :data (add-api-wallet-data (:agentAddress action)
                                           (agent-name-with-expiry (:agentName action) now-ms))}

               "usdClassTransfer"
               (let [ntl (units/parse-units (str (:amount action)) 6)]
                 (when-not (and ntl (pos? ntl))
                   (refuse "Enter an amount greater than zero."))
                 {:action-id (:usd-class-transfer action-ids)
                  :data (usd-class-transfer-data ntl (true? (:toPerp action)))})

               "sendAsset"
               (send-asset-call action ctx)

               "cDeposit"
               {:action-id (:staking-deposit action-ids)
                :data (staking-deposit-data (:wei action))}

               "cWithdraw"
               {:action-id (:staking-withdraw action-ids)
                :data (staking-withdraw-data (:wei action))}

               "tokenDelegate"
               {:action-id (:token-delegate action-ids)
                :data (token-delegate-data (:validator action)
                                           (:wei action)
                                           (true? (:isUndelegate action)))}

               "approveBuilderFee"
               {:action-id (:approve-builder-fee action-ids)
                :data (approve-builder-fee-data (percent->decibps (:maxFeeRate action))
                                                (:builder action))}

               (refuse (str "\"" (:type action) "\" isn't available from a "
                            "smart-contract account. Withdraw to HyperEVM "
                            "instead: it goes back to the account itself.")))]
    (assoc call :to core-writer-address)))

(defn refused?
  [err]
  (true? (::refused (ex-data err))))
