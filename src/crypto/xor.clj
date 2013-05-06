(ns crypto.xor
  "xor computations"
  (:require [midje.sweet      :as m]
            [crypto.hex       :as hex]
            [crypto.binary    :as binary]
            [crypto.frequency :as frequency]
            [crypto.byte      :as byte]
            [crypto.ascii     :as ascii]))

(defn bitxor
  "Apply bit-xor to the seq using key as the key"
  [seq key]
  (map bit-xor seq key))

(m/fact
  (bitxor [0 0 0 0 1 1 1 1] [0 0 0 0 1 1 1 1])         => [0 0 0 0 0 0 0 0]
  (bitxor [0 0 0 0 1 1 1 1] [1 1 1 1 1 1 1 1])         => [1 1 1 1 0 0 0 0]
  (bitxor [0 0 0 0 1 1 1 1] [1 1 1 1 0 0 0 0])         => [1 1 1 1 1 1 1 1]
  (bitxor [1 1 1 1 1 1 1 1] [1 1 1 1 0 0 0 0])         => [0 0 0 0 1 1 1 1]
  (apply bitxor [[0 0 0 0 1 1 1 1] [1 1 1 1 1 1 1 1]]) => [1 1 1 1 0 0 0 0])

(defn- xor-byte
  "Compute the xor between the input by (byte) and the key key (byte). No check on key."
  [by0 by1]
  (->> [by0 by1]
       (map byte/to-bits)
       (apply bitxor)
       (partition 8)
       (map binary/to-bytes)))

(m/fact :one-way-and-back
  (xor-byte [0 1 2 3 4 5] [0 1 2 3 4 5]) => [0 0 0 0 0 0]
  (xor-byte [0 0 0 0 0 0] [0 1 2 3 4 5]) => [0 1 2 3 4 5])

(defn xor
  "Compute the xor between the input by (byte) and the key key (byte). The key is repeated if need be."
  [by key]
  (->> key
       cycle
       (take (count by))
       (xor-byte by)))

(m/fact
  (xor (hex/to-bytes "abcd") (hex/to-bytes "de"))                                                                   => (hex/to-bytes "7513")
  (xor (hex/to-bytes "1c0111001f010100061a024b53535009181c") (hex/to-bytes "686974207468652062756c6c277320657965")) => (hex/to-bytes "746865206b696420646f6e277420706c6179")
  (xor (hex/to-bytes "746865206b696420646f6e277420706c6179") (hex/to-bytes "686974207468652062756c6c277320657965")) => (hex/to-bytes "1c0111001f010100061a024b53535009181c"))

(defn encrypt
  "Given a map {:key 'ascii key' :msg 'ascii message'}, encode the key message into hexadecimal then encrypt the hex msg with the hex key."
  [{:keys [key msg]}]
  (->> [msg key]
       (map ascii/to-bytes)
       (apply xor)
       hex/encode))

(m/fact
  (encrypt {:key "X"
            :msg "Cooking MC's like a pound of bacon"}) => "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

(defn decrypt
  "Given a map {:key 'ascii key' :msg 'hex encoded message'}, decode the encrypted message."
  [{:keys [msg key]}]
  (->> [(hex/to-bytes msg) (ascii/to-bytes key)]
       (apply xor)
       byte/to-ascii))

(m/fact
  (decrypt {:key "X"
            :msg (encrypt {:key "X"
                           :msg "Cooking MC's like a pound of bacon"})})
  => "Cooking MC's like a pound of bacon"
  (decrypt {:key "a"
            :msg (encrypt {:key "a"
                           :msg "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"})})
  => "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted")

(defn decrypt-brute-force
  "Decrypt by brute forcing"
  [byte-secret]
  (->> (range 0 255)                               ;; generate all possible characters
       (map (fn [k] [k (xor byte-secret [k])]))     ;; compute xor it with the fixed hex encrypted secret
       (reduce
        (fn [m [k x :as r]]
           (assoc m (frequency/compute-diff x) r)) ;; compute the frequency for each possible xor'd results into a sorted map (by its key)
        (sorted-map))
       first                                       ;; first element is the smallest frequency difference
       (#(let [[comp-diff [key secret]] %]         ;; use destructuring to go and fetch what we want (I let other stuff to explain)
           [((comp str char) key)
            (byte/to-ascii secret)]))))            ;; key + decoded secret key in ascii

(m/fact
  (decrypt-brute-force (hex/to-bytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
  => ["X" "Cooking MC's like a pound of bacon"])

(m/fact :using-my-own-food
  (-> {:key "X"
       :msg "Cooking MC's like a pound of bacon"}
      encrypt
      hex/to-bytes
      decrypt-brute-force)
  => ["X" "Cooking MC's like a pound of bacon"])

(m/fact :other-checking-to-bullet-proof
  (-> {:key "a"
       :msg "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"}
      encrypt
      hex/to-bytes
      decrypt-brute-force)
  => ["a" "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"])

(m/fact :other-checking-to-bullet-proof
  (-> {:key "z"
       :msg "There are no more trouble in paradise"}
      encrypt
      hex/to-bytes
      decrypt-brute-force)
  => ["z" "There are no more trouble in paradise"])

(m/fact :other-checking-to-bullet-proof
  (-> {:key " "
       :msg "hello dude"}
      encrypt
      hex/to-bytes
      decrypt-brute-force)
  => [" " "hello dude"])
