(ns crypto.xor
  "xor computations"
  (:require [midje.sweet :as m]
            [crypto.hex  :as hex]
            [crypto.binary :as binary]))

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

(defn compute-xor
  "Compute xor of 2 hex strings"
  [h0 h1]
  (->> [h0 h1]
       (map hex/to-bits)
       (apply bitxor)
       (partition 8)
       (map binary/to-bytes)
       hex/encode))

(m/fact :one-way-and-back
  (compute-xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (compute-xor "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")

(defn xor
  "Compute the xor of between the input s and the key key."
  [s key]
  {:pre [(>= (count s) (count key))]}
  (->> key
       cycle
       (take (count s))
       (compute-xor s)))

(m/fact
  (xor "abcd" "defghi")                                                               => (m/throws AssertionError "Assert failed: (>= (count s) (count key))")
  (xor "abcd" "de")                                                                   => "7513"
  (xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (xor "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")

(defn encrypt
  "Given a map {:key 'ascii key' :msg 'ascii message'}, encode the key message into hexadecimal then encrypt the hex msg with the hex key."
  [{:keys [key msg]}]
  (->> [msg key]
       (map hex/encode)
       (apply xor)))

(m/fact
  (encrypt {:key "X"
            :msg "Cooking MC's like a pound of bacon"}) => "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

(defn decrypt
  "Given a map {:key 'ascii key' :msg 'ascii message'}, decode the encrypted message."
  [{:keys [key msg]}]
  (->> [msg (hex/encode key)]
       (apply xor)
       hex/decode))

(m/fact
  (decrypt {:key "X"
            :msg (encrypt {:key "X"
                           :msg "Cooking MC's like a pound of bacon"})})
  => "Cooking MC's like a pound of bacon"
  (decrypt {:key "a"
            :msg (encrypt {:key "a"
                           :msg "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"})})
  => "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted")
