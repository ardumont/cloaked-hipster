(ns crypto.challenge2
  "2. Fixed XOR
Write a function that takes two equal-length buffers (hex encoded) and produces their XOR sum.
The string:
 1c0111001f010100061a024b53535009181c - hex encoded
... after hex decoding, when xor'd against:
 686974207468652062756c6c277320657965 - hex encoded (hit the bull's eye)
... should produce:
 746865206b696420646f6e277420706c6179 - hex encoded (the kids don't play)"
  (:require [midje.sweet       :as m]
            [crypto.binary     :as binary]
            [crypto.hex        :as hex]
            [crypto.xor        :as xor]
            [clojure.string    :as s]))

(defn compute-xor
  "Compute xor of 2 hex strings"
  [h0 h1]
  (->> [h0 h1]
       (map hex/to-bits)
       (apply xor/bitxor)
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
