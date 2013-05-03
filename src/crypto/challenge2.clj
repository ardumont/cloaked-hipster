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
            [crypto.xor        :as xor]))

(defn xor
  "Compute xor of 2 hex strings"
  [h0 h1]
  (->> [h0 h1]
       (map hex/to-bits)
       (apply xor/bitxor)
       (partition 8)
       (map binary/to-bytes)
       hex/encode))

(m/fact :one-way
  (xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (xor "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")

(defn xor-with-checks
  "Compute the xor of 2 hex strings of same size."
  [h0 h1]
  {:pre [(= (count h0) (count h1))]}
  (xor h0 h1))

(m/fact
  (xor-with-checks "abc" "defv")                                                                  => (m/throws AssertionError "Assert failed: (= (count h0) (count h1))")
  (xor-with-checks "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (xor-with-checks "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")
