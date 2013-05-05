(ns crypto.c2
  "2. Fixed XOR
Write a function that takes two equal-length buffers (hex encoded) and produces their XOR sum.
The string:
 1c0111001f010100061a024b53535009181c - hex encoded
... after hex decoding, when xor'd against:
 686974207468652062756c6c277320657965 - hex encoded (hit the bull's eye)
... should produce:
 746865206b696420646f6e277420706c6179 - hex encoded (the kids don't play)"
  (:require [midje.sweet :as m]
            [crypto.xor  :as xor]
            [crypto.hex  :as hex]
            [crypto.byte :as byte]))

(defn xor
  "Given a hex msg and a hex key, encode the msg with the key using xor"
  [msg key]
  (->> [msg key]
       (map hex/to-bytes)
       (apply xor/xor)
       byte/to-hex))

(m/fact
  (xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (xor "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")
