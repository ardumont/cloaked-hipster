(ns crypto.challenge2
  "2. Fixed XOR
Write a function that takes two equal-length buffers (hex encoded) and produces their XOR sum.
The string:
 1c0111001f010100061a024b53535009181c - hex encoded
... after hex decoding, when xor'd against:
 686974207468652062756c6c277320657965 - hex encoded (hit the bull's eye)
... should produce:
 746865206b696420646f6e277420706c6179 - hex encoded (the kids don't play)"
  (:require [midje.sweet :as m]
            [crypto.xor  :as xor]))

(m/fact
  (xor/xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (xor/xor "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")
