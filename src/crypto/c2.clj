(ns crypto.c2
  "XOR 2 equal-length hex-encoded buffers."
  (:require [midje.sweet :as m]
            [crypto.xor  :as xor]
            [crypto.hex  :as hex]
            [crypto.byte :as byte]))

(defn xor
  "Given a hex msg and a hex key, encode the msg with the key using xor"
  [msg key]
  (->> [msg key]
       (map hex/>bytes)
       (apply xor/xor)
       byte/>hex))

(m/fact
  (xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179"
  (xor "746865206b696420646f6e277420706c6179" "686974207468652062756c6c277320657965") => "1c0111001f010100061a024b53535009181c")
