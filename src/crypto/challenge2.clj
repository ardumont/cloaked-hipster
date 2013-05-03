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
            [crypto.challenge1 :as c1]
            [crypto.byte       :as byte]
            [crypto.binary     :as binary]
            [crypto.ascii      :as ascii]
            [crypto.base64     :as b64]
            [crypto.hex        :as hex]))

(defn xor
  "compute the xor of 2 hex strings"
  [h0 h1]
  {:pre [(= (count h0) (count h1))]}
  (->> [h0 h1]
       (map (comp byte/to-bits ascii/to-bytes hex/decode))
       (apply map bit-xor)
       (partition 8)
       (map binary/to-bytes)
       hex/encode))

(m/fact
  (xor "abc" "defv")                                                                  => (m/throws AssertionError "Assert failed: (= (count h0) (count h1))")
  (xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") => "746865206b696420646f6e277420706c6179")
