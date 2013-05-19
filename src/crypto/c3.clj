(ns crypto.c3
  "Single-character XOR Cipher. Find the key and decrypt the message."
  (:require [midje.sweet       :as m]
            [crypto.xor        :as xor]
            [crypto.byte       :as byte]
            [crypto.binary     :as binary]
            [crypto.ascii      :as ascii]
            [crypto.base64     :as b64]
            [crypto.hex        :as hex]))

(def brute-force (comp xor/decrypt-brute-force hex/>bytes))

(m/fact
  (brute-force "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
  => ["X" "Cooking MC's like a pound of bacon"])
