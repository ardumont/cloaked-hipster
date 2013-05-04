(ns crypto.challenge3
  "3. Single-character XOR Cipher
The hex encoded string:
  1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
... has been XOR'd against a single character.
Find the key, decrypt the message.

Write code to do this for you.
How? Devise some method for scoring a piece of English plaintext (Character frequency is a good metric).
Evaluate each output and choose the one with the best score.
Tune your algorithm until this works."
  (:require [midje.sweet       :as m]
            [crypto.xor        :as xor]
            [crypto.byte       :as byte]
            [crypto.binary     :as binary]
            [crypto.ascii      :as ascii]
            [crypto.base64     :as b64]
            [crypto.hex        :as hex]))

(m/fact
  (xor/decrypt-brute-force "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
  => ["X" "Cooking MC's like a pound of bacon"])
