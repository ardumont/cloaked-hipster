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
            [crypto.challenge2 :as c2]
            [crypto.byte       :as byte]
            [crypto.binary     :as binary]
            [crypto.ascii      :as ascii]
            [crypto.base64     :as b64]
            [crypto.hex        :as hex]
            [crypto.frequency  :as frequency]))

(defn decrypt
  "Decrypt by brute forcing"
  [hex-encrypted-secret]
  (->> (range 0 255)                                              ;; generate all possible characters
       (map (comp
             (fn [k] [k (c2/xor hex-encrypted-secret k)])
             byte/to-hex))                                        ;; compute the character's byte representation then xor it with the fixed hex encrypted secret
       (reduce
        (fn [m [k x :as r]]
           (assoc m (frequency/compute-diff x) r))                ;; compute the frequency for each possible xor'd results into a sorted map (by its key)
        (sorted-map))
       first                                                      ;; first element is the least frequency difference
       second                                                     ;; second element is our [k xor-string-with-k]
       (map hex/decode)))                                         ;; decode key and value

(m/fact
  (decrypt "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
  => ["X" "Cooking MC's like a pound of bacon"])

(m/fact :using-my-own-food
  (->> ["Cooking MC's like a pound of bacon" "X"]
       (map hex/encode)
       (apply c2/xor)
       decrypt)
  => ["X" "Cooking MC's like a pound of bacon"])

(m/fact :other-checking-to-bullet-proof
  (->> ["There are some trouble in paradise, the sentence needs to be very long for it to be decrypted" "a"]
       (map hex/encode)
       (apply c2/xor)
       decrypt)
  => ["a" "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"])

(comment :some-repl-tryout
  (def s "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

  (->> (range 0 255)
       (map (comp (fn [key] (c2/xor s key)) byte/to-hex))
       (map (fn [s] [(frequency/compute-frequency-total s) [s (frequency/compute-freq s)]]))
       (into {})
       (sort-by min)
       first))
