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

(defn encrypt
  "Given a map {:key 'ascii key' :msg 'ascii message'}, encode the key message into hexadecimal then encrypt the hex msg with the hex key."
  [{:keys [key msg]}]
  (->> [msg key]
       (map hex/encode)
       (apply c2/xor)))

(m/fact
  (encrypt {:key "X"
            :msg "Cooking MC's like a pound of bacon"}) => "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

(defn decrypt
  "Given a map {:key 'ascii key' :msg 'ascii message'}, decode the encrypted message."
  [{:keys [key msg]}]
  (->> [msg (hex/encode key)]
       (apply c2/xor)
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

(defn decrypt-brute-force
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
       first                                                      ;; first element is the smallest frequency difference
       second                                                     ;; second element is our [k xor-string-with-k]
       (map hex/decode)))                                         ;; decode key and value

(m/fact
  (decrypt-brute-force "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
  => ["X" "Cooking MC's like a pound of bacon"])

(m/fact :using-my-own-food
  (-> {:key "X"
       :msg "Cooking MC's like a pound of bacon"}
      encrypt
      decrypt-brute-force)
  => ["X" "Cooking MC's like a pound of bacon"])

(m/fact :other-checking-to-bullet-proof
  (->> {:key "a"
        :msg "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"}
       encrypt
       decrypt-brute-force)
  => ["a" "There are some trouble in paradise, the sentence needs to be very long for it to be decrypted"])

(m/fact :other-checking-to-bullet-proof
  (->> {:key "z"
        :msg "There are some trouble in paradise"}
       encrypt
       decrypt-brute-force)
  => ["z" "There are some trouble in paradise"])
(comment :some-repl-tryout
  (def s "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

  (->> (range 0 255)
       (map (comp (fn [key] (c2/xor s key)) byte/to-hex))
       (map (fn [s] [(frequency/compute-frequency-total s) [s (frequency/compute-freq s)]]))
       (into {})
       (sort-by min)
       first))
