(ns crypto.key
  "Manipulation around computing the key size and the key itself from xor encoded bytes."
  (:require [midje.sweet    :as m]
            [crypto
             [block         :as block]
             [frequency     :as frequency]
             [math          :as math]
             [xor           :as xor]
             [byte          :as byte]]
            [clojure.string :as string]))

(def ^{:doc "threshold from which we have a multiple of the key length"}
  threshold 3/50)

(defn keysize
  "Given an encrypted message and a range test, compute the potential key size."
  [encrypted-msg range-test]
  (->> (for [n range-test
             :let [freq (->> encrypted-msg
                             (block/shift n)
                             (frequency/frequency-equals encrypted-msg))]
             :when (< threshold freq)]
         n)
       math/lgcd))

(m/fact
  (let [msg->encrypt "Let's continue our assumption that this text file is written in English. Therefore, we know which words are the most common in this language. We also know that each byte represents a character that stands for a letter or punctuation mark in the text. So it has a meaning. Because in every text different parts and words appear multiple times, we can use an algorithm that applies XOR until we get a meaningful text-file. This stands for a text file that does not contain gibberish."]
    (-> {:key "this is no longer a secret"
         :msg msg->encrypt}
        xor/encrypt-bytes
        (keysize (range 2 50))) => (count "this is no longer a secret")
    (-> {:key "secret"
         :msg msg->encrypt}
        xor/encrypt-bytes
        (keysize (range 2 50))) => (count "secret")
    (-> {:key "the key or the message must be long enough"
         :msg msg->encrypt}
        xor/encrypt-bytes
        (keysize (range 2 44))) => (count "the key or the message must be long enough")
    (-> {:key "yet another secret and some more secret"
         :msg msg->encrypt}
        xor/encrypt-bytes
        (keysize (range 2 40))) => (count "yet another secret and some more secret")))

(defn compute-key
  "Given a byte-input, compute its keysize and try and compute the key by transposing block of keysize."
  ([byte-input]
     (->> (range 2 40)
          (keysize byte-input)
          (compute-key byte-input)))
  ([byte-input key-size]
     (->> (block/transpose byte-input key-size)
          (map (comp first xor/decrypt-brute-force))
          (string/join ""))))

(m/fact
  (let [msg->encrypt "Let's continue our assumption that this text file is written in English. Therefore, we know which words are the most common in this language. We also know that each byte represents a character that stands for a letter or punctuation mark in the text. So it has a meaning. Because in every text different parts and words appear multiple times, we can use an algorithm that applies XOR until we get a meaningful text-file. This stands for a text file that does not contain gibberish."]
    (-> {:key "secret"
         :msg msg->encrypt}
        xor/encrypt-bytes
        (compute-key (count "secret"))) => "secret"
    (-> {:key "secret"
         :msg msg->encrypt}
        xor/encrypt-bytes
        compute-key)                    => "secret"))

(comment
  (def msg->encrypt "Let's continue our assumption that this text file is written in English. Therefore, we know which words are the most common in this language. We also know that each byte represents a character that stands for a letter or punctuation mark in the text. So it has a meaning. Because in every text different parts and words appear multiple times, we can use an algorithm that applies XOR until we get a meaningful text-file. This stands for a text file that does not contain gibberish.")

  (-> {:key "yet another secret and some more"
       :msg msg->encrypt}
      xor/encrypt-bytes
      (compute-key (count "yet another secret and some more")))

  (-> (xor/decrypt-bytes {:key "yet another secret and ssme more"
                          :msg (-> {:key "yet another secret and some more"
                                    :msg msg->encrypt}
                                   xor/encrypt-bytes)})
      byte/>ascii))
