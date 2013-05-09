(ns crypto.c6
  "6. Break repeating-key XOR
The buffer https://gist.github.com/3132752 is base64-encoded repeating-key XOR.
Break it.

How:
a. Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.

b. Write a function to compute the edit distance/Hamming distance between two strings.
The Hamming distance is just the number of differing bits.
The distance between: 'this is a test'
                 and: 'wokka wokka!!!'
                  is: 37

c. For each KEYSIZE, take the FIRST KEYSIZE [0..KEYSIZE] worth of bytes, and the SECOND KEYSIZE [KEYSIZE+1..2*KEYSIZE] worth of bytes,
 and find the edit distance between them. Normalize this result by dividing by KEYSIZE.

d. The KEYSIZE with the smallest normalized edit distance is probably the key.
 You could proceed perhaps with the smallest 2-3 KEYSIZE values.
 Or take 4 KEYSIZE blocks instead of 2 and average the distances.

e. Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.

f. Now transpose the blocks: make a block that is the first byte of every block, and a block
that is the second byte of every block, and so on.

g. Solve each block as if it was single-character XOR. You already have code to do this.

e. For each block, the single-byte XOR key that produces the best looking histogram is the
 repeating-key XOR key byte for that block. Put them together and you have the key."
  (:require [midje.sweet      :as m]
            [crypto.c1        :as c1]
            [crypto.distance  :as distance]
            [crypto.file      :as file]
            [crypto.byte      :as byte]
            [crypto.ascii     :as ascii]
            [crypto.hex       :as hex]
            [crypto.block     :as block]
            [crypto.frequency :as frequency]
            [crypto.char      :as char]
            [crypto.math      :as math]
            [crypto.xor       :as xor]
            [clojure.string   :as s]
            [incanter
             [core            :as icore]
             [charts          :as icharts]
             [datasets        :as idata]]))

(def hex-input (-> "./resources/base64-encoded"
                     file/load-simple
                     c1/decode))

(def byte-input (hex/to-bytes hex-input))

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
  (let [msg-to-encrypt "Let's continue our assumption that this text file is written in English. Therefore, we know which words are the most common in this language. We also know that each byte represents a character that stands for a letter or punctuation mark in the text. So it has a meaning. Because in every text different parts and words appear multiple times, we can use an algorithm that applies XOR until we get a meaningful text-file. This stands for a text file that does not contain gibberish."]
    (-> {:key "this is no longer a secret"
         :msg msg-to-encrypt}
        xor/encrypt-bytes
        (keysize (range 2 50))) => (count "this is no longer a secret")
    (-> {:key "secret"
         :msg msg-to-encrypt}
        xor/encrypt-bytes
        (keysize (range 2 50))) => (count "secret")
    (-> {:key "the key or the message must be long enough"
         :msg msg-to-encrypt}
        xor/encrypt-bytes
        (keysize (range 2 44))) => (count "the key or the message must be long enough")
    (-> {:key "yet another secret and some more secret"
         :msg msg-to-encrypt}
        xor/encrypt-bytes
        (keysize (range 2 40))) => (count "yet another secret and some more secret")))

;; f. Now transpose the blocks: make a block that is the first byte of every block, and a block
;; that is the second byte of every block, and so on.

(defn transpose-blocks
  "Given a byte input and a key size, return the list of byte blocks transposed."
  [byte-input key-size]
  (->> byte-input
       (map-indexed (fn [i b] [i b]))
       (reduce
        (fn [m [i b]]
          (let [idx (if (zero? i) 0 (mod i key-size))]
            (update-in m [idx] conj b)))
        (sorted-map))
       (map (comp reverse second))))

(m/fact
  (transpose-blocks (range 0 20) 4) => [(range 0 20 4)
                                        (range 1 20 4)
                                        (range 2 20 4)
                                        (range 3 20 4)])

;; e. For each block, the single-byte XOR key that produces the best looking histogram is the
;;  repeating-key XOR key byte for that block. Put them together and you have the key.

;; f. Now transpose the blocks: make a block that is the first byte of every block, and a block
;; that is the second byte of every block, and so on.

;; g. Solve each block as if it was single-character XOR. You already have code to do this.

(defn compute-key
  "Given a byte-input, compute its keysize and try and compute the key by transposing block of keysize."
  ([byte-input]
     (->> (range 2 40)
          (keysize byte-input)
          (compute-key byte-input)))
  ([byte-input key-size]
     (->> (transpose-blocks byte-input key-size)
          (map (comp first xor/decrypt-brute-force))
          (s/join ""))))

(m/fact
  (let [msg-to-encrypt "Let's continue our assumption that this text file is written in English. Therefore, we know which words are the most common in this language. We also know that each byte represents a character that stands for a letter or punctuation mark in the text. So it has a meaning. Because in every text different parts and words appear multiple times, we can use an algorithm that applies XOR until we get a meaningful text-file. This stands for a text file that does not contain gibberish."]
    (-> {:key "secret"
         :msg msg-to-encrypt}
        xor/encrypt-bytes
        (compute-key (count "secret"))) => "secret"
    (-> {:key "secret"
         :msg msg-to-encrypt}
        xor/encrypt-bytes
        compute-key)                    => "secret"))

(comment
  (xor/decrypt-bytes {:key "yet fnother'secrev and some mor} secret"
                      :msg (-> {:key "yet another secret and some more secret"
                                :msg msg-to-encrypt}
                               xor/encrypt-bytes)})
)
