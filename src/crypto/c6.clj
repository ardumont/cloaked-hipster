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

(defn all-blocks
  "Given a byte input and a key size, return the list of byte blocks with such size"
  [byte-input key-size]
  (->> byte-input
       (partition key-size))) ;; e. Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.

(defn potential-keys
  "Given the byte-input, compute the potential key size and return the list of those possible keys with such sizes."
  [byte-input range-test]
  (let [key-size (keysize byte-input range-test)]
    (all-blocks byte-input key-size)))

;; f. Now transpose the blocks: make a block that is the first byte of every block, and a block
;; that is the second byte of every block, and so on.

;; g. Solve each block as if it was single-character XOR. You already have code to do this.

;; e. For each block, the single-byte XOR key that produces the best looking histogram is the
;;  repeating-key XOR key byte for that block. Put them together and you have the key.

(defn compute-possible-transpositions
  "Given a blocks, compute all possible transpositions."
  [blocks]
  (for [b blocks
        i (range 0 (count blocks))]
    (block/transpose i b blocks)))

(m/fact
  (compute-possible-transpositions [[1 :a] [2 :b] [3 :c]])
  => [[[1 :a 1 :a] [1 :a 2 :b] [1 :a 3 :c]] [[1 1 :a :a] [2 1 :a :b] [3 1 :a :c]] [[1 :a 1 :a] [2 :b 1 :a] [3 :c 1 :a]]
      [[2 :b 1 :a] [2 :b 2 :b] [2 :b 3 :c]] [[1 2 :b :a] [2 2 :b :b] [3 2 :b :c]] [[1 :a 2 :b] [2 :b 2 :b] [3 :c 2 :b]]
      [[3 :c 1 :a] [3 :c 2 :b] [3 :c 3 :c]] [[1 3 :c :a] [2 3 :c :b] [3 3 :c :c]] [[1 :a 3 :c] [2 :b 3 :c] [3 :c 3 :c]]])

;; f. Now transpose the blocks: make a block that is the first byte of every block, and a block
;; that is the second byte of every block, and so on.

;; g. Solve each block as if it was single-character XOR. You already have code to do this.

;; e. For each block, the single-byte XOR key that produces the best looking histogram is the
;;  repeating-key XOR key byte for that block. Put them together and you have the key.

(comment
  (def blks (potential-keys byte-input (range 2 41)))

  (def all-transposed-blocks (compute-possible-transpositions blks))

  (nth blks 4)
  (nth all-transposed-blocks 2)

  (def block-by-block
    (->> all-transposed-blocks
         (map (partial map xor/decrypt-brute-force))))
)

(defn freq
  "For a given key, compute the frequencies of xor this key with all the other blocks"
  [key blocks]
  (->> (for [msg blocks]
         (xor/xor msg key))
       frequencies))

;; Here's how to perform an attack that will break the trivial XOR encryption in a few minutes:

;;     Determine how long the key is

;; This is done by XORing the encrypted data with itself shifted various numbers of places, and examining how many bytes are the same.
;; If the bytes that are equal are greater than a certain percentage (6% accoridng to Bruce Schneier's Applied Cryptography second edition),
;; then you have shifted the data by a multiple of the keylength. By finding the smallest amount of shifting that results in a large amount of equal bytes, you find the keylength.

;;     Shift the cipher text by the keylength, and XOR against itself.

;; This removes the key and leaves you with the plaintext XORed with the plaintext shifted the length of the key. There should be enough plaintext to determine the message content.

(defn draw
  "Draw the bar chart for the given data {:col0 key :col1 freq-of-this-key} "
  [data]
  (->> data
       (into [])
       ((fn [d]
          (icore/view
           (icharts/bar-chart
            :col-0
            :col-1
            :data (icore/$where {:col-1 {:gt 0}}
                                (->> d
                                     icore/to-dataset))))))))

(defn compute-and-draw
  [key byte-input]
  (->> byte-input
       potential-keys
       (freq key)
       draw))

(comment
  (def all-data ^{:doc "all potential data to graph"}
    (let [blocks (potential-keys byte-input (range 2 41))]
      (map (fn [key] (freq key blocks)) blocks)))

  (draw (nth all-data 255))

  (let [blocks (potential-keys byte-input (range 2 41))]
    (->> blocks
         (map (fn [key] (freq key blocks)))
         (filter (fn [m]
                   (some #(< 5 %) (vals m))))))

  ;; draw the bar-chart for the english standard frequency
  (->> frequency/frequency
       draw)

  ;; given a potential key, compute and draw the diagram
  (compute-and-draw "5752" byte-input)
)
