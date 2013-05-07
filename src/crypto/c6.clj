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
            [crypto.c2        :as c2]
            [crypto.xor       :as xor]
            [crypto.distance  :as distance]
            [crypto.file      :as file]
            [crypto.byte      :as byte]
            [crypto.ascii     :as ascii]
            [crypto.hex       :as hex]
            [crypto.frequency :as frequency]
            [crypto.char      :as char]
            [clojure.string   :as s]
            [incanter
             [core            :as icore]
             [charts          :as icharts]
             [datasets        :as idata]]))

(def hex-input (-> "./resources/base64-encoded"
                     file/load-simple
                     c1/decode))

(def byte-input (hex/to-bytes hex-input))

(defn block
  "Compute 2 n-block chars, [0..n] and [n..n+1]"
  [s n v]
  (let [vect (vec v)]
    [(subvec vect s (+ s n)) (subvec vect (+ s n) (+ s (* 2 n)))]))

(m/fact
  (block 0 3 (ascii/to-bytes "hello, dude")) => [[104 101 108] [108 111 44]]
  (block 0 3 "hello world!")                 => [[\h \e \l] [\l \o \space]]
  (block 0 6 "hello world!")                 => [[\h \e \l \l \o \space] [\w \o \r \l \d \!]]
  (block 2 2 "hello world!")                 => [[\l \l] [\o \space]])

(defn make-4blocks
  "Make nb-blocks of size n with the string s. If nb-blocks is too large, return by convention 4 blocks."
  [n s]
  (for [i (range 0 4)] (block (* n i) n s)))

(m/fact
  (make-4blocks 2 "hello world, will be broke into multiple blocks.") => [[[\h \e] [\l \l]]
                                                                          [[\l \l] [\o \space]]
                                                                          [[\o \space] [\w \o]]
                                                                          [[\w \o] [\r \l]]]
  (make-4blocks 5 "little by little, we close the line ")             => [[[\l \i \t \t \l] [\e \space \b \y \space]]
                                                                          [[\e \space \b \y \space] [\l \i \t \t \l]]
                                                                          [[\l \i \t \t \l] [\e \, \space \w \e]]
                                                                          [[\e \, \space \w \e] [\space \c \l \o \s]]])

(defn norm-hamming
  "Normalize the hamming distance between 4 n-blocks from the sequence byte-input."
  [n byte-input]
  (let [blks (make-4blocks n byte-input)                       ;;  c. For each KEYSIZE (n), take the FIRST n block worth of bytes [0..n], and the SECOND n blocks worth of bytes [n+1..2*n]
                                                               ;; (Or take 4 KEYSIZE blocks instead of 2 and average the distances.)
        l    (count blks)
        c    (->> blks
                  (map (fn [[bs be]] (distance/hamming bs be))) ;; and find the hamming/edit distance between them.
                  (apply +))
        mean-distance (/ c l)]
    (/ mean-distance n)))                                      ;; Normalize this result by dividing by KEYSIZE.

(m/fact
  (norm-hamming 2 (ascii/to-bytes "hello world, this must be long enough!")) => 19/8
  (norm-hamming 3 (ascii/to-bytes "hello world, this must be long enough!")) => 31/12)

(defn keysize
  "Given the input, return the potential keysize."
  [byte-input range-test]
  (->> range-test                                         ;; a. Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.
       (reduce
        (fn [m n] (assoc m n (norm-hamming n byte-input))) ;; c. For each KEYSIZE, take the FIRST KEYSIZE [0..KEYSIZE] worth of bytes, and the SECOND KEYSIZE [KEYSIZE+1..2*KEYSIZE] worth
                                                          ;; of bytes, and find the edit distance between them. Normalize this result by dividing by KEYSIZE.
        {})
       (apply min-key second)                             ;; d. The KEYSIZE with the smallest normalized
       first))                                            ;;  hamming/edit distance is probably the key.

(comment
  (keysize byte-input (range 2 41)))

(defn all-blocks
  "Given a byte input and a key size, return the list of byte blocks with such size"
  [byte-input key-size]
  (->> byte-input
       (partition key-size))) ;; e. Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.

(comment
  (all-blocks byte-input 2)

  ;; I don't succeed in breaking, so i test with things i know!

  (def encrypted-msg (-> {:key "secret"
                          :msg "this is a test with sufficient length to have data to break"}
                         xor/encrypt
                         hex/to-bytes))

  (count encrypted-msg)

  ;; check that i can decrypt - OK
  (byte/to-ascii (xor/xor encrypted-msg (ascii/to-bytes "secret")))

  ;; now trying to break it

  ;; first determine the keysize...
  (keysize encrypted-msg (range 2 12))

  (->> (range 2 12)
       (reduce
        (fn [m n] (assoc m n (norm-hamming n encrypted-msg)))
        {})
       )

  ;; boum - 2 this is false, the keysize is 6

  )

(defn potential-keys
  "Given the byte-input, compute the potential key size and return the list of those possible keys with such sizes."
  [byte-input range-test]
  (let [key-size (keysize byte-input range-test)]
    (all-blocks byte-input key-size)))

(defn shift
  "n-shift the sequence of data"
  [n data]
  (if (= 0 n)
    data
    (let [l     (count data)
          [h t] (split-at (mod n l) data)]
      (concat t h))))

(m/fact
  (shift 0  [:a :b])             => [:a :b]
  (shift 3  [:a :b :c :d :e :f]) => [:d :e :f :a :b :c]
  (shift 1  [:a :b :c :d :e :f]) => [:b :c :d :e :f :a]
  (shift -1 [:a :b :c])          => [:c :a :b])

(defn inject-block
  "Given an input of data, inject a block of data at the nth position"
  [n block data]
  (let [l (-> block count inc)]
    (->> data
         (map (fn [seq]
                (->> seq
                     (split-at (mod n l))
                     (#(let [[h t] %]
                         (concat h (conj t block))))
                     flatten))))))

(m/fact
  (inject-block 0 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 1 :a] [1 :a 2 :b] [1 :a 3 :c]]
  (inject-block 1 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 1 :a :a] [2 1 :a :b] [3 1 :a :c]]
  (inject-block 2 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 1 :a] [2 :b 1 :a] [3 :c 1 :a]]
  (inject-block 3 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 1 :a] [1 :a 2 :b] [1 :a 3 :c]]
  (inject-block 1 [2 :b] [[1 :a] [2 :b] [3 :c]]) => [[1 2 :b :a] [2 2 :b :b] [3 2 :b :c]]
  (inject-block 2 [3 :c] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 3 :c] [2 :b 3 :c] [3 :c 3 :c]])

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
    (inject-block i b blocks)))

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
