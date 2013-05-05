(ns crypto.c6
  "6. Break repeating-key XOR
The buffer https://gist.github.com/3132752 is base64-encoded repeating-key XOR.
Break it.

How:
a. Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.

b. Write a function to compute the edit distance/Hamming distance between two strings.
The Hamming distance is just the number of differing bits.
The distance between: this is a test
                 and: wokka wokka!!!
                 is 37.

c. For each KEYSIZE, take the FIRST KEYSIZE [0..KEYSIZE] worth of bytes, and the SECOND KEYSIZE [n+1..2*KEYSIZE] worth of bytes,
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

(defn normalize
  "Normalize the hamming distance between 4 n-blocks from the sequence s."
  [n s]
  (let [bs [(block 0 n s) (block n n s) (block (* 2 n) n s) (block (* 3 n) n s)]
        d (/ (->> bs
                  (map (fn [[bs be]] (distance/hamming bs be)))
                  (apply +))
             4)]
    (/ d n)))

(m/fact
  (normalize 2 (ascii/to-bytes "hello world")) => 19/8)

(defn potential-key-sizes
  "Given the input, return the potential sizes (in natural order)"
  [byte-input]
  (->> (range 2 41)
       (reduce
        (fn [m n] (assoc m n (normalize n byte-input)))
        (sorted-map))))

(comment
  (potential-key-sizes byte-input))

(defn all-blocks
  "Given a byte input and a key size, return the list of hex-encoded blocks with such size"
  [byte-input key-size]
  (->> byte-input
       (partition key-size)))

(comment
  (all-blocks byte-input 2))

(defn potential-keys
  "Given the byte-input, compute the potential key size and return the list of those possible keys with such sizes."
  [byte-input]
  (let [key-size (-> byte-input
                     potential-key-sizes
                     vals
                     first
                     int)]
    (all-blocks byte-input key-size)))

(defn shift
  "n-shift the sequence of data"
  [n data]
  (if (= 0 n)
    data
    (let [[h t] (split-at n data)]
      (concat t h))))

(m/fact
  (shift 0 [:a :b])             => [:a :b]
  (shift 3 [:a :b :c :d :e :f]) => [:d :e :f :a :b :c]
  (shift 1 [:a :b :c :d :e :f]) => [:b :c :d :e :f :a])

(defn inject-block
  "Given an input of data, inject a block of data at the nth position"
  [n block data]
  (->> data
       (map (fn [seq]
              (->> seq
                   (split-at n)
                   (#(let [[h t] %]
                       (concat h (conj t block))))
                   flatten)))))

(m/fact
  (inject-block 0 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 1 :a] [1 :a 2 :b] [1 :a 3 :c]]
  (inject-block 1 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 1 :a :a] [2 1 :a :b] [3 1 :a :c]]
  (inject-block 2 [1 :a] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 1 :a] [2 :b 1 :a] [3 :c 1 :a]]
  (inject-block 1 [2 :b] [[1 :a] [2 :b] [3 :c]]) => [[1 2 :b :a] [2 2 :b :b] [3 2 :b :c]]
  (inject-block 2 [3 :c] [[1 :a] [2 :b] [3 :c]]) => [[1 :a 3 :c] [2 :b 3 :c] [3 :c 3 :c]])

(defn compute-possible-transpositions
  "Given a blocks, compute all possible transpositions."
  [blocks]
  (->> blocks
       (map-indexed
        (fn [i k] (inject-block i k blocks)))))

(m/fact
  (compute-possible-transpositions [[1 :a] [2 :b] [3 :c]]) => [[[1 :a 1 :a] [1 :a 2 :b] [1 :a 3 :c]]
                                                               [[1 2 :b :a] [2 2 :b :b] [3 2 :b :c]]
                                                               [[1 :a 3 :c] [2 :b 3 :c] [3 :c 3 :c]]])

;; f. Now transpose the blocks: make a block that is the first byte of every block, and a block
;; that is the second byte of every block, and so on.

;; g. Solve each block as if it was single-character XOR. You already have code to do this.

;; e. For each block, the single-byte XOR key that produces the best looking histogram is the
;;  repeating-key XOR key byte for that block. Put them together and you have the key.

(comment
  (def blks (potential-keys byte-input))
  (def blk (first blks))

  (def all-transposed-blocks (compute-possible-transpositions blks))
  (ffirst all-transposed-blocks)

  (def block-by-block
    (->> all-transposed-blocks
         (map (fn [bl]
                (map xor/decrypt-brute-force bl)))))
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
    (let [blocks (potential-keys byte-input)]
      (map (fn [key] (freq key blocks)) blocks)))

  (draw (nth all-data 255))

  (let [blocks (potential-keys byte-input)]
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
