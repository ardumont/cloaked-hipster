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
            [crypto.xor       :as xor]
            [crypto.distance  :as distance]
            [crypto.file      :as file]
            [crypto.byte      :as byte]
            [crypto.ascii     :as ascii]
            [crypto.hex       :as hex]
            [crypto.frequency :as frequency]
            [incanter
             [core            :as icore]
             [charts          :as icharts]
             [datasets        :as idata]]))

(def byte-input (-> "./resources/base64-encoded"
                     file/load-simple
                     c1/decode
                     hex/to-bytes))

(defn block
  "Compute 2 n-block chars, [0..n] and [n..n+1]"
  [n s]
  [(take n s) (->> s (drop n) (take n))])

(m/fact
  (block 3 (ascii/to-bytes "hello, dude")) => [[104 101 108] [108 111 44]]
  (block 3 "hello world!")                 => [[\h \e \l] [\l \o \space]]
  (block 7 "hello world!")                 => [[\h \e \l \l \o \space \w] [\o \r \l \d \!]])

(defn normalize
  "Normalize the hamming distance between 2 n-blocks from the sequence s."
  [n s]
  (let [d (->> s
          (block n)
          (apply distance/hamming))]
    (/ d n)))

(m/fact
  (normalize 2 (ascii/to-bytes "hello")) => 3/2)

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

(defn mean
  [vals]
  (let [n 4]
    (->
     (->> vals (take n) (apply +))
     (/ n)
     float
     Math/round)))

(m/fact
  (mean (-> byte-input potential-key-sizes vals)) => 2)

(defn potential-keys
  "Given the byte-input, compute the potential key size and return the list of those possible keys with such sizes."
  [byte-input]
  (let [key-size (mean (-> byte-input
                           potential-key-sizes
                           vals))]
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
  (let [l (count block)]
    (->> data
         (shift n)
         (interleave (cycle [block])))))

(m/fact
  ;; (inject-block 0 [1 2 3] [:a :b :c :d :e :f]) => [1 2 3 :a :b :c 1 2 3 :d :e :f]
  ;; (inject-block 1 [1 2 3] [:a :b :c :d :e :f]) => [1 2 3 :d :e :f 1 2 3 :a :b :c]
  ;; (inject-block 2 [1 2 3] [:a :b :c :d :e :f]) => [1 2 3 :a :b :c 1 2 3 :d :e :f]
  ;; (inject-block 2 [1 2]   [:a :b :c :d :e :f]) => [1 2 :e :f 1 2 :a :b 1 2 :c :d]
  (inject-block 0 [1 :a]  [[1 :a] [2 :b] [3 :c]]) => [[1 :a] [1 :a] [1 :a] [2 :b] [1 :a] [3 :c]]
  (inject-block 1 [1 :a]  [[1 :a] [2 :b] [3 :c]]) => [[1 :a] [2 :b] [1 :a] [3 :c] [1 :a] [1 :a]]
  (inject-block 2 [1 :a]  [[1 :a] [2 :b] [3 :c]]) => [[1 :a] [3 :c] [1 :a] [1 :a] [1 :a] [2 :b]])

;; e. Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.

;; f. Now transpose the blocks: make a block that is the first byte of every block, and a block
;; that is the second byte of every block, and so on.

;; g. Solve each block as if it was single-character XOR. You already have code to do this.

;; e. For each block, the single-byte XOR key that produces the best looking histogram is the
;;  repeating-key XOR key byte for that block. Put them together and you have the key.

(defn compute
  "Given a blocks, compute all possible transpositions."
  [blocks]
  (->> blocks
       (map-indexed
        (fn [i k] (inject-block i k blocks)))))

(m/fact
  (compute [[1 :a] [3 :b] [5 :b]]) => [[[1 :a] [1 :a] [1 :a] [3 :b] [1 :a] [5 :b]]
                                       [[3 :b] [3 :b] [3 :b] [5 :b] [3 :b] [1 :a]]
                                       [[5 :b] [5 :b] [5 :b] [1 :a] [5 :b] [3 :b]]])

(defn freq
  "For a given key, compute the frequencies of xor this key with all the other blocks"
  [key blocks]
  (->> (for [msg blocks]
         (xor/xor msg key))
       frequencies))

;; Here's how to perform an attack that will break the trivial XOR encryption in a few minutes:

;;     Determine how long the key is

;; This is done by XORing the encrypted data with itself shifted various numbers of places, and examining how many bytes are the same. If the bytes that are equal are greater than a certain percentage (6% accoridng to Bruce Schneier's Applied Cryptography second edition), then you have shifted the data by a multiple of the keylength. By finding the smallest amount of shifting that results in a large amount of equal bytes, you find the keylength.

;;     Shift the cipher text by the keylength, and XOR against itself.

;; This removes the key and leaves you with the plaintext XORed with the plaintext shifted the length of the key. There should be enough plaintext to determine the message content.

;; Section 8.2 of the Sci.crypt FAQ has more details on cracking repeated-key ciphers.

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
