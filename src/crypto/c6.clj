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
  (:require [midje.sweet     :as m]
            [crypto.c1       :as c1]
            [crypto.xor      :as xor]
            [crypto.distance :as distance]
            [crypto.file     :as file]
            [crypto.byte     :as byte]
            [crypto.ascii    :as ascii]
            [crypto.hex      :as hex]
            [incanter
             [core     :as icore]
             [charts   :as icharts]
             [datasets :as idata]]))

(def byte-input (-> "./resources/base64-encoded"
                     file/load-simple
                     c1/decode
                     hex/to-bytes))

(defn block
  "Compute 2 n-block chars, [0..n] and [n..n+1]"
  [n s]
  [(take n s) (take n (drop n s))])

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

(defn potential-size
  "Given the input, return the potential sizes (in natural order)"
  [input]
  (->> (range 2 41)
       (reduce
        (fn [m n] (assoc m n (normalize n input)))
        (sorted-map))))

(defn freq
  "Compute the frequencies from the n possible key"
  [n input]
  (let [size           (-> (potential-size input)
                           ffirst)
        potential-keys (->> input
                            (partition size)
                            (map (comp hex/encode ascii/to-bytes)))
        k              (nth potential-keys n)]
    (->> (for [msg potential-keys]
           (xor/xor msg k))
         frequencies
         (into []))))

(defn draw
  [n input]
  (->> input
       (freq n)
       ((fn [data] (icore/view
                   (icharts/bar-chart :col-0 :col-1 :data (->> data
                                                               icore/to-dataset)))))))

(comment
  (let [size           (-> (potential-size byte-input)
                           ffirst)
        potential-keys (->> byte-input
                            (partition size)
                            (map (comp hex/encode ascii/to-bytes)))
        k              (nth potential-keys 1)]
    (->> (for [msg potential-keys]
           (xor/xor msg k))
         frequencies
         ((fn [data] (icore/view
                     (icharts/bar-chart :col-0 :col-1 :data (->> data
                                                                 (into [])
                                                                 icore/to-dataset)))))))

  (fn [data] (icore/view
             (icharts/bar-chart :col-0 :col-1 :data (->> data
                                                         (into [])
                                                         icore/to-dataset))))
  )
