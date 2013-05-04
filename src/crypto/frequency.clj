(ns crypto.frequency
  "A dictionary namespace"
  (:require [midje.sweet :as m]
            [crypto.byte :as byte]
            [clojure.set :as set]
            [crypto.char :as char]
            [crypto.hex  :as hex]
            [crypto.util :as util]))

(def ^{:doc "English letter frequency - http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html"}
  frequency
  (into {} [[\e 12.02] [\E 12.02] [\t 9.10] [\T 9.10] [\a 8.12] [\A 8.12] [\o 7.68] [\O 7.68]
            [\i 7.31] [\I 7.31] [\n 6.95] [\N 6.95] [\s 6.28] [\S 6.28] [\r 6.02] [\R 6.02]
            [\h 5.92] [\H 5.92] [\d 4.32] [\D 4.32] [\l 3.98] [\L 3.98] [\u 2.88] [\U 2.88]
            [\c 2.71] [\C 2.71] [\m 2.61] [\M 2.61] [\f 2.30] [\F 2.30] [\y 2.11] [\Y 2.11]
            [\w 2.09] [\W 2.09] [\g 2.03] [\G 2.03] [\p 1.82] [\P 1.82] [\b 1.49] [\B 1.49]
            [\v 1.11] [\V 1.11] [\k 0.69] [\K 0.69] [\x 0.17] [\X 0.17] [\q 0.11] [\Q 0.11]
            [\j 0.10] [\J 0.10] [\z 0.07] [\Z 0.07] [\space 0.1]]))

(m/fact
  (->> (util/range-char \a \z)
       (map frequency))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07]
  (->> (util/range-char \A \Z)
       (map frequency))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07])

(def hex-frequency ^{:doc "frequency of english, key are encoded into hexadecimal"}
  (reduce
   (fn [m [k v]] (assoc m (char/to-hex k) v))
   {}
   frequency))

(m/fact
  (->> (util/range \a \z)
       (map (comp hex-frequency byte/to-hex)))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07]
    (->> (util/range \A \Z)
         (map (comp hex-frequency byte/to-hex)))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07])

(defn- compute-freq
  "Compute the frequency of hexadecimal characters inside a string."
  [s]
  (let [l (/ (count s) 2)]
    (->> s
         (partition 2)
         frequencies
         (reduce
          (fn [m [k v]]
            (assoc m (clojure.string/join "" k) (float (/ v l))))
          {}))))

(m/future-fact :does-not-understand-why-this-does-work-yet
  (compute-freq (hex/encode "hello")) => (m/just {"6f" 0.1, "6c" 0.2, "65" 0.1, "68" 0.1}))

(m/fact "`just` provides extended equality"
  {:a 1, :b 2, :c "some text"} => (m/just {:a 1, :b 2, :c #"text"}))

(defn- diff-freq
  "Compute the difference between two frequency maps into a difference frequency map."
  [m0 m1]
  (->> (set/union (-> m0 keys set)
                  (-> m1 keys set))
       (reduce
        (fn [m k] (assoc m k (Math/abs (- (m0 k 0) (m1 k 0)))))
        {})))

(m/fact
  (diff-freq {:a 1 :b 10} {:c 10 :b 10 :a 2})
  => {:a 1 :b 0 :c 10})

(defn- sum-diff-map
  "Compute the sum of a map."
  [d-freq]
  (reduce (fn [s [k v]] (+ s v)) 0 d-freq))

(m/fact
  (sum-diff-map {:a 1 :b 2 :c 3}) => 6)

(defn compute-diff
  "Given a hexadecimal encoded word, compute the difference frequency from the standard english frequency map."
  [w]
  (->> w
       compute-freq
       (diff-freq hex-frequency)
       sum-diff-map))

(m/fact
  (compute-diff (hex/encode "hello")) => 199.07999998509885)

(comment :some-tryout-from-the-repl
  (hex-frequency "77")
  (def s "e4c8c8cccec9c087eae480d487cbceccc287c687d7c8d2c9c387c8c187c5c6c4c8c9")
  (def h-freq (compute-freq s))
  (def d-freq (diff-freq hex-frequency h-freq))
  (reduce (fn [s [k v]] (+ s v)) 0 d-freq)
  (compute-frequency-total s))
