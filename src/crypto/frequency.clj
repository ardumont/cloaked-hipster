(ns crypto.frequency
  "A dictionary namespace"
  (:require [midje.sweet  :as m]
            [crypto.byte  :as byte]
            [clojure.set  :as set]
            [crypto.char  :as char]
            [crypto.math  :as math]
            [crypto.block :as block]))

(def ^{:doc "English letter frequency - http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html"}
  frequency
  (into {} [[\e 12.02] [\E 12.02] [\t 9.10] [\T 9.10] [\a 8.12] [\A 8.12] [\o 7.68] [\O 7.68]
            [\i 7.31]  [\I 7.31]  [\n 6.95] [\N 6.95] [\s 6.28] [\S 6.28] [\r 6.02] [\R 6.02]
            [\h 5.92]  [\H 5.92]  [\d 4.32] [\D 4.32] [\l 3.98] [\L 3.98] [\u 2.88] [\U 2.88]
            [\c 2.71]  [\C 2.71]  [\m 2.61] [\M 2.61] [\f 2.30] [\F 2.30] [\y 2.11] [\Y 2.11]
            [\w 2.09]  [\W 2.09]  [\g 2.03] [\G 2.03] [\p 1.82] [\P 1.82] [\b 1.49] [\B 1.49]
            [\v 1.11]  [\V 1.11]  [\k 0.69] [\K 0.69] [\x 0.17] [\X 0.17] [\q 0.11] [\Q 0.11]
            [\j 0.10]  [\J 0.10]  [\z 0.07] [\Z 0.07] [\space 0.1]]))

(m/fact
  (->> (crypto.util/range-char \a \z)
       (map frequency))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07]
  (->> (crypto.util/range-char \A \Z)
       (map frequency))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07])

(def byte-freq  ^{:doc "frequency of english, key are encoded into hexadecimal"}
  (reduce
   (fn [m [k v]] (assoc m (int k) v))
   {}
   frequency))

(m/fact
  (->> (crypto.util/range \a \z)
       (map (comp byte-freq int)))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07]
    (->> (crypto.util/range \A \Z)
         (map (comp byte-freq int)))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07])

(defn- compute-freq
  "Compute the frequency of byte inside a byte string."
  [s]
  (let [l (count s)]
    (->> s
         frequencies
         (reduce
          (fn [m [k v]]
            (assoc m k (/ v l)))
          {}))))

(m/fact
  (compute-freq (crypto.ascii/to-bytes "hello")) => (m/just {111 1/5, 108 2/5, 101 1/5, 104 1/5}))

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
       (diff-freq byte-freq)
       sum-diff-map))

(m/fact
  (compute-diff (crypto.ascii/to-bytes "hello")) => 199.07999998509888)

(defn frequency-equals
  "Compute the frequency of same characters between 2 sequence."
  [b0 b1]
  (let [l (max (count b0) (count b1))
        s (->> (map (fn [c0 c1] (if (= c0 c1) 1 0)) b0 b1)
               (apply +))]
    (/ s l)))

(m/fact
  (frequency-equals "123 hello" "123 salut") => 5/9
  (frequency-equals "123 hello" "123 hello") => 1
  (frequency-equals "123 hello" "123")       => 1/3
  (frequency-equals "123"       "123 hello") => 1/3)
