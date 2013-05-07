(ns crypto.distance
  "A distance namespace"
  (:require [midje.sweet   :as m]
            [crypto.ascii  :as ascii]
            [crypto.binary :as binary]
            [crypto.byte   :as byte]))

(defn hamming-bit
  "hamming distance on bits sequence"
  [by0 by1]
  (->> [by0 by1]
       (apply map (fn [b0 b1] (if (= b0 b1) 0 1)))
       (apply +)))

(let [[by0 by1] [[0 1 1 0 1 0 0 0 0 1 1 0 0 1 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 1 0 0 0 0 0 0 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 1 0 0 1 0 0 1 1 0 0 1 0 0]
                 [0 1 1 0 1 0 0 0 0 1 1 0 0 1 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 1 1 1 0 1 0 1 0 1 1 0 0 1 0 0 0 1 1 0 0 1 0 1]]
      m {true 0}]
  (->> [by0 by1]
       (apply map (fn [b0 b1] (m (= b0 b1) 1)))
       (apply +)))

(m/fact
  (hamming-bit [1 0 0] [0 0 0]) => 1
  (hamming-bit [0 0 0] [0 0 0]) => 0
  (hamming-bit [0 0 0] [1 1 1]) => 3
  (hamming-bit [1 0 1 1 1 0 1] [1 0 0 1 0 0 1]) => 2
  (hamming-bit [0 1 1 0 1 0 0 0 0 1 1 0 0 1 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 1 0 0 0 0 0 0 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 1 0 0 1 0 0 1 1 0 0 1 0 0]
               [0 1 1 0 1 0 0 0 0 1 1 0 0 1 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 0 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 1 1 1 0 1 0 1 0 1 1 0 0 1 0 0 0 1 1 0 0 1 0 1]) => 10
  (hamming-bit [0 1 1 1 0 0 1 0 0 1 1 0 1 1 1 1 0 1 1 1 0 0 1 1 0 1 1 0 0 1 0 1 0 1 1 1 0 0 1 1]
               [0 1 1 1 0 1 0 0 0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 0 1 1 0 0 1 0 1 0 1 1 0 0 1 0 0]) => 10)

(defmulti hamming "Compute the hamming distance between to equal strings or between 2 equal bytes sequence."
  (fn [v0 v1]
    (when (or (-> v0 string?)
              (-> v0 first char?)) :str)))

;; deal with byte
(defmethod hamming :default
  [by0 by1]
  {:pre [(= (count by0) (count by1))]}
  (->> [by0 by1]
       (map byte/to-bits)    ;; transform into 2 8-bits sequence
       (apply hamming-bit))) ;; compare bit to bit

(m/fact
  (hamming [0 0 1] [1 0 0])                                                     => 2
  (hamming (ascii/to-bytes "this") (ascii/to-bytes "is a test for exception"))  => (m/throws AssertionError "Assert failed: (= (count by0) (count by1))")
  (hamming (ascii/to-bytes "this is a test") (ascii/to-bytes "wokka wokka!!!")) => 37
  (hamming (ascii/to-bytes "this") (ascii/to-bytes "that"))                     => 4
  (hamming (ascii/to-bytes "dude") (ascii/to-bytes "word"))                     => 10
  (hamming (ascii/to-bytes "toned") (ascii/to-bytes "roses"))                   => 10)

(defmethod hamming :str
  [s0 s1]
  {:pre [(= (count s0) (count s1))]}        ;; this version does not support string with different sizes
  (->> [s0 s1]
       (map (partial mapcat ascii/to-bits)) ;; transform into 2 8-bits sequences
       (apply hamming-bit)))                ;; compare bit to bit

(m/fact
  (hamming [\t \h \i \s] [\t \h \a \t])       => 4
  (hamming "this" "is a test for exception")  => (m/throws AssertionError "Assert failed: (= (count s0) (count s1))")
  (hamming "this is a test" "wokka wokka!!!") => 37)
