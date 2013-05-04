(ns crypto.distance
  "A distance namespace"
  (:require [midje.sweet   :as m]
            [crypto.ascii  :as ascii]
            [crypto.binary :as binary]))

(defn hamming
  "hamming distance between 2 equal length strings."
  [s0 s1]
  {:pre [(= (count s0) (count s1))]}                 ;; this version does not support string with different sizes
  (->> [s0 s1]
       (map (partial mapcat ascii/to-bits))          ;; transform into 2 sequences of bits
       (apply map (fn [b0 b1] (if (not= b0 b1) 1 0))) ;; compare bit to bit
       (apply +)))                                   ;; compute the sum

(m/fact
  (hamming "this" "is a test for exception")  => (m/throws AssertionError "Assert failed: (= (count s0) (count s1))")
  (hamming "this is a test" "wokka wokka!!!") => 37)
