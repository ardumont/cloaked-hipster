(ns crypto.math
  "Some math utility functions. From old clojure contrib."
  (:require [midje.sweet  :as m]
            [crypto.block :as block]))

(defn gcd "(gcd a b) returns the greatest common divisor of a and b" [a b]
  (if (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "gcd requires two integers"))
    (loop [a (Math/abs a) b (Math/abs b)]
      (if (zero? b) a,
          (recur b (mod a b))))))

(m/fact
  (gcd 7 42)  => 7
  (gcd 21 42) => 21
  (gcd 7 2)   => 1)

(defn lcm
  "(lcm a b) returns the least common multiple of a and b"
  [a b]
  (when (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "lcm requires two integers")))
  (cond (zero? a) 0
        (zero? b) 0
        :else (Math/abs (* b (quot a (gcd a b))))))

(defn lgcd
  "Compute the gcd from a list. Return such gcd if compatible or else return nil"
  [l]
  (if (= 1 (count l))
    (first l)
    (let [r      (rest l)
          allgcd (map (fn [n0 n1] (gcd n0 n1)) l r)]
      (recur allgcd))))

(m/fact
  (lgcd [12 18 24 30 36 42 48])    => 6
  (lgcd [12 14 18 24 30 36 42 48]) => 2
  (lgcd [1 5 3])                   => 1
  (lgcd [10])                      => 10)
