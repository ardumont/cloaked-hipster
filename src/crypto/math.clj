(ns crypto.math
  "Some math utility functions. From old clojure contrib."
  (:require [midje.sweet                :as m]
            [clojure.math.numeric-tower :as math]))

(defn lgcd
  "Compute the gcd from a list. Return such gcd if compatible or else return nil"
  [l]
  (if (or (= [] l) (= 1 (count l)))
    (first l)
    (let [r      (rest l)
          allgcd (map (fn [n0 n1] (math/gcd n0 n1)) l r)]
      (recur allgcd))))

(m/fact
  (lgcd [])                        => nil
  (lgcd [12 18 24 30 36 42 48])    => 6
  (lgcd [12 14 18 24 30 36 42 48]) => 2
  (lgcd [1 5 3])                   => 1
  (lgcd [10])                      => 10)
