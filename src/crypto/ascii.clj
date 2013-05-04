(ns crypto.ascii
  "ascii manipulation"
  (:use [midje.sweet :only [fact]])
  (:require [midje.sweet   :as m]
            [crypto.binary :as binary]))

(defn to-bytes
  "ascii to bytes"
  [s]
  (->> s
       (map int)))

(m/fact
  (to-bytes "abcdef") => [97 98 99 100 101 102]
  (to-bytes "123")    => [49 50 51])

(def to-bits ^{:private true
                 :doc "Convert a char into a 8-bits sequence"}
  (comp binary/to-8bits int))

(m/fact
  (to-bits \a) => [0 1 1 0 0 0 0 1])
