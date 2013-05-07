(ns crypto.ascii
  "ascii manipulation"
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
  (to-bits \a) => [0 1 1 0 0 0 0 1]
  (to-bits \t) => [0 1 1 1 0 1 0 0]
  (to-bits \r) => [0 1 1 1 0 0 1 0]
  (to-bits \o) => [0 1 1 0 1 1 1 1]
  (to-bits \s) => [0 1 1 1 0 0 1 1]
  (to-bits \e) => [0 1 1 0 0 1 0 1]
  (to-bits \n) => [0 1 1 0 1 1 1 0]
  (to-bits \d) => [0 1 1 0 0 1 0 0]

  (mapcat to-bits "roses") => [0 1 1 1 0 0 1 0, 0 1 1 0 1 1 1 1, 0 1 1 1 0 0 1 1, 0 1 1 0 0 1 0 1, 0 1 1 1 0 0 1 1]
  (mapcat to-bits "toned") => [0 1 1 1 0 1 0 0, 0 1 1 0 1 1 1 1, 0 1 1 0 1 1 1 0, 0 1 1 0 0 1 0 1, 0 1 1 0 0 1 0 0])
