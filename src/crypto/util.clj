(ns crypto.util
  "A utility namespace"
  (:require [midje.sweet :as m]))

(defn range
  [cs ce]
  (->> [(int cs) (inc (int ce))]
       (apply clojure.core/range)))

(m/fact
  (range \a \c) => [97 98 99])

(defn range-char
  "range for characters. Beware, contrary to the clojure.core/range function, this one is inclusive."
  [cs ce]
  (->> (range cs ce)
       (map char)))

(m/fact
  (range-char \a \c) => [\a \b \c])
