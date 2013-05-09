(ns crypto.util
  "A utility namespace"
  (:require [midje.sweet :as m]))

(defn rng
  [cs ce]
  (->> [(int cs) (inc (int ce))]
       (apply range)))

(m/fact
  (rng \a \c) => [97 98 99])

(defn rng-char
  "range for characters. Beware, contrary to the clojure.core/range function, this one is inclusive."
  [cs ce]
  (->> (rng cs ce)
       (map char)))

(m/fact
  (rng-char \a \c) => [\a \b \c])
