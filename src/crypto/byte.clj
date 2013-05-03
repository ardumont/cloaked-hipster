(ns crypto.byte
  "Bytes manipulation namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]))

(defn encode
  "string to byte[]"
  [s]
  (->> s
       (map (comp byte int))
       byte-array
       bytes))

(m/fact
  (-> "clojure rocks!" to-bytes String.) => "clojure rocks!")

(defn get-byte
  "Given a byte-array and an index, return the byte at the index i"
  [the-bytes i]
  (aget the-bytes i))

(m/fact
  (let [s "clojure rocks!"
        a (to-bytes s)
        l (.length s)]
    (map #(get-byte a %) (range 0 l))) => [99 108 111 106 117 114 101 32 114 111 99 107 115 33])

(defn decode
  "byte[] to string"
  [b]
  (->> b
       (map char)
       (s/join "")))

(m/fact
  (decode [99 108 111 106 117 114 101 32 114 111 99 107 115 33]) => "clojure rocks!")
