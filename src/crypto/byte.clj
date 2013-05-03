(ns crypto.byte
  "Bytes manipulation namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto.binary  :as binary]))

(defn encode
  "string to byte[]"
  [s]
  (->> s
       (map (comp byte int))
       byte-array
       bytes))

(m/fact
  (-> "clojure rocks!" encode String.) => "clojure rocks!")

(defn get-byte
  "Given a byte-array and an index, return the byte at the index i"
  [abytes i]
  (aget abytes i))

(m/fact
  (let [s "clojure rocks!"
        a (encode s)
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

(def to-bits ^{:doc "Transform a byte sequence into a 8-bits word binary sequence."}
  (partial mapcat binary/to-8bits))

(m/fact
  (to-bits [97 98 99]) => [0 1 1 0 0 0 0 1,
                           0 1 1 0 0 0 1 0,
                           0 1 1 0 0 0 1 1]
  (to-bits [104 97])  => [0 1 1 0 1 0 0 0,
                          0 1 1 0 0 0 0 1])
