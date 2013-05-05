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
       byte-array))

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

(def to-ascii decode)

(m/fact
  (decode [99 108 111 106 117 114 101 32 114 111 99 107 115 33]) => "clojure rocks!"
  (to-ascii [99 108 111 106 117 114 101 32 114 111 99 107 115 33]) => "clojure rocks!")

(def to-bits ^{:doc "Transform a byte sequence into a 8-bits word binary sequence."}
  (partial mapcat binary/to-8bits))

(m/fact
  (to-bits [0 1 2])    => [0 0 0 0 0 0 0 0,
                           0 0 0 0 0 0 0 1,
                           0 0 0 0 0 0 1 0]
  (to-bits [97 98 99]) => [0 1 1 0 0 0 0 1,
                           0 1 1 0 0 0 1 0,
                           0 1 1 0 0 0 1 1]
  (to-bits [104 97])   => [0 1 1 0 1 0 0 0,
                           0 1 1 0 0 0 0 1])

(defmulti to-hex "byte to hexadecimal, beware, we want 2 characters even for number strictly under 16"
  (fn [b]
    (let [num? (number? b)]
      (cond (and num? (< b 16)) :num-special
            num?                :num
            :else               :sequence))))

(defmethod to-hex :num-special  [b] (format "0%x" b));; we prefix the numbers strictly under 16 so that we have a 2 string representation
(defmethod to-hex :num          [b] (format "%x" b))
(defmethod to-hex :default      [b] (->> b
                                         (map to-hex)
                                         (s/join "")))

(m/fact
  (map to-hex (range 0 20)) => ["00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f" "10" "11" "12" "13"]
  (to-hex (range 0 20))     => "000102030405060708090a0b0c0d0e0f10111213")
