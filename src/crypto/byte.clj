(ns crypto.byte
  "Bytes manipulation namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto.binary  :as binary]
            [clojure.string :as string]))

(defn to-signed-byte
  "Small utility to coerce an unsigned byte into a signed byte (jvm expects it)."
  [x]
  (.byteValue x))

(m/fact
  (to-signed-byte 0)   => 0
  (to-signed-byte 128) => -128
  (to-signed-byte 255) => -1)

(defn to-bytes-array
  "Given a seq of int, compute the byte[] equivalent."
  [byts]
  (->> byts
       (map to-signed-byte)
       byte-array
       bytes))

(m/fact
  (String. (to-bytes-array [48 49 50]))  => "012")

(defn to-ascii
  "byte[] to string"
  [b]
  (->> b
       (map char)
       (s/join "")))

(m/fact
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
