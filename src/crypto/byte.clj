(ns crypto.byte
  "Bytes manipulation namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto.binary  :as binary]
            [clojure.string :as string]))

(defn >signed-byte
  "Small utility to coerce an unsigned byte into a signed byte (jvm expects it)."
  [x]
  (.byteValue x))

(m/fact
  (>signed-byte 0)   => 0
  (>signed-byte 128) => -128
  (>signed-byte 255) => -1)

(defn >bytes-array
  "Given a seq of int, compute the byte[] equivalent."
  [byts]
  (->> byts
       (map >signed-byte)
       byte-array
       bytes))

(m/fact
  (String. (>bytes-array [48 49 50]))  => "012")

(defn >ascii
  "byte[] to string"
  [b]
  (->> b
       (map char)
       (s/join "")))

(m/fact
  (>ascii [99 108 111 106 117 114 101 32 114 111 99 107 115 33]) => "clojure rocks!")

(def >bits ^{:doc "Transform a byte sequence into a 8-bits word binary sequence."}
  (partial mapcat binary/>8bits))

(m/fact
  (>bits [0 1 2])    => [0 0 0 0 0 0 0 0,
                           0 0 0 0 0 0 0 1,
                           0 0 0 0 0 0 1 0]
  (>bits [97 98 99]) => [0 1 1 0 0 0 0 1,
                           0 1 1 0 0 0 1 0,
                           0 1 1 0 0 0 1 1]
  (>bits [104 97])   => [0 1 1 0 1 0 0 0,
                           0 1 1 0 0 0 0 1])

(defmulti >hex "byte to hexadecimal, beware, we want 2 characters even for number strictly under 16"
  (fn [b]
    (let [num? (number? b)]
      (cond (and num? (< b 16)) :num-special
            num?                :num
            :else               :sequence))))

(defmethod >hex :num-special  [b] (format "0%x" b));; we prefix the numbers strictly under 16 so that we have a 2 string representation
(defmethod >hex :num          [b] (format "%x" b))
(defmethod >hex :default      [b] (->> b
                                         (map >hex)
                                         (s/join "")))

(m/fact
  (map >hex (range 0 20)) => ["00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f" "10" "11" "12" "13"]
  (>hex (range 0 20))     => "000102030405060708090a0b0c0d0e0f10111213")

(defn rand-bytes
  "Generate a random list of unsigned bytes."
  [size]
  (let [rand (java.security.SecureRandom/getInstance "SHA1PRNG")
        buffer (make-array Byte/TYPE size)]
    (.nextBytes rand buffer)
    (map #(+ 128 %) buffer)))

(m/fact
  (apply max (rand-bytes 100)) => #(<= % 255)
  (rand-bytes 100)             => #(every? true? (map (fn [e] (<= 0 e)) %)))
