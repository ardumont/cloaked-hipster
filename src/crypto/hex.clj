(ns crypto.hex
  "Hexadecimal namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto.byte    :as byte]
            [crypto.ascii   :as ascii]))

(defn- byte-to-hex
  "byte to hexadecimal"
  [b]
  (-> b
      Integer/toHexString))

(m/fact
  (map byte-to-hex (range 0 20)) => ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f" "10" "11" "12" "13"])

(defn- encode-bytes
  "Encode a bytes sequence into hex string"
  [b]
  (->> b
       (map byte-to-hex)
       (s/join "")))

(m/fact
  (encode-bytes (ascii/to-bytes "haskell rocks!"))                                   => "6861736b656c6c20726f636b7321"
  (encode-bytes (ascii/to-bytes "I'm killing your brain like a poisonous mushroom")) => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

(defmulti encode "Encode in hexadecimal a string or a sequence of bytes"
  string?)

;; byte sequence into hex string
(defmethod encode false [b] (encode-bytes b))

(m/fact
  (encode (ascii/to-bytes "haskell rocks!"))                                   => "6861736b656c6c20726f636b7321"
  (encode (ascii/to-bytes "I'm killing your brain like a poisonous mushroom")) => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

;; String to hexadecimal string
(defmethod encode true
  [s]
  (->> s
       byte/encode
       encode-bytes))

(m/fact
  (encode "haskell rocks!")                                   => "6861736b656c6c20726f636b7321"
  (encode "I'm killing your brain like a poisonous mushroom") => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

(defn- hex-to-byte
  "Hexadecimal sequence into byte"
  [h]
  (-> (s/join "" h)
      (Integer/parseInt 16)))

(m/fact
  (hex-to-byte '(\0))    => 0
  (hex-to-byte '(\0 \1)) => 1
  (hex-to-byte '(\f))    => 15
  (hex-to-byte '(\f \f)) => 255)

(defn to-bytes
  [s]
  (->> s
       (partition 2)
       (map hex-to-byte)))

(m/fact
  (to-bytes "6861736b656c6c") => [104 97 115 107 101 108 108])

(defn decode
  "Hexadecimal string to string"
  [s]
  (->> s
       to-bytes
       byte/decode
       (s/join "")))

(m/fact
  (decode "686974207468652062756c6c277320657965")                                                             => "hit the bull's eye"
  (decode "746865206b696420646f6e277420706c6179")                                                             => "the kid don't play"
  (decode "6861736b656c6c20726f636b7321")                                                                     => "haskell rocks!"
  (decode "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") => "I'm killing your brain like a poisonous mushroom")
