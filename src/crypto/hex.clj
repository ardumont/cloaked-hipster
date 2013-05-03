(ns crypto.hex
  "Hexadecimal namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto.byte    :as b]))

(defn- byte-to-hex
  "byte to hexadecimal"
  [b]
  (-> b
      Integer/toHexString))

(m/fact
  (map byte-to-hex (range 0 20)) => ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f" "10" "11" "12" "13"])

(defn encode
  "String to hexadecimal string"
  [s]
  (->> s
       b/encode
       (map byte-to-hex)
       (s/join "")))

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

(defn decode
  "Hexadecimal string to string"
  [s]
  (->> s
       (partition 2)
       (map hex-to-byte)
       b/decode
       (s/join "")))

(m/fact
  (decode "6861736b656c6c20726f636b7321")                                                                     => "haskell rocks!"
  (decode "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") => "I'm killing your brain like a poisonous mushroom")
