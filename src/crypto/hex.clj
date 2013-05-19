(ns crypto.hex
  "Hexadecimal namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto
             [byte    :as byte]
             [ascii   :as ascii]]))

(defn- hex->byte
  "Hexadecimal sequence into byte"
  [h]
  (-> (s/join "" h)
      (Integer/parseInt 16)))

(m/fact
  (hex->byte '(\0))    => 0
  (hex->byte '(\0 \1)) => 1
  (hex->byte '(\f))    => 15
  (hex->byte '(\f \f)) => 255)

(defn >bytes
  [s]
  (->> s
       (partition 2)
       (map hex->byte)))

(m/fact
  (>bytes "6861736b656c6c") => [104 97 115 107 101 108 108]
  (>bytes "6861736b656c6c") => [104 97 115 107 101 108 108])

(defn >ascii
  "Hexadecimal string to string"
  [s]
  (->> s
       >bytes
       byte/>ascii
       (s/join "")))

(m/fact
  (>ascii "686974207468652062756c6c277320657965")                                                             => "hit the bull's eye"
  (>ascii "746865206b696420646f6e277420706c6179")                                                             => "the kid don't play"
  (>ascii "6861736b656c6c20726f636b7321")                                                                     => "haskell rocks!"
  (>ascii "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") => "I'm killing your brain like a poisonous mushroom")

(def >bits ^{:private true
                   :doc "hexadecimal to bits"}
  (comp byte/>bits >bytes))

(m/fact
  (>bits "aaff") => [1 0 1 0 1 0 1 0,
                       1 1 1 1 1 1 1 1])

(defn >b64
  "Encode an hexadecimal string into base64"
  [s]
  (-> s
      >bytes
      byte/>b64))

(m/fact
  (>b64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  => "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
