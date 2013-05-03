(ns crypto.hex
  "Hexadecimal namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]
            [crypto.byte    :as b]))

(defn encode
  "String to hexadecimal string"
  [s]
  (->> s
       b/encode
       (map #(-> % Integer/toHexString))
       (s/join "")))

(m/fact
  (encode "haskell rocks!")                                   => "6861736b656c6c20726f636b7321"
  (encode "I'm killing your brain like a poisonous mushroom") => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

(defn decode
  "Hexadecimal string to string"
  [s]
  (->> s
       (partition 2)
       (map (fn [c] (-> (s/join "" c)
                       (Integer/parseInt 16))))
       b/decode
       (s/join "")))

(m/fact
  (decode "6861736b656c6c20726f636b7321")                                                                     => "haskell rocks!"
  (decode "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") => "I'm killing your brain like a poisonous mushroom")
