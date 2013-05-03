(ns crypto.hex
  "Hexadecimal namespace"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]))

(defn to-hex
  "String to hexadecimal string"
  [s]
  (->> s
       (map #(-> % int Integer/toHexString))
       (s/join "")))

(m/fact
  (to-hex "haskell rocks!")                                   => "6861736b656c6c20726f636b7321"
  (to-hex "I'm killing your brain like a poisonous mushroom") => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

(defn from-hex
  "Hexadecimal string to string"
  [s]
  (->> s
       (partition 2)
       (map (fn [c] (-> (s/join "" c)
                       (Integer/parseInt 16)
                       char)))
       (s/join "")))

(m/fact
  (from-hex "6861736b656c6c20726f636b7321")                                                                     => "haskell rocks!"
  (from-hex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") => "I'm killing your brain like a poisonous mushroom")
