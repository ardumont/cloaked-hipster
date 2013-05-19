(ns crypto.ascii
  "ascii manipulation"
  (:require [midje.sweet   :as m]
            [crypto
             [binary :as binary]
             [byte   :as byte]
             [ascii  :as ascii]]))

(defn >bytes
  "ascii to bytes"
  [s]
  (->> s
       (map int)))

(m/fact
  (>bytes "abcdef") => [97 98 99 100 101 102]
  (>bytes "123")    => [49 50 51])

(defmulti >bits "Convert a char into a 8-bits sequence"
  string?)

(defmethod >bits false [c] ((comp binary/>8bits int) c))

(m/fact
  (>bits \a) => [0 1 1 0 0 0 0 1]
  (>bits \t) => [0 1 1 1 0 1 0 0]
  (>bits \r) => [0 1 1 1 0 0 1 0]
  (>bits \o) => [0 1 1 0 1 1 1 1]
  (>bits \s) => [0 1 1 1 0 0 1 1]
  (>bits \e) => [0 1 1 0 0 1 0 1]
  (>bits \n) => [0 1 1 0 1 1 1 0]
  (>bits \d) => [0 1 1 0 0 1 0 0])

(defmethod >bits true [s]
  (->> s
       (mapcat >bits)
       (partition 8)))

(m/fact
  (>bits "roses") => [[0 1 1 1 0 0 1 0]
                        [0 1 1 0 1 1 1 1]
                        [0 1 1 1 0 0 1 1]
                        [0 1 1 0 0 1 0 1]
                        [0 1 1 1 0 0 1 1]]
  (>bits "toned") => [[0 1 1 1 0 1 0 0]
                        [0 1 1 0 1 1 1 1]
                        [0 1 1 0 1 1 1 0]
                        [0 1 1 0 0 1 0 1]
                        [0 1 1 0 0 1 0 0]])

(defn >b64
  [s]
  (->> s
       ascii/>bytes
       byte/>b64))

(m/fact
  (>b64 "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
  => "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
