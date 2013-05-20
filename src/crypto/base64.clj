(ns crypto.base64
  "encode and decode a string in base64"
  (:require [midje.sweet    :as m]
            [crypto
             [dico          :as dico]
             [byte          :as byte]
             [binary        :as binary]]
            [clojure.string :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; decoding

(def decode-b64char ^{:doc "Decode a 8-bit base64 representation into a 6-bits representation."}
  (comp binary/>6bits dico/base64-dec))

(m/fact
  (decode-b64char \a) => [0 1 1 0 1 0]
  (decode-b64char \b) => [0 1 1 0 1 1])

(defn decode4
  "Decode 4 characters into 3 bytes (24 bits)"
  [s]
  (->> s
       (take-while #(not= \= %))
       (mapcat decode-b64char)))

(m/fact
  (decode4 "ab==") => [0 1 1 0 1 0,
                       0 1 1 0 1 1]
  (decode4 "ba==") => [0 1 1 0 1 1,
                       0 1 1 0 1 0])
(m/fact
  (decode4 "aab=") => [0 1 1 0 1 0,
                       0 1 1 0 1 0,
                       0 1 1 0 1 1]
  (decode4 "abb=") => [0 1 1 0 1 0,
                       0 1 1 0 1 1,
                       0 1 1 0 1 1])
(m/fact
  (decode4 "aaaa") => [0 1 1 0 1 0,
                       0 1 1 0 1 0,
                       0 1 1 0 1 0,
                       0 1 1 0 1 0]
  (decode4 "abaa") => [0 1 1 0 1 0,
                       0 1 1 0 1 1,
                       0 1 1 0 1 0,
                       0 1 1 0 1 0]
  (decode4 "aaba") => [0 1 1 0 1 0,
                       0 1 1 0 1 0,
                       0 1 1 0 1 1,
                       0 1 1 0 1 0]
  (decode4 "aaab") => [0 1 1 0 1 0,
                       0 1 1 0 1 0,
                       0 1 1 0 1 0,
                       0 1 1 0 1 1])

(defn >bits
  "Decode base64 message"
  [s]
  (->> s
       (partition 4)
       (mapcat decode4)
       (partition 8)))

(m/fact
  (>bits "YW55IGNhcm5hbCBwbGVhcw==") => '((0 1 1 0 0 0 0 1) (0 1 1 0 1 1 1 0) (0 1 1 1 1 0 0 1) (0 0 1 0 0 0 0 0) (0 1 1 0 0 0 1 1) (0 1 1 0 0 0 0 1) (0 1 1 1 0 0 1 0) (0 1 1 0 1 1 1 0) (0 1 1 0 0 0 0 1) (0 1 1 0 1 1 0 0) (0 0 1 0 0 0 0 0) (0 1 1 1 0 0 0 0) (0 1 1 0 1 1 0 0) (0 1 1 0 0 1 0 1) (0 1 1 0 0 0 0 1) (0 1 1 1 0 0 1 1))
  (>bits "YW55IGNhcm5hbCBwbGVhc3U=") => '((0 1 1 0 0 0 0 1) (0 1 1 0 1 1 1 0) (0 1 1 1 1 0 0 1) (0 0 1 0 0 0 0 0) (0 1 1 0 0 0 1 1) (0 1 1 0 0 0 0 1) (0 1 1 1 0 0 1 0) (0 1 1 0 1 1 1 0) (0 1 1 0 0 0 0 1) (0 1 1 0 1 1 0 0) (0 0 1 0 0 0 0 0) (0 1 1 1 0 0 0 0) (0 1 1 0 1 1 0 0) (0 1 1 0 0 1 0 1) (0 1 1 0 0 0 0 1) (0 1 1 1 0 0 1 1) (0 1 1 1 0 1 0 1))
  (>bits "YW55IGNhcm5hbCBwbGVhc3Vy") => '((0 1 1 0 0 0 0 1) (0 1 1 0 1 1 1 0) (0 1 1 1 1 0 0 1) (0 0 1 0 0 0 0 0) (0 1 1 0 0 0 1 1) (0 1 1 0 0 0 0 1) (0 1 1 1 0 0 1 0) (0 1 1 0 1 1 1 0) (0 1 1 0 0 0 0 1) (0 1 1 0 1 1 0 0) (0 0 1 0 0 0 0 0) (0 1 1 1 0 0 0 0) (0 1 1 0 1 1 0 0) (0 1 1 0 0 1 0 1) (0 1 1 0 0 0 0 1) (0 1 1 1 0 0 1 1) (0 1 1 1 0 1 0 1) (0 1 1 1 0 0 1 0)))

(defn >ascii
  "Decode base64 message"
  [s]
  (->> s
       >bits
       (map binary/>char)  ;; converted back into char
       (s/join "")))

(m/fact
  (>ascii "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
  =>  "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."

  (>ascii "YW55IGNhcm5hbCBwbGVhcw==") => "any carnal pleas"
  (>ascii "YW55IGNhcm5hbCBwbGVhc3U=") => "any carnal pleasu"
  (>ascii "YW55IGNhcm5hbCBwbGVhc3Vy") => "any carnal pleasur")

(defn >bytes
  "Decode base64 message"
  [s]
  (->> s
       >bits
       (map binary/>bytes)))

(m/fact
  (>bytes "YW55IGNhcm5hbCBwbGVhcw==") => '(97 110 121 32 99 97 114 110 97 108 32 112 108 101 97 115)
  (>bytes "YW55IGNhcm5hbCBwbGVhc3U=") => '(97 110 121 32 99 97 114 110 97 108 32 112 108 101 97 115 117)
  (>bytes "YW55IGNhcm5hbCBwbGVhc3Vy") => '(97 110 121 32 99 97 114 110 97 108 32 112 108 101 97 115 117 114))

(defn >hex
  "Decode a base64 encoded string into an hexadecimal string"
  [s]
  (-> s
      >bytes
      byte/>hex))

(m/fact
  (>hex "bWF5IHRoZSByZXBsIGJlIHdpdGggeW91") => "6d617920746865207265706c206265207769746820796f75")
