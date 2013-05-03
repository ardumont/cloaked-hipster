(ns crypto.base64
  "encode and decode a string in base64"
  (:use [midje.sweet :only [fact]])
  (:require [midje.sweet    :as m]
            [crypto.dico    :as d]
            [crypto.binary  :as b]
            [crypto.byte    :as byte]
            [crypto.ascii   :as ascii]
            [clojure.string :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; encoding

;; Given a partition of 24 bits, compute the complement [partition of multiple 6 bits, list of complement = char]
(defmulti comp24 count)

;; complement 4 bits to be able to have 2 bytes (12 bits) and we complements with 2 = chars
(defmethod comp24 8 [b] [(b/comp-after 12 b)
                         [\= \=]])

(m/fact
  (comp24 [1 1 1 1 1 1 1 1]) => [[1 1 1 1 1 1,
                                  1 1 0 0 0 0]
                                 [\= \=]])

;; complement 2 bits to be able to have 3 bytes (18 bits) and we complements with 1 = char
(defmethod comp24 16 [b] [(b/comp-after 18 b)
                          [\=]])

(m/fact
  (comp24 [1 1 1 1 1 1 1 1, 0 0 0 0 0 0 1 1]) => [[1 1 1 1 1 1,
                                                   1 1 0 0 0 0,
                                                   0 0 1 1 0 0]
                                                  [\=]])

;; chunk of 24 remains the same without any complement
(defmethod comp24 :default [b] [b []])

(m/fact
  (comp24 [1 1 1 1 1 1 1 1, 0 0 0 0 0 0 1 1, 1 1 1 1 1 1 1 1]) => [[1 1 1 1 1 1,
                                                                    1 1 0 0 0 0,
                                                                    0 0 1 1 1 1,
                                                                    1 1 1 1 1 1]
                                                                   []])

(defn- to-base64
  "Given a 8 or 16 or 24-bits chunk, compute the bits sequence into base64."
  [b]
  (let [[part complement] (comp24 b)
        p24               (->> part
                               (partition 6)
                               (map (comp d/base64 b/to-bytes)))]
    (concat p24 complement)))

(m/fact
  (to-base64 [1 1 1 1 1 1, 1 1 0 0 0 0])                           => [\/ \w]
  (to-base64 [1 1 1 1 1 1, 1 1 0 0 0 0, 0 0 1 1 0 0])              => [\/ \w \M]
  (to-base64 [1 1 1 1 1 1, 1 1 0 0 0 0, 0 0 1 1 1 1, 1 1 1 1 1 1]) => [\/ \w \P \/])

(defn- encode-bytes
  "Encode bytes sequence into base64"
  [b]
  (->> b
       byte/to-bits        ;; into 8-bits word binary sequence
       (partition-all 24)  ;; 24-bits chunks
       (mapcat to-base64)  ;; deal with the last chunk of bits (which can be of size 8, 16 or 24)
       (s/join "")))

(m/fact
  (encode-bytes (ascii/to-bytes "any carnal pleas"))   => "YW55IGNhcm5hbCBwbGVhcw=="
  (encode-bytes (ascii/to-bytes "any carnal pleasu"))  => "YW55IGNhcm5hbCBwbGVhc3U="
  (encode-bytes (ascii/to-bytes "any carnal pleasur")) => "YW55IGNhcm5hbCBwbGVhc3Vy")

;; Encode into base64
(defmulti encode "Encode ascii string or bytes sequence into base64"
  string?)

(defmethod encode false
  [b]
  (encode-bytes b))

(m/fact
  (encode (ascii/to-bytes "any carnal pleas"))   => "YW55IGNhcm5hbCBwbGVhcw=="
  (encode (ascii/to-bytes "any carnal pleasu"))  => "YW55IGNhcm5hbCBwbGVhc3U="
  (encode (ascii/to-bytes "any carnal pleasur")) => "YW55IGNhcm5hbCBwbGVhc3Vy")

(defmethod encode true
  [s]
  (->> s
       ascii/to-bytes
       encode-bytes))

(m/fact
  (encode "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
  => "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; decoding

(def decode-b64char ^{:doc "Decode a 8-bit base64 representation into a 6-bits representation."}
  (comp b/to-6bits d/base64-dec))

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

(defn decode
  "Decode base64 message"
  [s]
  (->> s
       (partition 4)         ;; 4 words (32 bits)
       (mapcat decode4)      ;; decoded into 3 bytes (24 bits)
       (partition 8)         ;; spliced into byte word (8 bits)
       (map ascii/bits2char) ;; converted back into char
       (s/join "")))         ;; then joined to form a string

(m/fact
  (decode "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
  =>  "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."

  (decode "YW55IGNhcm5hbCBwbGVhcw==") => "any carnal pleas"
  (decode "YW55IGNhcm5hbCBwbGVhc3U=") => "any carnal pleasu"
  (decode "YW55IGNhcm5hbCBwbGVhc3Vy") => "any carnal pleasur")
