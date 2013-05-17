(ns crypto.base64
  "encode and decode a string in base64"
  (:require [midje.sweet    :as m]
            [crypto
             [dico          :as d]
             [binary        :as binary]
             [byte          :as byte]
             [ascii         :as ascii]]
            [clojure.string :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; encoding

;; Given a partition of 24 bits, compute the complement [partition of multiple 6 bits, list of complement = char]
(defmulti comp24 count)

;; complement 4 bits to be able to have 2 bytes (12 bits) and we complements with 2 = chars
(defmethod comp24 8 [b] [(binary/comp-after 12 b)
                         [\= \=]])

(m/fact
  (comp24 [1 1 1 1 1 1 1 1]) => [[1 1 1 1 1 1,
                                  1 1 0 0 0 0]
                                 [\= \=]])

;; complement 2 bits to be able to have 3 bytes (18 bits) and we complements with 1 = char
(defmethod comp24 16 [b] [(binary/comp-after 18 b)
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

(defn- >base64
  "Given a 8 or 16 or 24-bits chunk, compute the bits sequence into base64."
  [b]
  (let [[part complement] (comp24 b)
        p24               (->> part
                               (partition 6)
                               (map (comp d/base64 binary/>bytes)))]
    (concat p24 complement)))

(m/fact
  (>base64 [1 1 1 1 1 1, 1 1 0 0 0 0])                           => [\/ \w]
  (>base64 [1 1 1 1 1 1, 1 1 0 0 0 0, 0 0 1 1 0 0])              => [\/ \w \M]
  (>base64 [1 1 1 1 1 1, 1 1 0 0 0 0, 0 0 1 1 1 1, 1 1 1 1 1 1]) => [\/ \w \P \/])

(defn- encode-bytes
  "Encode bytes sequence into base64"
  [b]
  (->> b
       byte/>bits        ;; into 8-bits word binary sequence
       (partition-all 24)  ;; 24-bits chunks
       (mapcat >base64)  ;; deal with the last chunk of bits (which can be of size 8, 16 or 24)
       (s/join "")))

(m/fact
  (encode-bytes (ascii/>bytes "any carnal pleas"))   => "YW55IGNhcm5hbCBwbGVhcw=="
  (encode-bytes (ascii/>bytes "any carnal pleasu"))  => "YW55IGNhcm5hbCBwbGVhc3U="
  (encode-bytes (ascii/>bytes "any carnal pleasur")) => "YW55IGNhcm5hbCBwbGVhc3Vy")

;; Encode into base64
(defmulti encode "Encode ascii string or bytes sequence into base64"
  string?)

(defmethod encode false
  [b]
  (encode-bytes b))

(m/fact
  (encode (ascii/>bytes "any carnal pleas"))   => "YW55IGNhcm5hbCBwbGVhcw=="
  (encode (ascii/>bytes "any carnal pleasu"))  => "YW55IGNhcm5hbCBwbGVhc3U="
  (encode (ascii/>bytes "any carnal pleasur")) => "YW55IGNhcm5hbCBwbGVhc3Vy")

(defmethod encode true
  [s]
  (->> s
       ascii/>bytes
       encode-bytes))

(m/fact
  (encode "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
  => "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; decoding

(def decode-b64char ^{:doc "Decode a 8-bit base64 representation into a 6-bits representation."}
  (comp binary/>6bits d/base64-dec))

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
  (>bits "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
  =>  (ascii/>bits "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

  (>bits "YW55IGNhcm5hbCBwbGVhcw==") => (ascii/>bits "any carnal pleas")
  (>bits "YW55IGNhcm5hbCBwbGVhc3U=") => (ascii/>bits "any carnal pleasu")
  (>bits "YW55IGNhcm5hbCBwbGVhc3Vy") => (ascii/>bits "any carnal pleasur"))

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
  (>bytes "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
  => (ascii/>bytes "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

  (>bytes "YW55IGNhcm5hbCBwbGVhcw==") => (ascii/>bytes "any carnal pleas")
  (>bytes "YW55IGNhcm5hbCBwbGVhc3U=") => (ascii/>bytes "any carnal pleasu")
  (>bytes "YW55IGNhcm5hbCBwbGVhc3Vy") => (ascii/>bytes "any carnal pleasur"))

(defn >hex
  "Decode a base64 encoded string into an hexadecimal string"
  [s]
  (-> s
      >bytes
      byte/>hex))

(m/fact
  (>hex "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
