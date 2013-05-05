(ns crypto.c1
  "1. Convert hex to base64 and back.

The string:
  49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d
should produce:
  SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t
Now use this code everywhere for the rest of the exercises. Here's a simple rule of thumb:
  Always operate on raw bytes, never on encoded strings. Only use hex and base64 for pretty-printing."
  (:require [midje.sweet   :as m]
            [crypto.byte   :as b]
            [crypto.hex    :as hex]
            [crypto.base64 :as b64]))

(defn encode "Encode an hexadecimal string into base64"
  [s]
  (-> s
      hex/to-bytes
      b64/encode))

(m/fact
  (encode "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
  => "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

(defn decode
  "Decode a base64 encoded string into an hexadecimal string"
  [s]
  (-> s
      b64/decode
      hex/encode))

(m/fact
  (decode "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  => "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")