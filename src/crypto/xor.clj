(ns crypto.xor
  "xor computations"
  (:require [midje.sweet :as m]))

(defn bitxor
  "Apply bit-xor to the seq using key as the key"
  [seq key]
  (map bit-xor seq key))

(m/fact
  (bitxor [0 0 0 0 1 1 1 1] [0 0 0 0 1 1 1 1])         => [0 0 0 0 0 0 0 0]
  (bitxor [0 0 0 0 1 1 1 1] [1 1 1 1 1 1 1 1])         => [1 1 1 1 0 0 0 0]
  (bitxor [0 0 0 0 1 1 1 1] [1 1 1 1 0 0 0 0])         => [1 1 1 1 1 1 1 1]
  (bitxor [1 1 1 1 1 1 1 1] [1 1 1 1 0 0 0 0])         => [0 0 0 0 1 1 1 1]
  (apply bitxor [[0 0 0 0 1 1 1 1] [1 1 1 1 1 1 1 1]]) => [1 1 1 1 0 0 0 0])
