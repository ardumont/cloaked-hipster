(ns crypto.binary
  "A binary namespace to deal with transformation into binary"
  (:require [midje.sweet :as m]))

;; bits sequence are read from the left to the right (big endian)
;; significant first, least significant
;; Exemple:
;; head - most significant [0 0 0 0 0 1 1 1] - least significant - tail
;; [0 0 0 0 0 1 1 1] reads 7

(defn- comp-bits-sequence
  "Complement a bits sequence by providing the policy through the complement-fn function."
  [n b complement-fn]
  (->> (iterate complement-fn b)
       (drop-while #(not= n (count %)))
       first))

(m/fact
  (comp-bits-sequence 8  [1 1 1]           (partial concat [0])) => [0 0 0 0 0 1 1 1]
  (comp-bits-sequence 8  [0 0 0 0 1 0 0 0] (partial concat [0])) => [0 0 0 0 1 0 0 0]
  (comp-bits-sequence 4  [1 1 1]           (partial concat [0])) => [0 1 1 1]
  (comp-bits-sequence 10 [0 0 0 0 1 0 0 0] (partial concat [0])) => [0 0 0 0 0 0 1 0 0 0]
  (comp-bits-sequence 8  [1 1 1]           #(concat % [0]))      => [1 1 1 0 0 0 0 0]
  (comp-bits-sequence 8  [0 0 0 0 1 0 0 0] #(concat % [0]))      => [0 0 0 0 1 0 0 0]
  (comp-bits-sequence 4  [1 1 1]           #(concat % [0]))      => [1 1 1 0]
  (comp-bits-sequence 10 [0 0 0 0 1 0 0 0] #(concat % [0]))      => [0 0 0 0 1 0 0 0 0 0])

(defn comp-before
  "Complement by the most significant side (head) a bits sequence to n bits (if necessary)."
  [n b]
  (comp-bits-sequence n b (partial concat [0])))

(m/fact
  (comp-before 8 [1 1 1])            => [0 0 0 0 0 1 1 1]
  (comp-before 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-before 4 [1 1 1])            => [0 1 1 1]
  (comp-before 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 0 0 1 0 0 0])

(defn comp-after
  "Complement by the least significant side (tail) a bits sequence to n bits (if necessary)."
  [n b]
  (comp-bits-sequence n b #(concat % [0])))

(m/fact
  (comp-after 10 [1 1 1 1 1 1 1 1]) => [1 1 1 1 1 1 1 1 0 0]
  (comp-after 8 [1 1 1])            => [1 1 1 0 0 0 0 0]
  (comp-after 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-after 4 [1 1 1])            => [1 1 1 0]
  (comp-after 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 1 0 0 0 0 0])

(defn- bin
  "Convert a byte into binary sequence (will create as much bits as needed)."
  [b]
  (if (= 0 b)
    []
    (concat (-> b (/ 2) int bin)
            [(mod b 2)])))

(m/fact
  (bin 97)  => [1 1 0 0 0 0 1]
  (bin 2)   => [1 0]
  (bin 255) => [1 1 1 1 1 1 1 1])

(defn- to-binary
  "Given a number, compute a function permitting the translation into a n-bits words binary sequence."
  [b]
  (comp (partial comp-before b) bin))

(m/fact
  ((to-binary 8) 97) => [0 1 1 0 0 0 0 1]
  ((to-binary 8) 2)  => [0 0 0 0 0 0 1 0])

(def to-8bits ^{:doc "Given a byte, compute its 8-bits word binary sequence."}
  (to-binary 8))

(m/fact
  (to-8bits 97) => [0 1 1 0 0 0 0 1]
  (to-8bits 2)  => [0 0 0 0 0 0 1 0])

(def to-6bits ^{:doc "Given a byte, compute its 6-bits word binary sequence."}
  (to-binary 6))

(m/fact
  (to-6bits 26) => [0 1 1 0 1 0]
  (to-6bits 1)  => [0 0 0 0 0 1]
  (to-6bits 2)  => [0 0 0 0 1 0]
  (to-6bits 3)  => [0 0 0 0 1 1])

(defn to-bytes
  "Convert a bits sequence into a number"
  [b]
  (->> (reverse b)
       (map-indexed (fn [i v] [(Math/pow 2 i) v]))
       (reduce (fn [a [e n]] (if (= n 1) (+ e a) a)) 0)
       int))

(m/fact
  (to-bytes [1 1 0 0 0 0 1])   => 97
  (to-bytes [0 1 1 0 0 0 0 1]) => 97
  (to-bytes [0 0 0 0 0 0 1 0]) => 2
  (to-bytes [0 0 0 0 0 0 0 0]) => 0
  (to-bytes [1 1 1 1 1 1 1 1]) => 255
  (to-bytes [1 1 1 1 1 1 1 0]) => 254)
