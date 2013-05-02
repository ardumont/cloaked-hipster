(ns crypto-challenge.binary
  "A binary namespace to deal with transformation into binary"
  (:use [midje.sweet :only [fact future-fact]]))

;; bits sequence are read from the left to the right (big endian)
;; significant first, least significant
;; Exemple:
;; head - most significant [0 0 0 0 0 1 1 1] - least significant - tail
;; [0 0 0 0 0 1 1 1] reads 7

(defn- comp-bit-sequence
  "Complement a bit sequence by providing the policy through the comp-fn function."
  [n b comp-fn]
  (->> (iterate comp-fn b)
       (drop-while #(not= n (count %)))
       first))

(fact
  (comp-bit-sequence 8  [1 1 1]           (partial concat [0])) => [0 0 0 0 0 1 1 1]
  (comp-bit-sequence 8  [0 0 0 0 1 0 0 0] (partial concat [0])) => [0 0 0 0 1 0 0 0]
  (comp-bit-sequence 4  [1 1 1]           (partial concat [0])) => [0 1 1 1]
  (comp-bit-sequence 10 [0 0 0 0 1 0 0 0] (partial concat [0])) => [0 0 0 0 0 0 1 0 0 0]
  (comp-bit-sequence 8  [1 1 1]           #(concat % [0]))      => [1 1 1 0 0 0 0 0]
  (comp-bit-sequence 8  [0 0 0 0 1 0 0 0] #(concat % [0]))      => [0 0 0 0 1 0 0 0]
  (comp-bit-sequence 4  [1 1 1]           #(concat % [0]))      => [1 1 1 0]
  (comp-bit-sequence 10 [0 0 0 0 1 0 0 0] #(concat % [0]))      => [0 0 0 0 1 0 0 0 0 0])

(defn comp-before
  "Complement by the most significant side (head) a bit sequence to n bits (if necessary)."
  [n b]
  (comp-bit-sequence n b (partial concat [0])))

(fact
  (comp-before 8 [1 1 1])            => [0 0 0 0 0 1 1 1]
  (comp-before 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-before 4 [1 1 1])            => [0 1 1 1]
  (comp-before 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 0 0 1 0 0 0])

(defn comp-after
  "Complement by the least significant side (tail) a bit sequence to n bits (if necessary)."
  [n b]
  (comp-bit-sequence n b #(concat % [0])))

(fact
  (comp-after 8 [1 1 1])            => [1 1 1 0 0 0 0 0]
  (comp-after 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-after 4 [1 1 1])            => [1 1 1 0]
  (comp-after 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 1 0 0 0 0 0])

(defn- to-bin
  "Convert a number into binary sequence"
  [n]
  (if (= 0 n)
    []
    (concat (-> n (/ 2) int to-bin)
            [(mod n 2)])))

(fact
  (to-bin 97) => [1 1 0 0 0 0 1]
  (to-bin 2)  => [1 0])

(def bin ^{:doc "Given a number, compute its 8-bit representation"}
  (comp (partial comp-before 8) to-bin))

(fact
  (bin 97) => [0 1 1 0 0 0 0 1]
  (bin 2)  => [0 0 0 0 0 0 1 0])
