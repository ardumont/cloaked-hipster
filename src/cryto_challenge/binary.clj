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
  (comp-after 10 [1 1 1 1 1 1 1 1])  => [1 1 1 1 1 1 1 1 0 0]
  (comp-after 8 [1 1 1])            => [1 1 1 0 0 0 0 0]
  (comp-after 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-after 4 [1 1 1])            => [1 1 1 0]
  (comp-after 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 1 0 0 0 0 0])

(defn- bin
  "Convert a number into binary sequence (will create as much bits as needed)."
  [n]
  (if (= 0 n)
    []
    (concat (-> n (/ 2) int bin)
            [(mod n 2)])))

(fact
  (bin 97) => [1 1 0 0 0 0 1]
  (bin 2)  => [1 0])

(def to-bin ^{:doc "Given a number, compute its 8-bit representation."}
  (comp (partial comp-before 8) bin))

(fact
  (to-bin 97) => [0 1 1 0 0 0 0 1]
  (to-bin 2)  => [0 0 0 0 0 0 1 0])

(defn to-num
  "Convert a bit sequence into a number"
  [b]
  (if (= b (repeat 8 0))
    0
    (->> (reverse b)
         (map-indexed (fn [i v] [(Math/pow 2 i) v]))
         (reduce (fn [a [e n]] (if (= n 1) (+ e a) a)) 0)
         int)))

(fact
  (to-num [1 1 0 0 0 0 1]) => 97
  (to-num [0 1 1 0 0 0 0 1]) => 97
  (to-num [0 0 0 0 0 0 1 0]) => 2
  (to-num [0 0 0 0 0 0 0 0]) => 0
  (to-num [1 1 1 1 1 1 1 1]) => 255
  (to-num [1 1 1 1 1 1 1 0]) => 254)
