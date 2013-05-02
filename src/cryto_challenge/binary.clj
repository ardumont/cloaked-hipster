(ns crypto-challenge.binary
  "A binary namespace to deal with transformation into binary"
  (:use [midje.sweet :only [fact future-fact]]))

;; bits sequence are read from the left to the right (big endian)
;; significant first, least significant
;; Exemple:
;; head - most significant [0 0 0 0 0 1 1 1] - least significant - tail
;; [0 0 0 0 0 1 1 1] reads 7

(defn comp-before
  "Complement a bit sequence to n bits (if necessary) - the bits are added by the most significant side (so by the head)."
  [n b]
  (->> (iterate (partial concat [0]) b)
       (drop-while #(not= n (count %)))
       first))

(fact
  (comp-before 8 [1 1 1])            => [0 0 0 0 0 1 1 1]
  (comp-before 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-before 4 [1 1 1])            => [0 1 1 1]
  (comp-before 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 0 0 1 0 0 0])

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
