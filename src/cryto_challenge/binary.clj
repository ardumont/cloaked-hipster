(ns cryto-challenge.binary
  "A binary namespace to deal with transformation into binary"
  (:use [midje.sweet :only [fact future-fact]]))

(defn- make8
  "Complement a bit sequence to 8 bits if necessary"
  [b]
  (->> (iterate #(concat [0] %) b)
       (drop-while #(not= 8 (count %)))
       (take 1)
       first))

(fact
  (make8 [1 1 1])           => [0 0 0 0 0 1 1 1]
  (make8 [0 0 0 0 1 0 0 0]) => [0 0 0 0 1 0 0 0])

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
  (comp make8 to-bin))

(fact
  (bin 97) => [0 1 1 0 0 0 0 1]
  (bin 2)  => [0 0 0 0 0 0 1 0])
