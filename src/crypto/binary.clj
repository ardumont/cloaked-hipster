(ns crypto.binary
  "A binary namespace to deal with transformation into binary"
  (:require [midje.sweet :as m]
            [crypto.dico :as dico]))

;; bits sequence are read from the left to the right (big endian)
;; significant first, least significant
;; Exemple:
;; head - most significant [0 0 0 0 0 1 1 1] - least significant - tail
;; [0 0 0 0 0 1 1 1] reads 7

(defn- comp-bits-sequence
  "Complement a bits sequence by providing the policy through the complement-fn function."
  [n b complement-fn]
  (let [c (Math/abs (- n (count b)))]
    (if (= 0 c)
      b
      (->> (repeat c 0)
           (complement-fn b)))))

(m/fact
  (comp-bits-sequence 8  [1 1 1]           #(concat %2 %)) => [0 0 0 0 0 1 1 1]
  (comp-bits-sequence 8  [0 0 0 0 1 0 0 0] #(concat %2 %)) => [0 0 0 0 1 0 0 0]
  (comp-bits-sequence 4  [1 1 1]           #(concat %2 %)) => [0 1 1 1]
  (comp-bits-sequence 10 [0 0 0 0 1 0 0 0] #(concat %2 %)) => [0 0 0 0 0 0 1 0 0 0]
  (comp-bits-sequence 8  [1 1 1]           concat)         => [1 1 1 0 0 0 0 0]
  (comp-bits-sequence 8  [0 0 0 0 1 0 0 0] concat)         => [0 0 0 0 1 0 0 0]
  (comp-bits-sequence 4  [1 1 1]           concat)         => [1 1 1 0]
  (comp-bits-sequence 10 [0 0 0 0 1 0 0 0] concat)         => [0 0 0 0 1 0 0 0 0 0])

(defn comp-before
  "Complement by the most significant side (head) a bits sequence to n bits (if necessary)."
  [n b]
  (comp-bits-sequence n b #(concat %2 %)))

(m/fact
  (comp-before 8 [1 1 1])            => [0 0 0 0 0 1 1 1]
  (comp-before 8 [0 0 0 0 1 0 0 0])  => [0 0 0 0 1 0 0 0]
  (comp-before 4 [1 1 1])            => [0 1 1 1]
  (comp-before 10 [0 0 0 0 1 0 0 0]) => [0 0 0 0 0 0 1 0 0 0])

(defn comp-after
  "Complement by the least significant side (tail) a bits sequence to n bits (if necessary)."
  [n b]
  (comp-bits-sequence n b concat))

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
    (conj (-> b (/ 2) int bin) (mod b 2))))

(m/fact
  (bin 97)  => [1 1 0 0 0 0 1]
  (bin 2)   => [1 0]
  (bin 255) => [1 1 1 1 1 1 1 1])

(defn >binary
  "Given a number, compute a function permitting the translation into a n-bits words binary sequence."
  [b]
  (comp (partial comp-before b) bin))

(m/fact
  ((>binary 8) 97) => [0 1 1 0 0 0 0 1]
  ((>binary 8) 2)  => [0 0 0 0 0 0 1 0])

(def >8bits ^{:doc "Given a byte, compute its 8-bits word binary sequence."}
  (>binary 8))

(m/fact
  (>8bits 97) => [0 1 1 0 0 0 0 1]
  (>8bits 2)  => [0 0 0 0 0 0 1 0])

(def >6bits ^{:doc "Given a byte, compute its 6-bits word binary sequence."}
  (>binary 6))

(m/fact
  (>6bits 26) => [0 1 1 0 1 0]
  (>6bits 1)  => [0 0 0 0 0 1]
  (>6bits 2)  => [0 0 0 0 1 0]
  (>6bits 3)  => [0 0 0 0 1 1])

(defn >bytes
  "Convert a bits sequence into a number"
  [b]
  (first
   (reduce
    (fn [[a e] n] [(+ a (* n e)) (/ e 2)])
    [0 (-> 2 (Math/pow (dec (count b))) int)]
    b)))

(m/fact
  (>bytes [1 1 0 0 0 0 1])   => 97
  (>bytes [0 1 1 0 0 0 0 1]) => 97
  (>bytes [0 0 0 0 0 0 1 0]) => 2
  (>bytes [0 0 0 0 0 0 0 0]) => 0
  (>bytes [1 1 1 1 1 1 1 1]) => 255
  (>bytes [1 1 1 1 1 1 1 0]) => 254)

(def >char ^{:private true
                 :doc "Convert a 8-bits sequence into a char"}
  (comp char >bytes))

(m/fact
  (>char [0 1 1 0 0 0 0 1]) => \a)

;; Given a partition of 24 bits, compute the complement [partition of multiple 6 bits, list of complement = char]
(defmulti comp24 count)

;; complement 4 bits to be able to have 2 bytes (12 bits) and we complements with 2 = chars
(defmethod comp24 8 [b] [(comp-after 12 b)
                         [\= \=]])

(m/fact
  (comp24 [1 1 1 1 1 1 1 1]) => [[1 1 1 1 1 1,
                                  1 1 0 0 0 0]
                                 [\= \=]])

;; complement 2 bits to be able to have 3 bytes (18 bits) and we complements with 1 = char
(defmethod comp24 16 [b] [(comp-after 18 b)
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

(defn >b64
  "Given a 8 or 16 or 24-bits chunk, compute the bits sequence into base64."
  [b]
  (let [[part complement] (comp24 b)
        p24               (->> part
                               (partition 6)
                               (map (comp dico/base64 >bytes)))]
    (concat p24 complement)))

(m/fact
  (>b64 [1 1 1 1 1 1, 1 1 0 0 0 0])                           => [\/ \w]
  (>b64 [1 1 1 1 1 1, 1 1 0 0 0 0, 0 0 1 1 0 0])              => [\/ \w \M]
  (>b64 [1 1 1 1 1 1, 1 1 0 0 0 0, 0 0 1 1 1 1, 1 1 1 1 1 1]) => [\/ \w \P \/])
