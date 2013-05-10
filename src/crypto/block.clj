(ns crypto.block
  "Block manipulations."
  (:require [midje.sweet :as m]))

(defn shift
  "n-shift the sequence of data - positive value shift to the right and negative value shift to the left. The shift is circular."
  [n data]
  (if (= 0 n)
    data
    (let [l     (count data)
          [h t] (split-at (mod n l) data)]
      (concat t h))))

(m/fact
  (shift 0  [:a :b])             => [:a :b]
  (shift 3  [:a :b :c :d :e :f]) => [:d :e :f :a :b :c]
  (shift 1  [:a :b :c :d :e :f]) => [:b :c :d :e :f :a]
  (shift -1 [:a :b :c])          => [:c :a :b])

(defn transpose
  "Given a data vector v and a size n, return the data vector transposed in row, column vector."
  [v n]
  (->> v
       (reduce
        (fn [[i m] b]
          (let [idx (if (zero? i) 0 (mod i n))]
            [(+ 1 i) (update-in m [idx] conj b)]))
        [0 (sorted-map)])
       second
       (map (comp reverse second))))

(m/fact
  (transpose (range 0 20) 4) => [(range 0 20 4)
                                 (range 1 20 4)
                                 (range 2 20 4)
                                 (range 3 20 4)])
