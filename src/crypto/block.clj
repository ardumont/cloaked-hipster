(ns crypto.block
  "Block"
  (:require [midje.sweet  :as m]
            [crypto.ascii :as ascii]))

(defn split
  "Compute 2 n-block chars, [i..i+n] and [i+n+1..i+2n]"
  [i n data]
  (->> data
       (drop i)
       (take (* 2 n))
       (partition-all n)))

(m/fact
  (split 0 3 (mapcat ascii/to-bits "hello, dude")) => [[0 1 1] [0 1 0]]
  (split 0 6 (mapcat ascii/to-bits "hello, dude")) => [[0 1 1 0 1 0] [0 0 0 1 1 0]]
  (split 2 2 "hello world!")                       => [[\l \l] [\o \space]]
  (split 0 2 "he")                                 => [[\h \e]]
  (split 0 6 "hello world! <6b")                   => [[\h \e \l \l \o \space] [\w \o \r \l \d \!]]
  (split 0 8 "hello world!")                       => [[\h \e \l \l \o \space \w \o] [\r \l \d \!]])

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
  "Given a byte input and a key size, return the list of byte blocks transposed."
  [byte-input key-size]
  (->> byte-input
       (map-indexed (fn [i b] [i b]))
       (reduce
        (fn [m [i b]]
          (let [idx (if (zero? i) 0 (mod i key-size))]
            (update-in m [idx] conj b)))
        (sorted-map))
       (map (comp reverse second))))

(m/fact
  (transpose (range 0 20) 4) => [(range 0 20 4)
                                 (range 1 20 4)
                                 (range 2 20 4)
                                 (range 3 20 4)])
