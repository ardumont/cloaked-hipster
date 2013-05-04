(ns crypto.char
  "A char namespace"
  (:require [midje.sweet :as m]
            [crypto.byte :as byte]))

(def to-hex ^{:doc "char to hex"}
  (comp byte/to-hex int))

(m/fact
  (->> (range 0 17)
       (map (comp to-hex char))) => ["00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f" "10"])
