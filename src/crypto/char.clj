(ns crypto.char
  "A char namespace"
  (:require [midje.sweet :as m]
            [crypto.byte :as byte]
            [clojure.set :as set]
            [crypto.util :as util]))

(def to-hex ^{:doc "char to hex"}
  (comp byte/to-hex int))

(m/fact
  (->> (range 0 17)
       (map (comp to-hex char))) => ["00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f" "10"])

(def set-chars ^{:private true
                 :doc "Set of characters to make up a word + space"}
  (set (concat (util/range-char \a \z) (util/range-char \A \Z) [\space \' \, \. \-])))

(def set-8bits ^{:private true
                 :doc "Range of characters from 0 to 255 (8 bits)"}
  (set (util/range-char \  \ÿ)))

(def set-not-word ^{:private true
                    :doc "Set of characters that are not making up a word"}
  (set/difference set-8bits set-chars))

(defn sentence?
  "is this a sentence?"
  [w]
  (->> w
       (some set-not-word)
       not))

(m/fact
  (sentence? "")                         => m/truthy
  (sentence? "this")                     => m/truthy
  (sentence? "shittywordareoktoo")       => m/truthy
  (sentence? "_won't work because of _") => m/falsey)
