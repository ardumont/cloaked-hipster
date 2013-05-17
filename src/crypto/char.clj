(ns crypto.char
  "A char namespace"
  (:require [midje.sweet :as m]
            [crypto.byte :as byte]
            [clojure.set :as set]
            [crypto.util :as util]))

(def >hex ^{:doc "char to hex"}
  (comp byte/>hex int))

(m/fact
  (->> (range 0 17)
       (map (comp >hex char))) => ["00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f" "10"])

(def set-chars ^{:private true
                 :doc "Set of characters to make up sentence + ponctuations character including newline"}
  (set (concat (util/rng-char \a \z) (util/rng-char \A \Z) [\space \' \, \. \- \: \; \? \newline])))

(def set-8bits ^{:private true
                 :doc "Range of characters from 0 to 255 (8 bits)"}
  (set (util/rng-char (char 0) (char 255))))

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
  (sentence? "Now that the party is jumping
")                                                    => m/truthy
  (sentence? "")                                      => m/truthy
  (sentence? "we : ; will,work-alright,won't we?.\n") => m/truthy
  (sentence? "shittywordareoktoo")                    => m/truthy
  (sentence? "_won't work because of _")              => m/falsey)
