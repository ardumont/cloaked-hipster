(ns crypto.file
  "Manipulating file"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]))

(defn load
  "Given a file, load the content of the file and return each line into a vector of lines."
  [f]
  (-> f
      slurp
      s/split-lines))
