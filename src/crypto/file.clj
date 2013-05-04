(ns crypto.file
  "Manipulating file"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]))

(defn load-simple
  [filepath]
  (-> filepath
      slurp
      (s/replace "\n" "")))

(defn load
  "Given a file, load the content of the file and return each line into a vector of lines."
  [filepath]
  (-> filepath
      load-simple
      s/split-lines))
