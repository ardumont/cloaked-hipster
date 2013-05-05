(ns crypto.file
  "Manipulating file"
  (:require [midje.sweet    :as m]
            [clojure.string :as s]))

(defn load-simple
  "Given a file, load its content and glue all lines together."
  [filepath]
  (-> filepath
      slurp
      (s/replace "\n" "")))

(defn load
  "Given a file, load the content of the file and return each line into a vector of lines."
  [filepath]
  (-> filepath
      slurp
      s/split-lines))
