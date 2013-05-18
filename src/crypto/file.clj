(ns crypto.file
  "File manipulations."
  (:refer-clojure :exclude [name])
  (:require [midje.sweet    :as m]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn basename
  [filepath]
  (->> (string/split filepath #"/")
       butlast
       (string/join "/")))

(defn name
  "Given a filename, return its extension."
  [filename]
  (string/join "."
               (-> filename
                   (string/split #"\.")
                   butlast)))

(m/fact
  (name "test.middle.another.txt") => "test.middle.another")

(defn ext
  "Given a filename, return its extension."
  [filename]
  (-> filename
      (string/split #"\.")
      last))

(m/fact
  (ext "test.middle.another.txt") => "txt")

(defn ld-simple
  "Given a file, load its content and glue all lines together."
  [filepath]
  (-> filepath
      slurp
      (string/replace "\n" "")))

(defn ld
  "Given a file, load the content of the file and return each line into a vector of lines."
  [filepath]
  (-> filepath
      slurp
      string/split-lines))

(defn new-name
  "Given a filepath, build a new name respecting the same path."
  [fname k]
  (let [ext (ext fname)
        name (name fname)]
    (str name k "." ext)))

(m/fact
  (new-name "/home/test/this/is/a/path/to/this-file.middle.txt" "-key") => "/home/test/this/is/a/path/to/this-file.middle-key.txt")
