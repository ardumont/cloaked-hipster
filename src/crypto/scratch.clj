(ns crypto.scratch
  (:require [midje.sweet      :as m]
            [crypto.c1        :as c1]
            [crypto.file      :as file]))

(->> "./resources/base64-encoded"
     file/load-simple
     c1/decode
     (spit "resources/base64-decoded-into-hex-encoded"))
