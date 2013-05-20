(ns crypto.scratch
  (:require [midje.sweet :as m]
            [crypto
             [ascii      :as ascii]
             [base64     :as b64]
             [hex        :as hex]
             [byte       :as byte]]))

(-> "may the repl be with you" ascii/>bytes byte/>hex)
"6d617920746865207265706c206265207769746820796f75"


(b64/>hex "bWF5IHRoZSByZXBsIGJlIHdpdGggeW91")"6d617920746865207265706c206265207769746820796f75"
