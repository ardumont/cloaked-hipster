(ns crypto.cipher
  "Encipher/Decipher files"
  (:require [midje.sweet :as m]
            [crypto
             [hex        :as hex]
             [binary     :as binary]
             [frequency  :as frequency]
             [byte       :as byte]
             [xor        :as xor]
             [ascii      :as ascii]]))

(defn otp
  "One-time pad"
  [msg]
  (let [k (-> msg count byte/rand-bytes)]
    {:key k
     :msg (xor/encrypt {:key k
                        :msg msg})}))

(m/fact
  (xor/decrypt
   (otp "may the repl be with you")) => "may the repl be with you")
