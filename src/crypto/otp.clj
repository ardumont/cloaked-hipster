(ns crypto.otp
  "Encipher/Decipher files"
  (:require [midje.sweet         :as m]
            [clojure.java.io     :as io]
            [clojure.string      :as string]
            [crypto
             [hex                :as hex]
             [binary             :as binary]
             [frequency          :as frequency]
             [byte               :as byte]
             [xor                :as xor]
             [file               :as file]
             [ascii              :as ascii]]))

(defn encrypt
  "One-time pad a ascii message."
  [msg]
  (let [k (-> msg count byte/rand-bytes)]
    {:key k
     :msg (xor/encrypt {:key k
                        :msg msg})}))

(def decrypt ^{:doc "The symmetric call to decrypt."}
  xor/decrypt)

(m/fact
  (decrypt
   (encrypt "may the repl be with you")) => "may the repl be with you")

(defn encrypt-file!
  "One-time pad with a ascii file. This will generate a file encoded with a one-time pad and another one with the key."
  [filepath]
  (let [{:keys [key msg]} (-> filepath
                              slurp
                              encrypt)
        k (byte/>ascii key)
        m (byte/>ascii msg)
        fname (-> filepath io/file .getPath)]
    (do
      (spit fname m)
      (spit (file/new-name fname (str "-key")) k))))

(defn decrypt-file!
  "Given a file with one-time pad encoded, this will find the key file and decipher the file."
  [filepath filekey]
  (let [msg (slurp filepath)
        key (slurp filekey)
        res (decrypt {:key key
                      :msg (ascii/>bytes msg)})]
    (spit filepath res)))
