(ns crypto.otp
  "Encipher/Decipher files"
  (:require [midje.sweet         :as m]
            [clojure.java.io     :as io]
            [clojure.string      :as string]
            [crypto
             [hex                :as hex]
             [base64             :as b64]
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
  "One-time pad with a ascii file. This will generate 1 file encrypted with a one-time pad encoded in base64 and another one with the key encoded in base64."
  [filepath]
  (let [{:keys [key msg]} (-> filepath
                              slurp
                              encrypt)
        k (byte/>b64 key)
        m (byte/>b64 msg)
        fname (-> filepath io/file .getPath)]
    (do
      (spit fname m)
      (spit (file/new-name fname (str "-key")) k))))

(defn decrypt-file!
  "Given a file with one-time pad encoded, this will find the key file and decipher the file."
  [filepath filekey]
  (let [msg (-> filepath slurp b64/bytes)
        key (-> filekey slurp b64/ascii)]
    (spit filepath (decrypt {:key key
                             :msg msg}))))
