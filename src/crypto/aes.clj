(ns crypto.aes
  "AES manipulations."
  (:require [midje.sweet   :as m]
            [crypto.block  :as block]
            [crypto.byte   :as byte]
            [crypto.xor    :as xor]
            [crypto.base64 :as b64])
  (:import [java.security Key]
           [javax.crypto Cipher]
           [javax.crypto.spec SecretKeySpec]))

;; doc http://docs.oracle.com/javase/7/docs/api/javax/crypto/Cipher.html
;; possible AES policy:
;; AES/CBC/NoPadding (128)
;; AES/CBC/PKCS5Padding (128)
;; AES/ECB/NoPadding (128)
;; AES/ECB/PKCS5Padding (128)

(defn- secret
  "Given a key s, compute a SecretKeySpec"
  [s]
  (SecretKeySpec. (.getBytes s) "AES"))

(defn encrypt
  [s key]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/NoPadding")
                 (.init Cipher/ENCRYPT_MODE (secret key)))]
    (.doFinal cipher (.getBytes s "UTF-8"))))

(defn decrypt
  [buf key]
  (let [cipher (doto (Cipher/getInstance "AES/ECB/NoPadding")
                 (.init Cipher/DECRYPT_MODE (secret key)))]
    (String. (.doFinal cipher buf) "UTF-8")))
