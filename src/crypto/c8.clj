(ns crypto.c8
  "Detecting ECB"
  (:require [midje.sweet   :as m]
            [crypto
             [file   :as file]
             [hex    :as hex]
             [base64 :as b64]
             [aes    :as aes]
             [byte   :as byte]
             [frequency :as frequency]
             [block  :as block]]
            [clojure.string :as string]))

;; One of them is ECB encrypted. Detect it.
;; Remember that the problem with ECB is that it is stateless and
;; deterministic; the same 16 byte plaintext block will always produce
;; the same 16 byte ciphertext.

(def keysize 16)

(defn aes-encoded?
  "We will count each block and see if there is repetition of the same 16 blocks."
  [b]
  (let [l (- (count b) keysize)]
    (->> (range l)                                                               ;; we will take only the size of the block minus 16 characters
         (reduce
          (fn [m i]
            (update-in m [(->> b (drop i) (take 16))] (fn [o] (if o (inc o) 0)))) ;; we will compute for each 16 blocks of characters, their frequency
          {})
         vals                                                                    ;; list all values
         (apply +)                                                               ;; compute their sum
         pos?)))                                                                 ;; if there is at least 1 repetition, this may very well be the ecb encoded block

(m/fact
  (->> "./resources/hex-encoded-ecb-encrypted"
       file/ld
       (map hex/>bytes)
       (filter aes-encoded?)
       byte/>hex) => "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a")
