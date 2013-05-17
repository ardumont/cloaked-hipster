(ns crypto.c4
  "Detect single-character XOR from a list of hex-encoded strings."
  (:require [midje.sweet    :as m]
            [crypto.char    :as char]
            [crypto.xor     :as xor]
            [clojure.string :as s]
            [crypto.file    :as file]
            [crypto.hex     :as hex]))

(defn compute
  "Compute from a list of words"
  [words]
  (->> words
       (map (comp
             (fn [w] [w (xor/decrypt-brute-force w)])
             hex/>bytes))
       (filter (fn [[_ [_ decrypted-sentence] :as all]]
                 (char/sentence? decrypted-sentence)))))

(defn compute-encrypted-words
  [filepath]
  (-> filepath
      file/ld
      compute))

(m/fact :future-fact->avoid-the-long-time-computation-just-change-future-fact-in>fact
  (compute-encrypted-words "./resources/encrypted-words")
  => [[[123 90 66 21 65 93 84 65 21 65 93 80 21 69 84 71 65 76 21 92 70 21 95 64 88 69 92 91 82 63] ["5" "Now that the party is jumping\n"]]])

;; crypto.c4> (time (-> "./resources/encrypted-words"
;;                      file/ld
;;                      compute))
;; "Elapsed time: 2.24091 msecs" <- misleading, do not take into account
;; ([(123 90 66 21 65 93 84 65 21 65 93 80 21 69 84 71 65 76 21 92 70 21 95 64 88 69 92 91 82 63) ["5" "Now that the party is jumping\n"]])
