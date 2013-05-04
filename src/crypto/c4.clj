(ns crypto.c4
  "4. Detect single-character XOR
One of the 60-character strings at:
  https://gist.github.com/3132713
has been encrypted by single-character XOR. Find it. (Your code from #3 should help.)"
  (:require [midje.sweet    :as m]
            [crypto.char    :as char]
            [crypto.xor     :as xor]
            [clojure.string :as s]))

(defn load-words
  "Given a file, load the content of the file and return each line into a vector of lines"
  [f]
  (-> f
      slurp
      s/split-lines))

(def words (load-words "./resources/encrypted-words.txt"))

(defn compute
  "Compute from a list of words"
  [words]
  (->> words
       (map (fn [w] [w (xor/decrypt-brute-force w)]))
       (filter (fn [[_ [_ decrypted-sentence] :as all]]
                 (char/sentence? decrypted-sentence)))))

(m/future-fact :future-fact-to-avoid-the-long-time-computation-just-change-future-fact-into-fact
  (-> "./resources/encrypted-words.txt"
      load-words
      compute)
  => ["7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f" ["5" "Now that the party is jumping\n"]])

;; crypto.challenge4> (-> "./resources/encrypted-words.txt"
;;                        load-words
;;                        compute)
;; (["7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f" ("5" "Now that the party is jumping\n")])

(comment :repl-tryout
  (filter (fn [[_ [_ decrypted-sentence] :as all]]
            (some #{\space} decrypted-sentence))
          [["not filtered" ["1" "12345"]]
           ["test" ["5" "Now that the party is jumping\n"]]])
  (def r (->> words
              (map (fn [w] [w (c3/decrypt-brute-force w)]))
              (filter (fn [[_ [_ decrypted-sentence] :as all]]
                        (char/sentence? decrypted-sentence)))))
  ;; solution read at the repl
  '("5" "Now that the party is jumping\n")  )
