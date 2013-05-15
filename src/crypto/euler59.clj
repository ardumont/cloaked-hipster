(ns crypto.euler59
  "Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange).
For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key.
The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes.
The user would keep the encrypted message and the encryption key in different locations, and without both halves, it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message,
which is likely, the key is repeated cyclically throughout the message.
The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower case characters.
Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain
common English words, decrypt the message and find the sum of the ASCII values in the original text."
  (:require [midje.sweet    :as m]
            [crypto.key     :as key]
            [crypto.xor     :as xor]
            [clojure.string :as string]))

(defn load-csv
  "Compute a comma separated bytes values into a clojure byte vector."
  [s]
  (read-string (str "[" (string/replace s #",|\n" " ") "]")))

(m/fact
  (load-csv "1, 2, 4, 8") => [1 2 4 8])

(defn load-f "Load the file"
  [filepath]
  (-> filepath
      slurp
      load-csv))

(def ascii-encrypted (load-f "./resources/euler-59-cipher"))

(def key-cipher (key/compute-key ascii-encrypted 3))

(def ascii-decrypted (xor/decrypt {:key key-cipher
                                   :msg ascii-encrypted}))

(m/future-fact
  (->> ascii-decrypted
       crypto.ascii/to-bytes
       (apply +)) => 107359)

(comment

  (defn decryptions [data]
    (let [chrs (range (int \a) (inc (int \z)))]
      (for [a chrs b chrs c chrs]
        (let [k (cycle [a b c])]
          (map (fn [a b] (bit-xor a b)) data k)))))

                                        ; Doesn't take much to validate english!
  (defn possibly-valid? [str]
    (re-find #"(?i: or )" str))

  (defn eulerize [data]
    (->> data
         decryptions
                                        ; this next step is a massive speedup. Each possible decryption is a
                                        ; *lazy-seq*, so if I can filter it out without fully executing it,
                                        ; I can save a lot of work. On my machine, moving the following 3 lines from
                                        ; inside decryptions to here improved the speed to solution from 2100ms to 33ms.
         (filter (partial not-any? (partial < (int \z))))
         (map (comp clojure.string/join (partial map char)))
         (filter possibly-valid?)
         first
         (map int)
         (apply +)))

  (eulerize ascii-encrypted))
