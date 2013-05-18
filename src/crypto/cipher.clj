(ns crypto.cipher
  "Encipher/Decipher files"
  (:require [midje.sweet         :as m]
            [clojure.java.io     :as io]
            [clojure.string      :as string]
            [clojure.tools.cli   :as cli]
            [clojure.tools.trace :as trace]
            [crypto
             [hex                :as hex]
             [binary             :as binary]
             [frequency          :as frequency]
             [byte               :as byte]
             [xor                :as xor]
             [file               :as file]
             [ascii              :as ascii]]))

(defn otp
  "One-time pad a ascii message."
  [msg]
  (let [k (-> msg count byte/rand-bytes)]
    {:key k
     :msg (xor/encrypt {:key k
                        :msg msg})}))

(m/fact
  (xor/decrypt
   (otp "may the repl be with you")) => "may the repl be with you")

(defn otp-encrypt-file!
  "One-time pad with a ascii file. This will generate a file encoded with a one-time pad and another one with the key."
  [filepath]
  (let [{:keys [key msg]} (-> filepath
                              slurp
                              otp)
        k (byte/>ascii key)
        m (byte/>ascii msg)
        fname (-> filepath io/file .getPath)]
    (do
      (spit (file/new-name fname (str "-encoded")) m)
      (spit (file/new-name fname (str "-key")) k))))

(defn otp-decrypt-file!
  "Given a file with one-time pad encoded, this will find the key file and decipher the file."
  [filepath filekey]
  (let [msg (slurp filepath)
        key (slurp filekey)
        res (xor/decrypt {:key key
                          :msg (ascii/>bytes msg)})]
    (spit (file/new-name filepath (str "-then-decoded")) res)))

(defn -main [& args]
  (let [[options args banner :as opts]
        (cli/cli args
             ["-h" "--help"     "Show help" :default false :flag true]
             ["-d" "--decrypt"  "Decrypt a file"]
             ["-e" "--encrypt"  "Encrypt a file"]
             ["-k" "--key-file" "the path to the key file"])]

    (if (or (options :help)
            (and (:decrypt options) (not (:key-file options))))
      ;; problem, we display the way to use the command
      (println banner)

      (do

        (when (:decrypt options)
          (println "Encrypting file " (:decrypt options) " using the key file " (:key-file options))
          (->> [:decrypt :key-file]
               (map options)
               (apply otp-decrypt-file!)))

        (when (:encrypt options)
          (println "Encrypting file " (:encrypt :options))
          (-> options :encrypt otp-encrypt-file!))

        (println "done!")))))

(comment
  (-main "-e" "/tmp/test.txt")
  (-main "-d" "/tmp/test-encoded.txt" "-k" "/tmp/test-key.txt"))
