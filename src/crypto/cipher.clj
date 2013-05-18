(ns crypto.cipher
  "Encipher/Decipher files"
  (:require [midje.sweet         :as m]
            [clojure.string      :as string]
            [clojure.tools.cli   :as cli]
            [crypto.otp          :as otp]))

(defn- pair
  "Given a list file:file-key space separated, return a list of pair [file file-key]."
  [s]
  (->>
   (string/split s #" ")
   (map #(string/split % #":"))))

(m/fact
  (pair  "/tmp/test-encode.txt:/tmp/test-key.txt /tmp/test-encode.txt:/tmp/test-key.txt") => [["/tmp/test-encode.txt" "/tmp/test-key.txt"]
                                                                                              ["/tmp/test-encode.txt" "/tmp/test-key.txt"]])

(defn -main [& args]
  (let [[options args banner :as opts]
        (cli/cli args
             ["-h" "--help"          "Show help" :default false :flag true]
             ["-e" "--encrypt"       "Encrypt a file"]
             ["-E" "--encrypt-files" "Encrypt a bunch of files"]
             ["-d" "--decrypt"       "Decrypt a file"]
             ["-D" "--decrypt-files" "Decrypt a bunch of files"]
             ["-k" "--key-file"      "Path to the key file"])]

    (if (or (options :help)
            (and (:decrypt options) (not (:key-file options))))

      ;; problem, we display the way to use the command
      (println banner)

      (do

        (when (:decrypt options)
          (println "Encrypting file " (:decrypt options) " using the key file " (:key-file options))
          (->> [:decrypt :key-file]
               (map options)
               (apply otp/decrypt-file!)))

        (when (:encrypt options)
          (println "Encrypting file " (:encrypt :options))
          (-> options :encrypt otp/encrypt-file!))

        (when (:decrypt-files options)
          (println "Decrypting the files " (:decrypt-files options))
          (->> options
               :decrypt-files
               pair
               (map (fn [[file keyfile]]
                      (otp/decrypt-file! file keyfile)))
               doall))

        (when (:encrypt-files options)
          (println "Encrypting the files " (:encrypt-files options))
          (let [files (-> options
                          :encrypt-files
                          (string/split #" "))]
            (->> files
                 (map otp/encrypt-file!)
                 doall)))
        (println "done!")))))

(comment
  (-main "-e" "/tmp/test.txt")
  (-main "-d" "/tmp/test-encoded.txt" "-k" "/tmp/test-key.txt")
  (-main "-D" "/tmp/test-encoded.txt:/tmp/test-key.txt /tmp/test-encoded.txt:/tmp/test-key.txt")
  (-main "-E" "/tmp/test.txt /tmp/test.txt")
  ;; lein run -m crypto.cipher/-main -E "./src/crypto/c1.clj ./src/crypto/c2.clj"
  ;; lein run -m crypto.cipher/-main -D "./src/crypto/c1-encoded.clj:./src/crypto/c1-key.clj ./src/crypto/c2-encoded.clj:./src/crypto/c2-key.clj"
  )
