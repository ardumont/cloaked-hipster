(ns crypto.bench
  "A namespace to measure the code"
  (:require [crypto
             [byte          :as b]
             [hex           :as hex]
             [base64        :as b64]
             [xor           :as xor]
             [c2            :as c2]
             [c3            :as c3]
             [c4            :as c4]
             [c5            :as c5]
             [c6            :as c6]
             [c7            :as c7]]
            [criterium.core :as crit]))

;;;;;;;;;;;;;;;;;;;;;;;;;; c1

;; crypto.c1> (crit/bench (hex/to-b64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
;; WARNING: Final GC required 3.411904550319699 % of runtime
;; Evaluation count : 55020 in 60 samples of 917 calls.
;;              Execution time mean : 1,096743 ms
;;     Execution time std-deviation : 18,680270 µs
;;    Execution time lower quantile : 1,070496 ms ( 2,5%)
;;    Execution time upper quantile : 1,129339 ms (97,5%)
;;                    Overhead used : 2,382193 ns

;; Found 1 outliers in 60 samples (1,6667 %)
;; 	low-severe	 1 (1,6667 %)
;;  Variance from outliers : 6,2677 % Variance is slightly inflated by outliers
;; nil

;; crypto.bench> (time (hex/to-b64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
;; "Elapsed time: 1.388937 msecs"
;; "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

;;;;;;;;;;;;;;;;;;;;;;;;;; c2

;; crypto.bench> (crit/bench (c2/xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"))
;; WARNING: Final GC required 3.792227670266822 % of runtime
;; Evaluation count : 92880 in 60 samples of 1548 calls.
;;              Execution time mean : 648,984022 µs
;;     Execution time std-deviation : 4,340272 µs
;;    Execution time lower quantile : 644,466167 µs ( 2,5%)
;;    Execution time upper quantile : 658,254670 µs (97,5%)
;;                    Overhead used : 2,348702 ns

;; Found 2 outliers in 60 samples (3,3333 %)
;; 	low-severe	 1 (1,6667 %)
;; 	low-mild	 1 (1,6667 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers
;; nil

;; crypto.bench> (time (c2/xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"))
;; "Elapsed time: 1.232153 msecs"
;; "746865206b696420646f6e277420706c6179"

;;;;;;;;;;;;;;;;;;;;;;;;;; c3

;; crypto.bench> (crit/bench (c3/brute-force "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
;; Evaluation count : 180 in 60 samples of 3 calls.
;;              Execution time mean : 345,211903 ms
;;     Execution time std-deviation : 1,857865 ms
;;    Execution time lower quantile : 342,354781 ms ( 2,5%)
;;    Execution time upper quantile : 348,706638 ms (97,5%)
;;                    Overhead used : 2,348702 ns
;; nil

;; crypto.bench> (time (c3/brute-force "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
;; "Elapsed time: 347.035457 msecs"
;; ["X" "Cooking MC's like a pound of bacon"]

;;;;;;;;;;;;;;;;;;;;;;;;;; c4

;; crypto.bench> (crit/bench (c4/compute-encrypted-words "./resources/encrypted-words"))
;; Evaluation count : 79920 in 60 samples of 1332 calls.
;;              Execution time mean : 766,667982 µs
;;     Execution time std-deviation : 4,220269 µs
;;    Execution time lower quantile : 761,402485 µs ( 2,5%)
;;    Execution time upper quantile : 778,691898 µs (97,5%)
;;                    Overhead used : 2,348702 ns

;; Found 3 outliers in 60 samples (5,0000 %)
;; 	low-severe	 3 (5,0000 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;; c5

;; crypto.bench> (crit/bench (c5/encrypt {:key "ICE"
;;                                        :msg "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"}))
;; Evaluation count : 25620 in 60 samples of 427 calls.
;;              Execution time mean : 2,343191 ms
;;     Execution time std-deviation : 33,893384 µs
;;    Execution time lower quantile : 2,318569 ms ( 2,5%)
;;    Execution time upper quantile : 2,419053 ms (97,5%)
;;                    Overhead used : 2,348702 ns

;; Found 5 outliers in 60 samples (8,3333 %)
;; 	low-severe	 2 (3,3333 %)
;; 	low-mild	 3 (5,0000 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers
;; nil

;; crypto.bench> (crit/bench (c5/decrypt {:key "ICE"
;;                                             :msg "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"}))
;; Evaluation count : 27960 in 60 samples of 466 calls.
;;              Execution time mean : 2,172029 ms
;;     Execution time std-deviation : 28,914999 µs
;;    Execution time lower quantile : 2,135203 ms ( 2,5%)
;;    Execution time upper quantile : 2,225239 ms (97,5%)
;;                    Overhead used : 2,348702 ns

;; Found 2 outliers in 60 samples (3,3333 %)
;; 	low-severe	 1 (1,6667 %)
;; 	low-mild	 1 (1,6667 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;; c6

;; crypto.bench> (crit/bench (c6/break-repeating-key-xor-in-b64-encoded "./resources/base64-encoded"))
;; Evaluation count : 60 in 60 samples of 1 calls.
;;              Execution time mean : 24,323362 sec
;;     Execution time std-deviation : 184,298216 ms
;;    Execution time lower quantile : 24,171696 sec ( 2,5%)
;;    Execution time upper quantile : 24,907294 sec (97,5%)
;;                    Overhead used : 2,348702 ns

;; Found 5 outliers in 60 samples (8,3333 %)
;; 	low-severe	 2 (3,3333 %)
;; 	low-mild	 3 (5,0000 %)
;;  Variance from outliers : 1,6389 % Variance is slightly inflated by outliers
;; nil
