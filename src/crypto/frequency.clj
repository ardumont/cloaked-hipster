(ns crypto.frequency
  "A dictionary namespace"
  (:require [midje.sweet :as m]))

(def ^{:doc "English letter frequency - http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html"}
  frequency
  {\e 12.02
   \t 9.10
   \a 8.12
   \o 7.68
   \i 7.31
   \n 6.95
   \s 6.28
   \r 6.02
   \h 5.92
   \d 4.32
   \l 3.98
   \u 2.88
   \c 2.71
   \m 2.61
   \f 2.30
   \y 2.11
   \w 2.09
   \g 2.03
   \p 1.82
   \b 1.49
   \v 1.11
   \k 0.69
   \x 0.17
   \q 0.11
   \j 0.10
   \z 0.07})

(m/fact
  (map frequency (map char (range 97 123)))
  => [8.12 1.49 2.71 4.32 12.02 2.30 2.03 5.92 7.31 0.10 0.69 3.98 2.61 6.95 7.68 1.82 0.11 6.02 6.28 9.10 2.88 1.11 2.09 0.17 2.11 0.07])
