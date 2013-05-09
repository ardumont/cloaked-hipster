(ns crypto.c5
  "5. Repeating-key XOR Cipher
Write the code to encrypt the string:
  Burning 'em, if you ain't quick and nimble
  I go crazy when I hear a cymbal
Under the key 'ICE' , using repeating-key XOR. It should come out to:
  0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f

Encrypt a bunch of stuff using your repeating-key XOR function. Get a feel for it."
  (:require [midje.sweet :as m]
            [crypto.xor  :as xor]
            [crypto.hex  :as hex]
            [crypto.byte :as byte]))

(defn encrypt "Encrypt the ascii msg using the ascii key and return the result into hexadecimal."
  [m]
  (-> m
      xor/encrypt
      byte/to-hex))

(defn decrypt "Decrypt the hexadecimal message using the ascii key and return the ascii message"
  [{:keys [key msg]}]
  (-> {:key key :msg (hex/to-bytes msg)}
      xor/decrypt
      byte/to-ascii))

(m/fact
  (encrypt {:key "ICE"
            :msg "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"})
  => "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  (decrypt {:key "ICE"
            :msg "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"})
  => "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
