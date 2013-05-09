(ns crypto.c6
  "6. Break repeating-key XOR
The buffer https://gist.github.com/3132752 is base64-encoded repeating-key XOR.
Break it.

How:
a. Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.

b. Write a function to compute the edit distance/Hamming distance between two strings.
The Hamming distance is just the number of differing bits.
The distance between: 'this is a test'
                 and: 'wokka wokka!!!'
                  is: 37

c. For each KEYSIZE, take the FIRST KEYSIZE [0..KEYSIZE] worth of bytes, and the SECOND KEYSIZE [KEYSIZE+1..2*KEYSIZE] worth of bytes,
 and find the edit distance between them. Normalize this result by dividing by KEYSIZE.

d. The KEYSIZE with the smallest normalized edit distance is probably the key.
 You could proceed perhaps with the smallest 2-3 KEYSIZE values.
 Or take 4 KEYSIZE blocks instead of 2 and average the distances.

e. Now that you probably know the KEYSIZE: break the ciphertext into blocks of KEYSIZE length.

f. Now transpose the blocks: make a block that is the first byte of every block, and a block
that is the second byte of every block, and so on.

g. Solve each block as if it was single-character XOR. You already have code to do this.

e. For each block, the single-byte XOR key that produces the best looking histogram is the
 repeating-key XOR key byte for that block. Put them together and you have the key."
  (:require [midje.sweet :as m]
            [crypto
             [xor   :as xor]
             [key   :as key]
             [ascii :as ascii]
             [c1    :as c1]
             [file  :as file]
             [hex   :as hex]]))

(defn break-repeating-key-xor
  "Given a byte input encoded, break the key and return the decrypted message"
  [input]
  (xor/decrypt {:key (-> input key/compute-key ascii/to-bytes)
                :msg input}))

(m/fact
  (-> "./resources/base64-encoded"
      file/load-simple
      c1/decode
      hex/to-bytes
      break-repeating-key-xor)
  => "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n")
