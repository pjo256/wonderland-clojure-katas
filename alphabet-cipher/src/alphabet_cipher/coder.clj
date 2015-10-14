(ns alphabet-cipher.coder)

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def len 26)

(defn distance [letter char]
  (Math/abs (- (int letter ) (int (.charAt char 0)))))

(defn keyword_cycle [keyword message_length]
  (take message_length (cycle keyword)))

(defn encode_letter [keyword_letter message_letter]
  (.charAt alphabet (mod (+ (distance keyword_letter "a") (distance message_letter "a")) len)))

(defn decode_letter [keyword_letter encoded_message_letter]
  (.charAt alphabet (mod (+ 1 (+ (distance keyword_letter "z") (distance encoded_message_letter "a"))) len)))

(defn decipher_letters [cipher_letter message_letter]
  (decode_letter message_letter cipher_letter))

(defn encode [keyword message]
  (apply str (map encode_letter (keyword_cycle keyword (count message)) message)))

(defn decode [keyword message]
  (apply str (map decode_letter (keyword_cycle keyword (count message)) message)))

(defn decipher [cipher message]
  (let [keyword_cycled (apply str (map decipher_letters cipher message))]
       (loop [ind 1]
             (let [substr_key (apply str (take ind keyword_cycled))]
               (if (= message (decode substr_key cipher))
                 substr_key
                (recur (inc ind)))))))
