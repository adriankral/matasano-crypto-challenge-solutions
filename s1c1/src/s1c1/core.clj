(ns s1c1.core)

;; dopóki możesz wczytać dwa znaki: wczytuj i produkuj Base64
;; jak możesz wczytać tylko jeden: wczytaj i potraktuj tak jakby drugi
;; był zerem

(def hexCharToNum
  {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \a 10 \b 11 \c 12 \d 13 \e 14 \f 15}
  )

(defn threeHexesToBin
  "Concatenates string concatenating three hex numbers binary representations."
  [s]
  (reduce + (map (fn [num i] (bit-shift-left num (* i 4))) (map hexCharToNum (seq s)) '(2 1 0))))

;; 3 hexy -> 2 znaki Base64
(defn binToBase64Indexes
  "Converts binary representation to Base64 indexes"
  [bin]
  (list
   (bit-shift-right (bit-and (bit-shift-left
                              (- (bit-shift-left 1 6) 1)
                              6) bin)
                    6)
   (bit-and (- (bit-shift-left 1 6) 1) bin)
   ))

(defn indexToBase64
  "Converts index to base64 value."
  [i]
  (if (= i -1) \=
      (if (and (<= 0 i) (>= 25 i))
        (char (+ i (int \A)))
        (if (and (<= 26 i) (>= 51 i))
          (char (- (+ i (int \a)) 26))
          (if (and (<= 52 i) (>= 61 i))
            (char (- (+ i (int \0)) 52))
            (if (= i 62) \+ \-))))))

(defn hexStringToBase64
  "Converts string containing hex to Base64"
  [s]
  (if (not (first s)) ""
      (concat
       (map indexToBase64 (binToBase64Indexes (threeHexesToBin (take 3 s))))
       (hexStringToBase64 (drop 3 s)))))

(defn -main
  []
  (println (clojure.string/join "" (hexStringToBase64 (read-line)))))
