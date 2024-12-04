(ns src.num
  (:require
   [clojure.math :as math]
   [clojure.string :as str]

   [src.kana :as k]))

(defn randrange
  ([end]
   (randrange 0 end 1))
  ([start end]
   (randrange start end 1))
  ([start end step]
   (+ start (* step (rand-int (math/floor-div (- end start) step))))))

(defn randnum [min-places max-places]
  (let [p (randrange min-places (inc max-places))
        n (math/pow 10 p)]
    (randrange n (dec (* 10 n)))))

(defonce DIGITS ["ゼロ" "いち" "に" "さん" "よん" "ご" "ろく" "なな" "はち" "きゅう"])

(defmulti jnumber identity)

(defmethod jnumber "number" [u n]
  (str (jnumber :default n) "ばん"))

(defmethod jnumber "d" [u n]
  (str (if (= 1 n) "" (get DIGITS n)) "じゅう"))

(defmethod jnumber "c" [u n]
  (get ["ひゃく" "にひゃく" "さんびゃく" "よんひゃく" "ごひゃく" "ろっぴゃく" "ななきゃく" "はっぴゃく" "きゅうひゃく"] (dec n)))

(defmethod jnumber "k" [u n]
  (if (= "?" n)
    "なんぜん"
    (get ["せん" "にせん" "さんぜん" "よんせん" "ごせん" "ろくせん" "ななせん" "はっせん" "きゅうせん"] (dec n))))

(defmethod jnumber "10k" [u n]
  (get ["いちまん" "にまん" "さんまん" "よんまん" "ごまん" "ろくまん" "ななまん" "はちまん" "きゅうまん"] (dec n)))

(defn ridx [coll i j]
  (let [i' (if (>= i 0) i (+ i (count coll)))
        j' (if (>= j 0) j (+ j (count coll)))]
    (subs coll i' j')))

(defn jnumber-with-sandhi [n suffix cases]
  (cond (contains? cases n) (get cases n)
        :else (let [modulo (mod n 10)]
                (cond (and (not (zero? modulo)) (contains? cases modulo)) (str (jnumber :default (- n modulo)) (get cases modulo))
                      (and (zero? modulo) (not (zero? n)) (contains? cases 10)) (str (ridx (jnumber :default n) 0 -3) (get cases 10))
                      :else (str (jnumber :default n) suffix)))))

; counter for long, cylindrical things
; counter for films, TV shows, etc.
; counter for goals, home runs, etc.
; counter for telephone calls
(defmethod jnumber "本" [u n]
  (jnumber-with-sandhi n "ほん" {"?" "なんぼん"
                               1 "いっぽん"
                               3 "さんぼん"
                               6 "ろっぽん"
                               8 "はっぽん"
                               10 "じゅっぽん"}))

; general-purpose counter​; Usually written using kana alone, suffixed to Japanese numerals 1-9 (ひと, ふた, etc.)
; 箇; 箇: Rarely-used kanji form. 個: Rarely-used kanji form.
(defmethod jnumber "つ" [u n]
  (case n
    "?" "いくつ"
    1 "ひとつ"
    2 "ふたつ"
    3 "みっつ"
    4 "よっつ"
    5 "いつつ"
    6 "むっつ"
    7 "ななつ"
    8 "やっつ"
    9 "ここのつ"
    10 "とお"
    (jnumber :default n)))

(defmethod jnumber "人" [u n]
  (case n
    1 "ひとり"
    2 "ふたり"
    4 "よにん"
    (str (jnumber :default n) "にん")))

; 1. counter for people (usu. seating, reservations and such) ​Honorific or respectful (sonkeigo) language
(defmethod jnumber "名" [u n]
  (str (jnumber :default n) "めい"))

; counter for storeys and floors of a building
(defmethod jnumber "階" [u n]
  (jnumber-with-sandhi n "かい" {"?" "なんかい" ; なんがい　also works
                               1 "いっかい"
                               3 "さんがい"
                               6 "ろっかい"
                               10 "じゅっかい"}))

; 1. counter for thin, flat objects (e.g. sheets of paper, plates, coins)​
; 2. counter for portions of gyōza or soba​
; 3. counter for ranks​
; 4. counter for wrestlers of a particular rank​
; 5. counter for fields or rice paddies​
; 6. counter for palanquin bearers
(defmethod jnumber "枚" [u n]
  (str (jnumber :default n) "まい"))

(defmethod jnumber "hour" [u n]
  (jnumber-with-sandhi n "じ" {4 "よ"
                              9 "く"}))

(defmethod jnumber "hours" [u n]
  (str (jnumber "hour" n) "かん"))

(defmethod jnumber "minute" [u n]
  (jnumber-with-sandhi n "ふん" {"?"  "なんぷん"
                               0 ""
                               1 "いっぷん"
                               3 "さんぷん"
                               4 "よんぷん"
                               6 "ろっぷん"
                               8 "はっぷん"
                               10 "じゅっぷん"}))

(defmethod jnumber "minutes" [u n]
  (str (jnumber "minute" n) "かん"))

(defmethod jnumber "years" [u n]
  (jnumber-with-sandhi n "ねんかん" {"*"  "まいとし"
                                 4 "よねんかん"}))

(defmethod jnumber "month" [u n]
  (jnumber-with-sandhi n "がつ" {"*" "まいつき"
                               4 "しがつ"}))

;　ヶ月
(defmethod jnumber "months" [u n]
  (jnumber-with-sandhi n "かげつ" {"*" "まいつき"
                                "各" "かくがつ"
                                1 "いっかげつ"
                                6 "ろっかけつ"
                                10 "じゅっかげつ"}))

(defmethod jnumber "weeks" [u n]
  (jnumber-with-sandhi n "しゅうかん"
                       {"*" "まいしゅう"
                        1 "いっしゅうかん"
                        8 "はっしゅうかん"
                        10 "じゅっしゅうかん"}))

(defmethod jnumber "day" [u n]
  (case n
    1 "ついたち"
    2 "ふつか"
    3 "みっか"
    4 "よっか"
    5 "いつか"
    6 "むいか"
    7 "なのか"
    8 "ようか"
    9 "ここのか"
    10 "とおか"
    14 "じゅうよっか"
    20 "はつか"
    24 "にじゅうよっか"
    (str (jnumber :default n) "にち")))

(defmethod jnumber "days" [u n]
  (case n
    1 "いちにち"
    (str (jnumber "day" n) "かん")))

(defn get-place [n x]
  (if (string? n)
    n
    (mod (quot x (math/pow 10 n)) 10)))

(defmethod jnumber :default [u n]
  (case n
    "?" "なん" ; 何 / what / which
    "*" "まい" ; 毎 / every / all
    "各" "かく" ; each / respective / various
    (str/join " " (for [p (reverse (range 5))
                        :let [place (get-place p n)]
                        :when (not (zero? place))]
                    (case p
                      4 (jnumber "10k" place)
                      3 (jnumber "k" place)
                      2 (jnumber "c" place)
                      1 (jnumber "d" place)
                      0 (get DIGITS place))))))

(defn ord [n]
  (case n 0 "zeroth" 1 "first" 2 "second" 3 "third" 4 "fourth" 5 "fifth" 6 "sixth" 7 "seventh" 8 "eighth" 9 "ninth"))

(defn num-terms []
  (concat
   (for [u ["d" "c" "k" "10k"]
         n (range 1 10)
         :let [base (get ["ten" "twenty" "thirty" "fourty" "fifty" "sixty" "seventy" "eighty" "ninety"] (dec n))]]
     {:eng (case u "d" base "c" (str n " hundred") "k" (str n " thousand") "10k" (str base " thousand"))
      :hir (jnumber u n)})
   (for [u ["本"  "枚" "人" "名" "years" "months" "weeks"]
         n (concat ["?" "*" "各"] (range 1 11))
         :when (not (and (= "つ" u) (contains? #{"*" "各"} n)))]
     {:eng (str (case n "?" "how many" "*" "all" "各" "each" n) " " (case u "本" "poles" "つ" "things" "枚" "sheets" "人" "people" "名" "names" u))
      :hir (jnumber u n)})
   (for [f (range 1 7)]
     {:eng (str (ord f) " floor") :hir (jnumber "階" f)})
   (for [m (range 5 60 5)]
     {:eng (str m "th minute") :hir (jnumber "minute" m)})
   (for [m (range 5 60 5)]
     {:eng (str m " minutes") :hir (jnumber "minutes" m)})
   (for [h (range 1 25)]
     {:eng (str h "th day") :hir (jnumber "day" h)})
   (for [h (range 1 25)]
     {:eng (str h " days") :hir (jnumber "days" h)})
   (for [h (range 1 13)]
     {:eng (str h " o'clock") :hir (jnumber "hour" h)})
   (for [h (range 1 13)]
     {:eng (str h " hours") :hir (jnumber "hours" h)})))

(defn -main []
  (print (str/join "\n" (map (fn [term] (str (:eng term) "," (k/romanize (:hir term)))) (num-terms)))))