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

(defonce HOURS
  ["いち"
   "に"
   "さん"
   "よ"
   "ご"
   "ろく"
   "しち"
   "はち"
   "く"
   "じゅう"
   "じゅう いち"
   "じゅう に"])

(defonce MINUTES {0 ""
                  5 "ご ふん"
                  10 "じゅっぷん"
                  15 "じゅうご ふん"
                  20 "にじゅっぷん"
                  25 "にじゅうご ふん"
                  30 "さんじゅっぷん"
                  35 "さんじゅうご ふん"
                  40 "よんじゅっぷん"
                  45 "よんじゅうご ふん"
                  50 "ごじゅっぷん"
                  55 "ごじゅうご ふん"})

(defonce DIGITS ["ゼロ" "いち" "に" "さん" "よん" "ご" "ろく" "なな" "はち" "きゅう"])

(defmulti jnumber identity)

(defmethod jnumber "number" [u n]
  (str (jnumber :default n) "ばん"))

(defmethod jnumber "hours" [u n]
  (str
   (get HOURS (dec n))
   "じ"))

(defmethod jnumber "minutes" [u n]
  (get MINUTES n))

(defmethod jnumber "d" [u n]
  (str (if (= 1 n) "" (get DIGITS n)) "じゅう"))

(defmethod jnumber "c" [u n]
  (get ["ひゃく" "にひゃく" "さんびゃく" "よんひゃく" "ごひゃく" "ろっぴゃく" "ななきゃく" "はっぴゃく" "きゅうひゃく"] (dec n)))

(defmethod jnumber "k" [u n]
  (get ["せん" "にせん" "さんぜん" "よんせん" "ごせん" "ろくせん" "ななせん" "はっせん" "きゅうせん"] (dec n)))

(defmethod jnumber "10k" [u n]
  (get ["いちまん" "にまん" "さんまん" "よんまん" "ごまん" "ろくまん" "ななまん" "はちまん" "きゅうまん"] (dec n)))

; counter for long, cylindrical things; counter for films, TV shows, etc.; counter for goals, home runs, etc.; counter for telephone calls
(defmethod jnumber "本" [u n]
  (case n
    1 "いっぽん"
    2 "にほん"
    3 "さんぼん"
    4 "よんほん"
    5 "ごほん"
    6 "ろっぽん"
    7 "ななほん"
    8 "はっぽん"
    9 "きゅうほん"
    10 "じゅっぽん"))

; general-purpose counter​; Usually written using kana alone, suffixed to Japanese numerals 1-9 (ひと, ふた, etc.)
; 箇; 箇: Rarely-used kanji form. 個: Rarely-used kanji form.
(defmethod jnumber "つ" [u n]
  (case n
    1 "ひとつ"
    2 "ふたつ"
    3 "みっつ"
    4 "よっつ"
    5 "いつつ"
    6 "むっつ"
    7 "ななつ"
    8 "やっつ"
    9 "ここのつ"
    10 "とお"))

; counter for storeys and floors of a building
(defmethod jnumber "階" [u n]
  (case n
    1 "いっかい"
    2 "にかい"
    3 "さんがい"
    4 "よんかい"
    5 "ごかい"
    6 "ろっかい"))

; 1. counter for thin, flat objects (e.g. sheets of paper, plates, coins)​
; 2. counter for portions of gyōza or soba​
; 3. counter for ranks​
; 4. counter for wrestlers of a particular rank​
; 5. counter for fields or rice paddies​
; 6. counter for palanquin bearers
(defmethod jnumber "枚" [u n]
  (str (jnumber :default n) "まい"))

(defn get-place [n x]
  (mod (quot x (math/pow 10 n)) 10))

(defmethod jnumber :default [u n]
  (str/join " " (for [p (reverse (range 5))
             :let [place (get-place p n)]
             :when (not (zero? place))]
         (case p
           4 (jnumber "10k" place)
           3 (jnumber "k" place)
           2 (jnumber "c" place)
           1 (jnumber "d" place)
           0 (get DIGITS place)))))

(defn ord [n]
  (case n 0 "zeroth" 1 "first" 2 "second" 3 "third" 4 "fourth" 5 "fifth" 6 "sixth" 7 "seventh" 8 "eighth" 9 "ninth"))

(defn num-terms []
  (concat
   (for [u ["d" "c" "k" "10k"]
         n (range 1 10)
         :let [base (get ["ten" "twenty" "thirty" "fourty" "fifty" "sixty" "seventy" "eighty" "ninety"] (dec n))]]
     {:eng (case u "d" base "c" (str n " hundred") "k" (str n " thousand") "10k" (str base " thousand"))
      :hir (jnumber u n)})
   (for [u ["本" "つ" "枚"]
         n (range 1 11)]
     {:eng (str n " " (case u "本" "poles" "つ" "things" "枚" "sheets")) :hir (jnumber u n)})
   (for [f (range 1 7)]
     {:eng (str (ord f) " floor") :hir (jnumber "階" f)})
   (for [m (range 5 60 5)]
     {:eng (str m " minutes") :hir (jnumber "minutes" m)})
   (for [h (range 1 13 1)]
     {:eng (str h " o'clock") :hir (jnumber "hours" h)})))

(defn -main []
  (print (str/join "\n" (map (fn [term] (str (:eng term) "," (k/romanize (:hir term)))) (num-terms)))))