(ns src.num
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

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

(defonce DIGITS ["ぜろ" "いち" "に" "さん" "よん" "ご" "ろく" "なな" "はち" "きゅう"])

(defmulti jnumber identity)

(defmethod jnumber "hours" [u n]
  (str
   (get HOURS (dec n))
   "じ"))

(defmethod jnumber "minutes" [u n]
  (get MINUTES n))

(defmethod jnumber "c" [u n]
  (get ["ひゃく" "にひゃく" "さんびゃく" "よんひゃく" "ごひゃく" "ろっぴゃく" "ななきゃく" "はっぴゃく" "きゅうひゃく"] (dec n)))

(defmethod jnumber "k" [u n]
  (get ["せん" "にせん" "さんぜん" "よんせん" "ごせん" "ろくせん" "ななせん" "はっせん" "きゅうせん"] (dec n)))

(defmethod jnumber "10k" [u n]
  (get ["いちまん" "にまん" "さんまん" "よんまん" "ごまん" "ろくまん" "ななまん" "はちまん" "きゅうまん"] (dec n)))

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
           1 (str (if (= 1 place) "" (get DIGITS place)) "じゅう")
           0 (get DIGITS place)))))