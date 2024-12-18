(ns src.conjugate
  (:require
   [clojure.string :as str]
   
   [src.kana :as k]))

(defonce hira-skip (set "んっゟゐゑを"))

(defonce hira-table
  (for [syl k/syllabary
        :let [hira (:h syl)
              sound (or (:ks syl) (:r syl))]
        :when (and hira (= 1 (count hira)) (not (hira-skip hira)))]
             [(:h syl) sound]))

(defonce hira->phoneme
  (into {} (for [[hira letters] hira-table]
             [hira [(if (= 1 (count letters)) "" (first letters)) (last letters)]])))

(defonce phoneme->hira
  (into {} (for [[hira letters] hira-table] [letters hira])))

(defn idx "`get` but with support for negative indexing"
  [coll i]
  (if (< i 0)
    (get coll (+ i (count coll)))
    (get coll i)))

(defn ridx "`subs` but with support for negative indexing"
  [coll i j]
  (let [i' (if (>= i 0) i (+ i (count coll)))
        j' (if (>= j 0) j (+ j (count coll)))]
    (subs coll i' j')))

(defn butlasts "Returns the string without the last n characters (n defaults to 1)"
  ([s]
   (ridx s 0 -1))
  ([s n]
   (ridx s 0 (- n))))

(defn shift "Performs a vowel shift on a hiragana character"
  [hira vowel]
  (let [[c v] (hira->phoneme hira)]
    (phoneme->hira (str c vowel))))

(defn shift-last "Performs a vowel shift on the last hiragana character of a string"
  [word vowel]
  (str (butlasts word) (shift (last word) vowel)))

(defn verb-group "Predicts the class of a hiragana-only word"
  [verb]
  (cond
    ; copula (technically not a verb, but can be conjugated) 
    (= "だ" verb) "だ"
    ; irregular / V3
    (str/ends-with? verb "する") "する"
    ; reading of 来る
    (and (str/ends-with? verb "くる") (not= verb "おくる")) "くる"
    ; NOTE: there are exceptions to this rule v
    (and (str/ends-with? verb "る") (#{"i" "e"} (last (hira->phoneme (idx verb -2))))) :ichidan ; The Ru-verbs, also known as the V2 verbs 
    (= "u" (last (hira->phoneme (last verb)))) :godan ; The U-verbs, also known as V1 verbs
    (not (= "い" (last verb))) :noun-or-na-adj
    ; else, either :i-adj or :noun-or-na-adj but impossible to know for sure
    ))

(defn v-stem
  "conjunctive / stem / masu / i / continuative form"
  [verb group]
  (case group
    :godan (shift-last verb "i")
    :ichidan (butlasts verb)
    "する" (str (butlasts verb 2) "し")
    "くる" (str (butlasts verb 2) "き")
    "だ" "です"))

(defn nai
  "plain negative form"
  [verb group]
  (str (if (= verb "ある")
         ""
        (case group
          :godan (if (str/ends-with? verb "う")
                   (str (butlasts verb) "わ")
                   (shift-last verb "a"))
          :ichidan (butlasts verb)
          "する" (str (butlasts verb 2) "し")
          "くる" (str (butlasts verb 2) "こ")
          :i-adj (str (butlasts verb) "く")))
       "ない"))

(defn nakatta
  "plain past negative form"
  [verb group]
  (str (butlasts (nai verb group)) "かった"))

(defn ta
  "plain past affirmative / perfective form"
  [verb group]
  (case group
    :godan (str (butlasts verb) (case (last verb)
                                 "う" "った"
                                 "つ" "った"
                                 "る" "った"

                                 "む" "んだ"
                                 "ぶ" "んだ"
                                 "ぬ" "んだ"

                                 "く" (if (= verb "行く")
                                       "った"
                                       "いた")

                                 "ぐ" "いだ"
                                 "す" "した"))
    :ichidan (str (butlasts verb) "た")
    "する" (str (butlasts verb 2) "した")
    "くる" (str (butlasts verb 2) "きた")
    "だ" "だった"
    :i-adj (str (butlasts verb) "かった")
    :noun-or-na-adj (str (butlasts verb) "った")))

(defn affirmative-te "connective form"
  [verb group]
  (case group
    "だ" "な"
    :godan (str (butlasts verb) (case (last verb)
                                  "う" (case verb
                                        "問う" "うて"
                                        "請う" "うて"
                                        "って")
                                  "つ" "って"
                                  "る" "って"

                                  ; nasally sounds
                                  "む" "んで"
                                  "ぶ" "んで"
                                  "ぬ" "んで"

                                  ; back of throat
                                  "く" (if (= verb "行く")
                                        "って"
                                        "いて")
                                  "ぐ" "いで"

                                  "す" "して"))
    :ichidan (str (butlasts verb) "て")
    "する" (str (butlasts verb 2) "して")
    "くる" (str (butlasts verb 2) "きて")
    :i-adj (str (butlasts verb) "くて")
    :noun-or-na-adj (str verb "で")))

(defn negative-te [verb group]
  (case group
     :godan (str (butlasts (nai verb group)) "くて")
     :ichidan (str (butlasts verb) "ないで")
     "する" (str (butlasts verb 2) "しないで")
     "くる" (str (butlasts verb 2) "こないで")))

(defn affirmative-ba
  "hypothetical / provisional form: if only, maybe, I wish, what about, should, why don't..."
  [verb group]
    (case group
      :godan (str (butlasts verb) (case (last verb)
                                    "う" "えば"
                                    "つ" "えば"
                                    "る" "えば"

                                    "む" "めば"
                                    "ぶ" "めば"
                                    "ぬ" "めば"

                                    "く" "けば"

                                    "ぐ" "げば"
                                    "す" "せば"))
      :ichidan (str (butlasts verb) "れば")
      "する" (str (butlasts verb 2) "すれば")
      "くる" (str (butlasts verb 2) "これば") 
      :i-adj (str (butlasts verb) "ければ")
      :noun-or-na-adj (str verb "なら")))

(defn negative-ba [verb group]
  (case group
   :godan (str (butlasts (nai verb group)) "ければ")
   :ichidan (str (butlasts verb) "なければ")
   "する" (str (butlasts verb 2) "しなければ")
   "くる" (str (butlasts verb 2) "こなければ")))

(defn affirmative-tara
  "potential form: when, if I ever..."
  [verb group]
  (str (ta verb group) "ら"))

(defn negative-tara [verb group]
  (str (nakatta verb group) "ら"))

(defn imperative [verb group] 
  (if (= verb "くれる")
    "くれ"
    (case group
      :godan (shift-last verb "e")
      :ichidan (str (butlasts verb) "ろ")
      "する" (str (butlasts verb 2) "しろ")
      "くる" (str (butlasts verb 2) "こい"))))

(defn negative-imperative [verb _]
  (str verb "な"))

(defn formal-imperative [verb form]
  (str (v-stem verb form) "なさい"))

(defn volitional
  "presumptive: shall we, let's..."
  [verb group]
  (case group
    :godan (str (shift-last verb "o") "う")
    :ichidan (str (butlasts verb) "よう")
    "する" (str (butlasts verb 2) "しよう")
    "くる" (str (butlasts verb 2) "こよう")))

; "can"
; Native speakers, when speaking casually, may shorten られる to れる, for example, 見みられる becomes 見みれる. However, that’s actually colloquial and not grammatically correct. 
(defn potential [verb group]
  (case group
    :godan (str (shift-last verb "e") "る")
    :ichidan (str (butlasts verb) "られる")
    "する" (str (butlasts verb 2) "できる")
    "くる" (str (butlasts verb 2) "こられる")))

; Note that the passive form of ru-verbs is identical to their potential form. The context and grammatical particles will give you clues as to which form is intended. 
(defn passive [verb group]
  (case group
    :godan (str (shift-last verb "a") "れる")
    :ichidan (str (butlasts verb) "られる")
    "する" (str (butlasts verb 2) "される")
    "くる" (str (butlasts verb 2) "こられる")))

(defn causative
  "let, allowed, made to..."
  [verb group]
  (case group
    :godan (str (shift-last verb "a") "せる")
    :ichidan (str (butlasts verb) "させる")
    "する" (str (butlasts verb 2) "させる")
    "くる" (str (butlasts verb 2) "こさせる")
    :i-adj (str (butlasts verb) "くする")
    :noun-or-na-adj (str verb "にする")))

(defn causative-passive [verb group]
  (case group
    :godan (str (shift-last verb "a") "せられる")
    :ichidan (str (butlasts verb) "させられる")
    "する" (str (butlasts verb 2) "させられる")
    "くる" (str (butlasts verb 2) "こさせられる")
    :i-adj (str (butlasts verb) "くさせられる")
    :noun-or-na-adj (str verb "にさせられる")))

(defn conjugate [verb ; dictionary form of the verb
                 form ; the form you want to conjugate to: :nonpast | :past | :tara | :volitional | :presumptive
                 polite?
                 negative?
                 ]
  (let [group (verb-group verb)
        v (v-stem verb group)]
    (if (= verb "だ")
      (case [form polite? negative?]
        [:nonpast false false] "だ"
        [:nonpast false true] "じゃない"
        [:nonpast true false] "です" ; is/are
        [:nonpast true true] "じゃありません" ; is/are not, also “ではありません”
        
        [:past false false] "だった"
        [:past false true] "じゃなかった" ; also "ではなかった"
        [:past true false] "でした" ; was
        [:past true true] "じゃありませんでした" ; was not, also ではありませんでした 
        )
      (case [form polite? negative?]
        [:nonpast false false] verb ; dictionary / plain
        [:nonpast false true] (nai verb group)
        [:nonpast true false] (str v "ます")
        [:nonpast true true] (str v "ません")
      
        [:past false false] (ta verb group)
        [:past false true] (nakatta verb group)
        [:past true false] (str v "ました")
        [:past true true] (str v "ませんでした")
      
        [:tara false false] (affirmative-tara verb group)
        [:tara false true] (negative-tara verb group)
        [:tara true false] (str v "ましたら")
        [:tara true true] (str v "ませんでしたら")
      
        [:volitional false false] (volitional verb group)
        [:volitional false true] (str (nai verb group) "だろう")
        [:volitional true false] (str v "ましょう")
        [:volitional true true] (str (nai verb group) "でしょう")
      
        [:presumptive false false] (str (ta verb group) "だろう")
        [:presumptive false true] (str (nakatta verb group) "だろう")
        [:presumptive true false] (str (ta verb group) "でしょう")
        [:presumptive true true] (str (nakatta verb group) "でしょう")))))

(defn conjugate-with-negation "returns a function which conjugates a word into a form which does not support politeness"
  [form negative?]
  (case form
    :ba (if negative? negative-ba affirmative-ba)
    ; In principle, a sentence with a te-form inflected verb is a subordinate clause that requires the main clause to be grammatically complete. However, when casually speaking, native speakers sometimes stop at a te-form verb clause, leaving the rest of the sentence implied. 
    :te (if negative? negative-te affirmative-te)
    :imperative (if negative? negative-imperative imperative)))

(defonce FORMS {:masu-stem v-stem
                :nai nai
                :te affirmative-te
                :negative-te negative-te})

#_(doseq [verb ["する"]] 
  (doseq [past? [false true]
          polite? [true false]
          negative? [false true]]
    (println {:verb verb
              :past? past?
              :polite? polite?
              :negative? negative?
              :conjugation (conjugate verb past? polite? negative?)}))
  (let [group (verb-group verb)]
    (doseq [[i form] (map vector 
                      (range)
                      [affirmative-te negative-te
                       affirmative-ba negative-ba
                       affirmative-tara negative-tara
                       imperative negative-imperative
                       volitional
                       potential
                       passive causative causative-passive
                       continuous])]
      (println {:form i :conjugation (form verb group)}))))
