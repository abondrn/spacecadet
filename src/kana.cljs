(ns src.kana
  (:require 
   [cljs.test :as t :refer [async deftest is testing]] 
   [clojure.pprint :as pprint]
   
   [src.trie :as trie]))

; https://en.wiktionary.org/wiki/Appendix:Easily_confused_Japanese_kana
(defonce similar-looking
  ["ちらきさ"
   "おむ"
   "こてに"
   "たな"
   "ぬめ"
   "ねれわゐ"
   "るろそ"
   "はほ"
   "シツ"
   "タクケ夕久"
   "スヌク久"
   "ンソリり" ; (利)
   "ウラヲう"
   "コユつワフ"
   "アマム"
   "ナメオ"
   "ハへヘ"
   "ヨヲ"
   "ヤセヒ七匕や也せ世サ"
   "エ工"
   "オ才"
   "カ力か" ; (加)
   "チ千"
   "ナ十"
   "ニ二こ"
   "ヌ又"
   "ホ木"
   "ミ彡"
   "ロ口"
   "ハ八"
   "ー一"
   "ト卜"
   "ム厶"
   "ヨ彐"
   "しレ"
   "りク"
   ])
;mo: も モ (毛)

; source: https://github.com/kanasubs/namban/blob/master/src/namban/shocho.cljx
; origin fields (ho and ko) derived from https://www.sljfaq.org/afaq/originofkana.html
(defonce syllabary
  [{:y "。" :ry "."} {:y "、" :ry ","} {:y "？" :ry "?"} {:y "！" :ry "!"}
   {:y "　" :ry " "} {:y "ゝ" :ry "ヽ"} {:y "ゞ" :ry "ヾ"} {:y "ー" :ry "-"}
   {:y "（" :ry "("} {:y "）" :ry ")"} {:y "｛" :ry "{"} {:y "｝" :ry "}"}
   {:y "［" :ry "["} {:y "］" :ry "]"} {:y "＠" :ry "@"} {:y "・" :ry " "}
   {:y "＃" :ry "#"} {:y "＄" :ry "$"} {:y "％" :ry "%"} {:y "＆" :ry "&"}
   {:y "｀" :ry "`"} {:y "：" :ry ":"} {:y "…" :ry "..."} {:y "＊" :ry "*"}
   {:y "※" :ry "*"} {:y "；" :ry ";"} {:y "￡" :ry "£"}

   ; this is some kind of yakumono, but can't be included above
   {:k "ヿ" :h "ゟ" :r " "}

   {:h "あ" :k "ア" :r "a" :ho "安" :ko "阿" :s "ａ" :as "a"}
   {:h "い" :k "イ" :r "i" :ho "以" :ko "伊"}
   {:h "う" :k "ウ" :r "u" :ho "宇" :ko "宇"}
   {:h "え" :k "エ" :r "e" :ho "衣" :ko "衣" :as "e"}
   {:h "お" :k "オ" :r "o" :ho "於" :ko "於"}

   ; numbers go after {:h "あ" :k "ア" :r "a" :s "ａ" :as "a"}
   {:s "０" :as "0"} {:s "１" :as "1"} {:s "２" :as "2"} {:s "３" :as "3"}
   {:s "４" :as "4"} {:s "５" :as "5"} {:s "６" :as "6"} {:s "７" :as "7"}
   {:s "８" :as "8"} {:s "９" :as "9"} {:s "ｂ" :as "b"} {:s "ｃ" :as "c"}
   {:s "ｄ" :as "d"} {:s "ｅ" :as "e"} {:s "ｆ" :as "f"} {:s "Ａ" :as "A"}
   {:s "Ｂ" :as "B"} {:s "Ｃ" :as "C"} {:s "Ｄ" :as "D"} {:s "Ｅ" :as "E"}
   {:s "Ｆ" :as "F"}

   ; TODO support wapuro -/nn
   ;{:h "ん" :k "ン" :r "n'"}
   {:h "ん" :k "ン" :r "n" :ho "毛无" :ko "尓爾"} ; n => ん/ン
   ;{:h "ん" :k "ン" :r "n-" :ks "n'"} ; n- => ん/ン/n'
   ;{:h "ん" :k "ン" :r "m" :ks "n"} ; m => n/ん/ン

   ; as in ちぇっ (chietsu/tietu)
   {:h "っ" :k "ッ" :r "tsu" :ks "tu"}

    ; sources:
    ;   - https://en.wikipedia.org/wiki/Hepburn_romanization
    ;   - https://en.wikipedia.org/wiki/Kunrei-shiki_romanization
    ; gojūon
   {:h "か" :k "カ" :r "ka" :ko "加" :ho "加"}
   {:h "き" :k "キ" :r "ki" :ko "幾" :ho "幾"}
   {:h "く" :k "ク" :r "ku" :ko "久" :ho "久"}
   {:h "け" :k "ケ" :r "ke" :ko "介" :ho "計"}
   {:h "こ" :k "コ" :r "ko" :ko "己" :ho "己"}

   {:h "さ" :k "サ" :r "sa"  :ho "左" :ko "散"}
   {:h "し" :k "シ" :r "shi" :ho "之" :ko "之" :ks "si"}
   {:h "す" :k "ス" :r "su"  :ho "寸" :ko "須"}
   {:h "せ" :k "セ" :r "se"  :ho "世" :ko "世"}
   {:h "そ" :k "ソ" :r "so"  :ho "會" :ko "會"}

   {:h "た" :k "タ" :r "ta"  :ho "太" :ko "多"}
   {:h "ち" :k "チ" :r "chi" :ho "知" :ko "千" :ks "ti"}
   {:h "つ" :k "ツ" :r "tsu" :ho "州" :ko "州" :ks "tu"}
   {:h "て" :k "テ" :r "te"  :ho "天" :ko "天"}
   {:h "と" :k "ト" :r "to"  :ho "止" :ko "止"}

   {:h "な" :k "ナ" :r "na" :ho "奈" :ko "奈"}
   {:h "に" :k "ニ" :r "ni" :ho "仁" :ko "二"}
   {:h "ぬ" :k "ヌ" :r "nu" :ho "奴" :ko "奴"}
   {:h "ね" :k "ネ" :r "ne" :ho "祢" :ko "祢"}
   {:h "の" :k "ノ" :r "no" :ho "乃" :ko "乃"}

   {:h "は" :k "ハ" :r "ha" :ho "波" :ko "八"}
   {:h "ひ" :k "ヒ" :r "hi" :ho "比" :ko "比"}
   {:h "ふ" :k "フ" :r "fu" :ho "不" :ko "不" :ks "hu"}
   {:h "へ" :k "ヘ" :r "he" :ho "部" :ko "部"}
   {:h "ほ" :k "ホ" :r "ho" :ho "保" :ko "保"}

   {:h "ま" :k "マ" :r "ma" :ho "末" :ko "万末"}
   {:h "み" :k "ミ" :r "mi" :ho "美" :ko "三"}
   {:h "む" :k "ム" :r "mu" :ho "武" :ko "牟"}
   {:h "め" :k "メ" :r "me" :ho "女" :ko "女"}
   {:h "も" :k "モ" :r "mo" :ho "毛" :ko "毛"}

   {:h "や" :k "ヤ" :r "ya" :ho "也" :ko "也"}
   {:h "ゆ" :k "ユ" :r "yu" :ho "由" :ko "由"}
   {:h "よ" :k "ヨ" :r "yo" :ho "与" :ko "与"}

   {:h "ら" :k "ラ" :r "ra" :ho "良" :ko "良"}
   {:h "り" :k "リ" :r "ri" :ho "利" :ko "利"}
   {:h "る" :k "ル" :r "ru" :ho "留" :ko "留"}
   {:h "れ" :k "レ" :r "re" :ho "礼" :ko "流礼"}
   {:h "ろ" :k "ロ" :r "ro" :ho "呂" :ko "呂"}

   {:h "わ" :k "ワ" :r "wa" :ho "和" :ko "和"}
   {:h "ゐ" :k "ヰ" :r "wi" :ho "為" :ko "井" :ks "i"}
   {:h "ゑ" :k "ヱ" :r "we" :ho "恵" :ko "慧恵" :ks "e"}
   {:h "を" :k "ヲ" :r "o"  :ho "遠" :ko "乎" :ks "o"}

    ; dakuten no gojūon
   {:h "が" :k "ガ" :r "ga"} {:h "ぎ" :k "ギ" :r "gi"} {:h "ぐ" :k "グ" :r "gu"}
   {:h "げ" :k "ゲ" :r "ge"} {:h "ご" :k "ゴ" :r "go"}

   {:h "ざ" :k "ザ" :r "za"} {:h "じ" :k "ジ" :r "ji" :ks "zi"}
   {:h "ず" :k "ズ" :r "zu"} {:h "ぜ" :k "ゼ" :r "ze"} {:h "ぞ" :k "ゾ" :r "zo"}

   {:h "だ" :k "ダ" :r "da"} {:h "ぢ" :k "ヂ" :r "ji" :ks "zi"}
   {:h "づ" :k "ヅ" :r "zu"} {:h "で" :k "デ" :r "de"} {:h "ど" :k "ド" :r "do"}

   {:h "ば" :k "バ" :r "ba"} {:h "び" :k "ビ" :r "bi"} {:h "ぶ" :k "ブ" :r "bu"}
   {:h "べ" :k "ベ" :r "be"} {:h "ぼ" :k "ボ" :r "bo"}

   {:h "ぱ" :k "パ" :r "pa"} {:h "ぴ" :k "ピ" :r "pi"} {:h "ぷ" :k "プ" :r "pu"}
   {:h "ぺ" :k "ペ" :r "pe"} {:h "ぽ" :k "ポ" :r "po"}

    ; yōon
   {:h "きゃ" :k "キャ" :r "kya"} {:h "きゅ" :k "キュ" :r "kyu"}
   {:h "きょ" :k "キョ" :r "kyo"}

   {:h "しゃ" :k "シャ" :r "sha" :ks "sya"}
   {:h "しゅ" :k "シュ" :r "shu" :ks "syu"}
   {:h "しょ" :k "ショ" :r "sho" :ks "syo"}

   {:h "ちゃ" :k "チャ" :r "cha" :ks "tya"}
   {:h "ちゅ" :k "チュ" :r "chu" :ks "tyu"}
   {:h "ちょ" :k "チョ" :r "cho" :ks "tyo"}

   {:h "にゃ" :k "ニャ" :r "nya"} {:h "にゅ" :k "ニュ" :r "nyu"}
   {:h "にょ" :k "ニョ" :r "nyo"}

   {:h "ひゃ" :k "ヒャ" :r "hya"} {:h "ひゅ" :k "ヒュ" :r "hyu"}
   {:h "ひょ" :k "ヒョ" :r "hyo"}

   {:h "みゃ" :k "ミャ" :r "mya"} {:h "みゅ" :k "ミュ" :r "myu"}
   {:h "みょ" :k "ミョ" :r "myo"}

   {:h "りゃ" :k "リャ" :r "rya"} {:h "りゅ" :k "リュ" :r "ryu"}
   {:h "りょ" :k "リョ" :r "ryo"}

    ; dakuten no yōon
   {:h "ぎゃ" :k "ギャ" :r "gya"} {:h "ぎゅ" :k "ギュ" :r "gyu"}
   {:h "ぎょ" :k "ギョ" :r "gyo"}

   ;{:h "じゃ" :k "ジャ" :r "ja" :ks "zya"} {:h "じゅ" :k "ジュ" :r "ju" :ks "zyu"}
   ;{:h "じょ" :k "ジョ" :r "jo" :ks "zyo"}

    ; order counts - ぢ syllab forms must go after じ forms above
   {:h "ぢゃ" :k "ヂャ" :r "ja" :ks "zya"} ; found no way to input this yet
   {:h "ぢゅ" :k "ヂュ" :r "ju" :ks "zyu"} ; found no way to input this yet
   {:h "ぢょ" :k "ヂョ" :r "jo" :ks "zyo"} ; found no way to input this yet

   {:h "びゃ" :k "ビャ" :r "bya"} {:h "びゅ" :k "ビュ" :r "byu"}
   {:h "びょ" :k "ビョ" :r "byo"}

   {:h "ぴゃ" :k "ピャ" :r "pya"} {:h "ぴゅ" :k "ぴュ" :r "pyu"}
   {:h "ぴょ" :k "ピョ" :r "pyo"}

    ; extended katakana with some hiragana occurrences
    ; hiragana occurrences for compatibility only appear last in maps
   {:k "イィ" :r "yi" :h "いぃ"} {:k "イェ" :r "ye" :h "いぇ"}

   {:k "ウァ" :r "wa" :h "うぁ"} {:k "ウィ" :r "wi" :h "うぃ"}
   {:k "ウゥ" :r "wu" :h "うぅ"}

   {:k "ウェ" :r "we" :h "うぇ"} {:k "ウォ" :r "wo" :h "うぉ"}

   {:k "ウュ" :r "wyu" :h "うゅ"}

   {:k "ヴァ" :r "va" :h "ゔぁ"} {:k "ヴィ" :r "vi" :h "ゔぃ"}
   {:h "ゔ" :k "ヴ" :r "vu"}

   {:k "ヴェ" :r "ve" :h "ゔぇ"} {:k "ヴォ" :r "vo" :h "ゔぉ"}

   {:k "ヴャ" :r "vya" :h "ゔゃ"} {:k "ヴュ" :r "vyu" :h "ゔゅ"}

   {:k "ヴィェ" :r "vye" :h "ゔぃぇ"} {:k "ヴョ" :r "vyo" :h "ゔょ"}

   {:k "キェ" :r "kye" :h "きぇ"}

   {:k "ギェ" :r "gye" :h "ぎぇ"}

   {:k "クァ" :r "kwa" :h "くぁ"} ; order counts - suggested by Japan's CJMECSST
   {:k "クィ" :r "kwi" :h "くぃ"} {:k "クェ" :r "kwe" :h "くぇ"}
   {:k "クォ" :r "kwo" :h "くぉ"}

   {:k "クヮ" :r "kwa" :h "くゎ"} ; order counts - suggested by US's ANSI

   {:k "グァ" :r "gwa" :h "ぐぁ"} ; order counts - suggested by Japan's CJMECSST
   {:k "グィ" :r "gwi" :h "ぐぃ"} {:k "グェ" :r "gwe" :h "ぐぇ"}
   {:k "グォ" :r "gwo" :h "ぐぉ"}

   {:k "グヮ" :r "gwa" :h "ぐゎ"} ; order counts - suggested by US's ANSI

   {:h "しぇ" :k "シェ" :r "she" :ks "sye"}

   {:h "じぇ" :k "ジェ" :r "je" :ks "zye"}

   {:k "スィ" :r "si" :h "すぃ"}

   {:k "ズィ" :r "zi" :h "ずぃ"}

   {:k "チェ" :r "che" :ks "tye" :h "ちぇ"}

   {:k "ツァ" :r "tsa" :h "つぁ"} {:h "つぃ" :k "ツィ" :r "tsi"}

   {:h "つぇ" :k "ツェ" :r "tse"} {:h "つぉ" :k "ツォ" :r "tso"}

   {:k "ツュ" :r "tsyu" :h "つゅ"}

   {:k "ティ" :r "ti" :h "てぃ"} {:k "トゥ" :r "tu" :h "とぅ"}

   {:k "テュ" :r "tyu" :h "てゅ"}

   {:k "ディ" :r "di" :h "でぃ"} {:k "ドゥ" :r "du" :h "どぅ"}

   {:k "デュ" :r "dyu" :h "でゅ"}

   {:k "ニェ" :r "nye" :h "にぇ"}

   {:k "ヒェ" :r "hye" :h "ひぇ"}

   {:k "ビェ" :r "bye" :h "びぇ"}

   {:k "ピェ" :r "pye" :h "ぴぇ"}

   {:k "ファ" :r "fa" :h "ふぁ"} {:k "フィ" :r "fi" :h "ふぃ"}
   {:k "フェ" :r "fe" :h "ふぇ"} {:k "フォ" :r "fo" :h "ふぉ"}

   {:k "フャ" :r "fya" :h "ふゃ"} {:k "フュ" :r "fyu" :h "ふゅ"}
   {:k "フィェ" :r "fye" :h "ふぃぇ"}

   {:k "フョ" :r "fyo" :h "ふょ"}

   {:k "ホゥ" :r "hu" :h "ほぅ"}

   {:k "ミェ" :r "mye" :h "みぇ"}

   {:k "リェ" :r "rye" :h "りぇ"}

   {:k "ラ゜" :r "la" :h "ら゜"} {:k "リ゜" :r "li" :h "り゜"}
   {:k "ル゜" :r "lu" :h "る゜"} {:k "レ゜" :r "le" :h "れ゜"}
   {:k "ロ゜" :r "lo" :h "ろ゜"}

   {:k "ヷ" :r "va" :h "ゔぁ"} {:k "ヸ" :r "vi" :h "ゔぃ"}
   {:k "ヹ" :r "ve" :h "ゔぇ"} {:k "ヺ" :r "vo" :h "ゔぉ"}

    ; kunrei-shiki exceptions
    ; Limited to international relations and situations with prior precedent in
    ; which a sudden spelling reform would be difficult
    ; kunrei->x only
   {:ks "sha" :h "しゃ" :k "シャ"}
   {:ks "shu" :h "しゅ" :k "シュ"} {:ks "sho" :h "しょ" :k "ショ"}

   {:ks "cha" :h "ちゃ" :k "チャ"}
   {:ks "chu" :h "ちゅ" :k "チュ"} {:ks "cho" :h "ちょ" :k "チョ"}

   {:ks "ja" :h "じゃ" :k "ジャ"}
   {:ks "ju" :h "じゅ" :k "ジュ"} {:ks "jo" :h "じょ" :k "ジョ"}

   {:ks "dya" :h "ぢゃ" :k "ヂャ" :r "ja"} {:ks "dyu" :h "ぢゅ" :k "ヂュ" :r "ju"}
   {:ks "dyo" :h "ぢょ" :k "ヂョ" :r "jo"}

   {:ks "kwa" :h "くゎ" :k "クヮ"}

   {:ks "gwa" :h "ぐゎ" :k "グヮ"}

   {:ks "wo" :h "を" :k "ヲ" :r "o" :ho "" :ko ""}

    ; ヶ choose from below which version to use, according to use frequency
    ;{:k "ヶ" :h "か" :r "ka"} ; for counter - tatoeba 一ヶ月
    ;{:k "ヶ" :h "が" :r "ga"} ; for conjuntive particle が
    ;{:k "ヶ" :h "こ" :r "ko"} ; for counter too
    ; ヵ
    ;{:h "か" :k "ヵ" :r "ka"} ; for counter sometimes when pronounced "ka"

    ;other
    ; order counts - ぢ syllab forms must go after じ forms above
   {:k "ヂェ" :r "je" :ks "zye" :h "ぢぇ"} ; ぢぇ/ヂェ => je/zye
   {:k "クョ" :r "kyo" :h "くょ"} ; "kyo" duplicate; cannot input クョ -> low priority 
   {:k "グョ" :r "gyo" :h "ぐょ"} ; "gyo" duplicate; cannot input グョ -> low priority
   ])

(defonce long-vowel-symbols
  [{:o "a" :w "aa" :r "ā" :ks "â" :h "あ" :k "ー"}
   {:o "i" :w "ii" :r "ī" :ks "î" :h "い" :k "ー"}
   {:o "e" :w "ee" :r "ē" :ks "ê" :h "え" :k "ー"}
   {:o "o" :w "ou" :r "ō" :ks "ô" :h "う" :k "ー"} ; first
   {:o "o" :w "oo" :r "ō" :ks "ô" :h "お" :k "ー"} ; second
   {:o "u" :w "uu" :r "ū" :ks "û" :h "う" :k "ー"}
   {:o "A" :w "AA" :r "Ā" :ks "Â" :h "あ" :k "ー"}
   {:o "I" :w "II" :r "Ī" :ks "Î" :h "い" :k "ー"}
   {:o "U" :w "UU" :r "Ū" :ks "Û" :h "う" :k "ー"}
   {:o "E" :w "EE" :r "Ē" :ks "Ê" :h "え" :k "ー"}
   {:o "O" :w "OU" :r "Ō" :ks "Ô" :h "う" :k "ー"}]) ; third

(defn concatv
  "Concatenate `xs` and return the result as a vector."
  [xs]
  (reduce into [] xs))

(defn get-first [coll & ks]
  (loop [ks' ks]
    (if (empty? ks')
      nil
      (if (contains? coll (first ks'))
        (get coll (first ks'))
        (recur (rest ks'))))))

(defonce VOWELS (set "あいえうお"))

(defonce doubled-consonants
  (for [syl syllabary
        :let [h (:h syl)]
        :when (and h (not (contains? VOWELS h)))
        :let [r (get-first syl :r :ks)
              h' (str "っ" h)
              k (str "ッ" (:k syl))]]
    (assoc syl :h h' :k k :r (str (first r) r))))

(defonce kata-skip (set "ンヿ"))

(defonce long-vowels
  (for [syl (concat syllabary doubled-consonants)
        :let [k (:k syl)]
        :when (and k (not (kata-skip k)))
        :let [r (get-first syl :r :ks)
              k (str k "ー")]]
    {:k k :r (str r (last r))}))

(defonce sym-syl
  (concatv (for [syl (concat syllabary doubled-consonants long-vowels)
                 :when (:k syl)] 
    (let [syl' (assoc syl :ks (get-first syl :r :ks))] 
      (if (:h syl')
        [[(:h syl') syl'] [(:k syl') syl']]
        [[(:k syl') syl']])))))

(defonce map->syl (into {} sym-syl))
(defonce trie->sym (trie/build-trie (keys map->syl)))

(defn get-map [f coll seq]
  (for [x seq]
    (if (contains? coll x)
      (f (get coll x))
      x)))

(defn romanize [txt]
  (->> txt
       (trie/tokenize trie->sym)
       (get-map :ks map->syl) 
       (apply str)))

(deftest test-romanize
  (testing "doubled consonants and vowel extension for katakana"
    (is (= "happiiawaa" (romanize "ハッピーアワー")))))

(defn -main []
  (pprint/pprint trie->sym)
  (pprint/pprint map->syl)
  (t/run-tests 'src.kana))