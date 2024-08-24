(ns src.quiz
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pprint]

   [promesa.core :as p]
   [applied-science.js-interop :as j]
   [nbb.repl :as repl]

   ["child_process$default" :as proc]
   ["inquirer$default" :as inquirer]
   ["chalk$default" :as chalk]
   ["fast-diff$default" :as diff]

   [src.kana :as k]
   [src.num :as num]
   [src.jisho :as jisho]))

(defn am-pm [n]
  (get ["ごぜん" "ごご"] n))

(defn rand-time []
  (let [am-or-pm (rand-int 2)
        h (num/randrange 1 12 1)
        m (num/randrange 0 55 5)
        jh (num/jnumber "hours" h)
        jm (num/jnumber "minutes" m)
        hir (str (am-pm am-or-pm) " " jh " " jm)
        rom (k/romanize hir)]
    {:am-or-pm am-or-pm
     :h h
     :m m
     :eng (pprint/cl-format nil "~d:~2,'0d ~d" h m (get ["a.m." "p.m."] am-or-pm))
     :hir hir
     :rom rom}))

(def exit (atom false))

(defn handle-sigint []
  (println "Exiting...")
  (reset! exit true))

(defn prompt [questions]
  (p/catch (inquirer/prompt (clj->js questions))
           (fn [error]
             (handle-sigint))))

(defn strip-whitespace [s]
  (str/replace s #"\s+" ""))

; https://teamjapanese.com/stop-in-japanese/
(defonce Q ["quit" "stop" "exit" "q" "yamete" "yamero" "tomare" "mouii" "sutoppu"])

(defn in? [xs el] (some #(= % el) xs))

(defn sum [xs]
  (reduce + xs))

(defn mean [xs]
  (/ (sum xs) (count xs)))

(defn ne-change [change] (and (not= (get change 0) 0)
                              (not (str/blank? (get change 1)))))

(defn min-diff [diffs]
  (apply min-key
         (fn [d] (sum (for [change d :when (ne-change change)] (count (get change 1)))))
         diffs))

(defn single-diff [linput answer]
  (let [lanswer (str/lower-case answer)]
    (if (str/includes? "(" lanswer)
      (min-diff [(diff linput (str/replace lanswer #"[()]" ""))
                 (diff linput (str/replace lanswer #"\(.*?\)" ""))])
      (diff linput lanswer))))

(defn diffs [input answer-or-answers]
  (let [linput (str/lower-case input)
        ldiff #(single-diff linput %)]
    (if (string? answer-or-answers)
      (ldiff answer-or-answers)
      (min-diff (map ldiff answer-or-answers)))))

(defn tts [text]
  (proc/execSync (str "say -v Kyoko '" text "'")))

; TODO: handle parentheses
(defn diff-attempt [input answer message]
  ;(println "\n" input answer)
  (let [raw-diff (diffs input answer)
        stripped-diff (filter ne-change raw-diff)]
    (cond ; no diff
      (== 0 (count stripped-diff)) true
          ; show answer
      (in? ["idk" "?" "skip"] input) (do (println "\n" answer) true)
          ; should quit
      (in? Q input) (do (reset! exit true) true)
      (in? ["rom" "romanize" "romanji"] input) (k/romanize message)
      (in? ["say" "speak" "aloud" "tts"] input) (do (tts message) "")
      :else
      (apply str (for [change raw-diff
                       :let [code (get change 0)
                             tokens (get change 1)]]
                   (case code
                     0  (chalk/green tokens)
                     1  (->> "?" (repeat (count (strip-whitespace tokens))) (apply str) chalk/italic chalk/yellow)
                     -1 (-> tokens chalk/red chalk/strikethrough)))))))

(defn quiz-loop [target message]
  (p/let [start (.now js/Date)
          attempts (atom 0)
          answers (prompt [{:name "attempt"
                            :type "input"
                            :message message
                            :validate (fn [input answers]
                                        (swap! attempts inc)
                                        (diff-attempt input target message))}])
          end (.now js/Date)]
    {:target target
     :message message
     :duration (/ (- end start) 1000)
     :reattempts (dec @attempts)}))

(defn quiz-numbers []
  (let [num (num/randnum 0 4)
        eng (str num)
        hir (num/jnumber "*" num)
        eng->jap? (zero? (rand-int 2))
        target (if eng->jap? (k/romanize hir) eng)
        msg (if eng->jap? (str "Translate to Japanese: " eng) (str "Translate to English: " (strip-whitespace hir)))]
    (quiz-loop target msg)))

(defn quiz-phone []
  (let [digits->str (fn [digits dsep gsep] (str/join gsep (map (fn [r] (str/join dsep (apply subvec digits r))) [[0 3] [3 7] [7 11]])))
        digits (->> #(rand-int 10) (repeatedly 11) vec)
        eng (digits->str digits "" "-")
        digits-hir (vec (map #(get num/DIGITS %) digits))
        hir (digits->str digits-hir " " " の ")
        eng->jap? (zero? (rand-int 2))
        target (if eng->jap? (k/romanize hir) eng)
        msg (if eng->jap? (str "Translate to Japanese: " eng) (str "Translate to English: " (strip-whitespace hir)))]
    (quiz-loop target msg)))

(defn quiz-time []
  (let [jtime (rand-time)]
    (quiz-loop (:rom jtime) (str "Translate to Japanese: " (:eng jtime)))))

(defn dups
  [f seq]
  (->> seq
       (group-by f)
       ; filter out map entries where its value has only 1 item 
       (remove #(or (nil? (key %)) (= 1 (count (val %)))))))

(defn quiz-terms [terms]
  (let [hir-dups (dups :hir terms)
        eng-dups (dups :eng terms)]
    (when (not-empty hir-dups)
      (print "Duplicates found: ")
      (pprint/pprint hir-dups))
    (when (not-empty eng-dups)
      (print "Duplicates found: ")
      (pprint/pprint eng-dups)))
  (fn []
    (let [questions (for [obj terms
                          :let [eng (:eng obj)
                                jap (or (:kat obj) (:hir obj))]
                          e2j? [true false]]
                      (if e2j?
                        [(str "Translate to Japanese: " (if (string? eng) eng (str/join "/" eng))) (k/romanize jap)]
                        [(str "Translate to English: " jap) eng]))
          log (transient [])]
      (p/loop [curr-questions (shuffle questions) next-questions []]
        (cond
          (not-empty curr-questions) (p/let [[msg target] (first curr-questions)
                                             res (quiz-loop target msg)]
                                       (if @exit
                                         (persistent! log)
                                         (do
                                           (conj! log res)
                                           (if (zero? (:reattempts res))
                                             (p/recur (rest curr-questions) next-questions)
                                             (p/recur (rest curr-questions) (conj next-questions [msg target]))))))
          (not-empty next-questions) (p/recur (shuffle next-questions) [])
          :else (persistent! log))))))

(defonce OBJECTS
   [{:eng "business card" :hir "めいし" :counter "sheets"}
    {:eng "umbrella" :hir "かさ" :counter "poles"}
    {:eng "book" :hir "ほん"}
    {:eng "file" :kat "ファイル" :counter "sheets"}
    {:eng "glasses" :hir "めがね"}
    {:eng "smart phone" :kat "スマホ"}
    {:eng "key" :hir "かぎ"}
    {:eng "wallet" :hir "さいふ"}
    {:eng "pen" :kat "ペン" :counter "poles"}
    {:eng "ballpoint pen" :kat "ボールペン" :counter "poles"}
    {:eng "mechnical pencil" :kat "シャーペン" :counter "poles"}
    {:eng "telephone" :hir "でんわ"}
    {:eng "bag" :hir " かばん"}
    {:eng ["shopping bag" "sack" "pounch"] :hir "ふくろ" :kan "袋"}
    {:eng ["watch" "clock"] :hir "とけい"}
    {:eng "(credit) card" :kat "カード" :counter "sheets"}
    {:eng "television" :kat "テレビ"}
    {:eng "computer" :kat "パソコン"}
    {:eng "refrigerator" :hir "れいぞうこ" :counter "machines"}
    {:eng "air conditioner" :kat "エアコン" :counter "machines"}
    {:eng "microwave oven" :hir "でんし れんじ"}
    {:eng "toaster" :kat "トースター"}
    {:eng "tablet" :kat "タブレット"}
    {:eng ["T-shirt" "tee"] :kat "T シャツ" :counter "sheets"}
    {:eng "vacuum (cleaner)" :hir "そうじき"}
    {:eng "shoes" :hir "くつ"}
    {:eng "camera" :kat "カメラ"}
    {:eng "towel" :kat "タオル"}

    {:eng "plate" :hir "さら" :counter "sheets"} ; often used with お
    {:eng "glass" :kat "コップ"}
    {:eng "mug" :kat "マグカップ"}
    {:eng "coffee cup" :kat "コーヒーカップ"}
    {:eng "apple" :hir "りんご"}
    {:eng "coffee" :kat "コーヒー"}
    {:eng "juice" :kat "ジュース"}
    {:eng "sandwich" :kat "サンドイッチ"}
    {:eng "(black) tea" :hir "こうちゃ"}
    {:eng "salad" :kat "サラダ"}
    {:eng "curry" :kat "カレー"}
    {:eng "sweets" :hir "かし"}
    {:eng "orange juice" :kat "オレンジジュース"}
    {:eng "chocolate cake" :kat "チョコレートケーキ"}
    {:eng "wine" :kat "ワイン" :counter "poles"}
    {:eng "red wine" :kat "あかワイン" :counter "poles"}
    {:eng "white wine" :kat "しろワイン" :counter "poles"}
    {:eng "beer" :kat "ビール" :counter "poles"}
    {:eng "beef" :hir "ぎゅうにく"}
    {:eng "cheese" :kat "チーズ"}
    {:eng ["orange" "mandarin" "clementine" "satsuma"] :hir "みかん"}
    {:eng "cheesecake" :kat "チーズケーキ"}])

(defonce EVENTS
   [{:eng "day before yesterday" :hir "おととい"}
    {:eng ["last day" "yesterday"] :hir "きのう"}
    {:eng ["this day" "today"] :hir "きょう"}
    {:eng ["next day" "tomorrow"] :hir "あした"}
    {:eng "day after tomorrow" :hir "あさって"}
    
    {:eng "last week" :hir "せんしゅう"}
    {:eng "this week" :hir "こんしゅう"}
    {:eng "next week" :hir "らいしゅう"}
    
    {:eng "last month" :hir "せんげつ"}
    {:eng "this month" :hir "こんげつ"}
    {:eng "next month" :hir "らいげつ"}
    
    {:eng "last year" :hir "きょねん"}
    {:eng "this year" :hir "ことし"}
    {:eng "next year" :hir "らいねん"}
    
    {:eng "lunchtime" :kat "ラチタイム"}
    {:eng "lunch break" :hir "ひるやすみ"}
    {:eng "meeting" :hir "かいぎ"} 
    {:eng "noon" :hir "ひる"}
    {:eng ["break" "time off"] :hir "やすみ"}
    {:eng "party" :kat "パーティー"}
    {:eng "happy hour" :kat "ハッピーアワー"}
    {:eng "presentation" :kat "プレゼン"}
    {:eng "concert" :kat "コンサート"}
    {:eng "breakfast" :hir "あさごはん"}
    {:eng "morning" :hir "あさ"}
    {:eng ["meal" "cooked rice"] :hir "ごはん"}
    {:eng ["dinner" "supper"] :hir "ばんごはん"}
    {:eng "evening" :hir "ばん"}

    {:eng "am" :hir "ごぜん"}
    {:eng "pm" :hir "ごご"}

    {:eng "now" :hir "いま"}

    {:eng "Sunday" :hir "にちようび"}
    {:eng "Monday" :hir "げつようび" :lit "moon day"}
    {:eng "Tuesday" :hir "かようび"}
    {:eng "Wednesday" :hir "すいようび"}
    {:eng "Thursday" :hir "もくようび"}
    {:eng "Friday" :hir "きんようび"}
    {:eng "Saturday" :hir "どようび"}

    {:eng "January" :hir (num/jnumber "month" 1)}
    {:eng "February" :hir (num/jnumber "month" 2)}
    {:eng "March" :hir (num/jnumber "month" 3)}
    {:eng "April" :hir "しがつ"}
    {:eng "May" :hir (num/jnumber "month" 5)}
    {:eng "June" :hir (num/jnumber "month" 6)}
    {:eng "July" :hir (num/jnumber "month" 7)}
    {:eng "August" :hir (num/jnumber "month" 8)}
    {:eng "September" :hir (num/jnumber "month" 9)}
    {:eng "October" :hir (num/jnumber "month" 10)}
    {:eng "November" :hir (num/jnumber "month" 11)}
    {:eng "December" :hir (num/jnumber "month" 12)}

    {:eng "business trip" :hir "しゅっちょう"}
    {:eng "birthday" :hir "たんじょうび"}
    {:eng "summer vacation" :hir "なつやすみ"}
    {:eng "festival" :hir "まつり"} ; used with お
    {:eng "trip" :hir "りょこう"}
    ])

(defonce LOCATIONS
  [{:eng "station" :hir "えき"}
   {:eng ["branch" "regional office"] :hir "ししゃ"}
   {:eng "park" :hir "こうえん"}
   {:eng ["home" "house"] :hir "うち"}
   {:eng "airport" :hir "くうこう"}
   {:eng "department store" :kat "デパート"}
   {:eng "bank" :hir "ぎんこう"}
   {:eng ["university" "college"] :hir "だいがく"}
   {:eng "embassy" :hir "たいしかん"}
   {:eng ["restaurant" "shop"] :hir "みせ"}
   {:eng ["mart" "supermarket"] :hir "スーパー"}
   {:eng "restaurant" :kat "レストラン"}
   {:eng "gym" :kat "ジム"}
   {:eng "(swimming) pool" :kat "プール"}
   {:eng "wine store" :kat "ワインショップ"}
   {:eng "information desk" :kat "インフォメーション"}
   {:eng "first-floor basement" :hir "“ちか いっかい"}
   {:eng "basement" :hir "ちか"}
   {:eng ["bathroom" "restroom"] :hir "おてあらい"}
   {:eng "hotel" :kat "ホテル"}

   ; means of transportation
   {:eng "bus" :kat "バス"}
   {:eng "airplane" :hir "ひこうき"}
   {:eng "superexpress train" :hir "しんかんせん"}
   {:eng "train" :hir "でんしゃ"}
   {:eng "subway" :hir "ちかてつ"}
   {:eng "car" :hir "くるま"}
   {:eng "taxi" :kat "タクシー"}
   {:eng ["motorbike" "motorcycle"] :kat "バイク"}
   {:eng "bycicle" :hir "じてんしゃ"}
   {:eng ["on foot" "walking"] :hir "あるいて"}

   {:eng "(South) Korea" :hir "かんこく"}
   {:eng "Japan" :hir "にほん"}
   {:eng "China" :hir "ちゅうごく"}
   {:eng "Germany" :kat "ドイツ"}
   {:eng ["England" "UK" "United Kingdom"] :kat "イギリス"}
   {:eng ["America" "US(A)"] :kat "アメリカ"}
   {:eng "Austrailia" :kat "オーストラリア"}
   {:eng "India" :kat "インド"}
   {:eng "France" :kat "フランス"}
   {:eng "Italy" :kat "イタリア"}
   {:eng "Switzerland" :kat "スイス"}

   {:eng "Tokyo" :hir "とうきょ"}
   {:eng "Yokohama" :hir "よこはま"} ; city near Tokyo
   {:eng "Ginza" :hir "ぎんざ"} ; district in Tokyo 
   {:eng "Shibuya" :hir "しぶや"}
   {:eng "Kyoto" :hir "きょうと"}
   {:eng "Hokkaido" :hir "ほっかいどう"} ; island in northern Japan
   {:eng "Osaka" :hir "おおさか"} ; western japan

   {:eng "Berlin" :hir "ベルリン"}
   {:eng "London" :hir "ロンドン"}
   {:eng "Seoul" :hir "ソウル"}
   {:eng "Hong Kong" :kat "ホンコン"}

   {:eng "company name" :hir "かいしゃの なまえ"}
   {:eng "address" :hir "じゅうしょ"}
   {:eng "phone number" :hir "でんわばんごう"}
   {:eng "mail(ing) address" :kat "メールアドレス"}
   {:eng "reception (desk)" :kat "フロント"}])

(defonce PEOPLE
  [{:eng "friend" :hir "ともだち"}
   {:eng "family" :hir "かぞく"}

   {:eng ["colleague" "coworker"] :hir "どうりょう"}
   {:eng ["boss" "superior"] :hir "じょうし"}
   {:eng "student" :hir "がくせい"}
   {:eng "intern" :kat "インターン"}
   {:eng "driver" :hir "うんてんしゅ"}
   {:eng "restaurant employee" :hir "みせの ひと"}

   {:eng "Allen" :kat "アレン"}
   {:eng "Smith" :kat "スミス"}
   {:eng "Tanaka" :hir "たなか"}
   {:eng "Hoffman" :kat "ホフマン"}
   {:eng "Brown" :kat "ブラウン"}
   {:eng "Chan" :kat "チャン"}
   {:eng "Harris" :kat "ハリス"}
   {:eng "Park" :kat "パク"}
   {:eng "Lopez" :kat "ロペス"}])

(defonce ADJECTIVES
  {:size [{:eng ["big" "large"] :hir "おうきい"}
          {:eng ["small" "little"] :hir "ちいさい"}]
   :color [{:eng "red" :hir "あかい"}
           {:eng "white" :hir "しろい"}
           {:eng "black" :hir "くろい"}]})

(defonce VERBS
  [{:eng "go" :hir "いく"}
   {:eng "come" :hir "くる" :n 5}
   {:eng ["return" "come back"] :hir "かえります"}])

(defonce quiz-expressions
  (quiz-terms
    [{:eng ["have a good trip" "have a good day"] :hir "いってらっしゃい"}
     {:eng "nice to meet you" :hir "はじめまして"}
     {:eng ["it was an honor" "nice to meet you" "thank you for your time"] :hir "よろしく おねがいします" :info "usually combined with はじめまし て. It is also used when taking one’s leave after having asked a favor. よろしく means “well” and is used as a request for the other person’s favorable consideration in the future."}
     {:eng "good morning" :hir "おはようございます"}
     {:eng ["goodbye" "I must go now" "I'm off"] :hir "しつれいします" :info "a form of “good-bye” when hanging up the phone or leaving a house or room. It is also used when entering a house or room, passing in front of someone, leaving in the middle of a gathering, and so on, to mean “excuse me.”"}

     {:eng "that's right" :hir "そうです"}
     {:eng ["I see" "is that so"] :hir "そうですか"} ; spoken with falling intonation
     {:eng "same here" :hir "こちらこそ"}
     {:eng "where are you from?" :hir "おくには どちらですか"}
     {:eng ["idk" "I don't know"] :hir "わかりません"}
     {:eng "thank you" :hir "ありがとうございます" :f :polite}
     {:eng "thank you very much" :hir "どうも ありがとうございます" :f :polite}
     {:eng "thanks" :hir "ありがとう" :f :casual}
     {:eng "thanks so much" :hir "どうも ありが とう" :f :casual}
     {:eng "please tell me" :hir "〜を おしえてください"}
     {:eng "one more time please" :hir "もう いちど おねがいします" :f :polite}

     {:eng ["last order" "last call"] :kat "ラストオーダー"}
     {:eng ["may I help you" "welcome"] :hir "いらっしゃいませ"}
     {:eng ["please show me" "may I see"] :hir "みせてください"}

     {:eng "alone" :hir "ひとりで"}

     ; question words: https://guidetojapanese.org/learn/complete/questionwords
     {:eng "who" :hir "だれ" :kan "誰" :info "attach の to ask whose"}
     {:eng "what" :hir ["なに" "なん"] :kan "何"}
     ;どうして – why
     ;なんで – why (casual)
     ;なぜ – why (formal)
     {:eng "how many" :hir "いくつ"}
     {:eng "how much" :hir "いくら"}

     ; The following question words can be used with 「も」 to include and/or exclude everything.
     ;誰も 【だれ・も】 – everybody or nobody when used with negative, (N5)
     ;何も 【なに・も】 – nothing/not anything/not any kind of when used with negative (N5)
     ;どこも – everywhere or nowhere when used with negative (N5)
     ;どうしても – no matter what
     ;どちらも – both ways
     ;いつも – always
     ; Things aren’t as consistent as one would hope however. For example, 「何も」 is usually not used to mean “everything”. And 「いつも」 always means “always” for both positive and negative forms. Other words can be used instead to express similar concepts.
     ;皆 【みんな】 – everybody
     ;皆さん 【みな・さん】 – everybody (polite)
     ;全部 【ぜん・ぶ】 – everything
     ;全然　【ぜん・ぜん】 – not at all (when used with negative)
     ;絶対 【ぜっ・たい】 – absolutely, unconditionally or never when used with negative

     ; The combination of two particles 「でも」 can be used with question words to indicate “any”.
     {:eng "anybody" :hir "だれ・でも" :kan "誰でも"}
     {:eng "anything" :hir "なん・でも" :kan "何でも"}
     {:eng "anywhere" :hir "どこでも"}
     {:eng "any amount" :hir "いくらでも"}
     {:eng "anyhow" :hir "どうでも"}
     {:eng "any way" :hir "どちらでも"}
     {:eng "any time" :hir "いつでも"}
     {:eng "any number of things" :hir "いくつでも"}

     ; The question marker can also be used with some question words to indicate “some”.
     {:eng ["somebody" "someone"] :hir "だれ・か" :kan "誰か" :n 5}
     {:eng ["something" "some kind of"] :hir "なに・か" :kan "何か" :n 5}
     ;どうしてか – for some reason
     ;なんでか – for some reason (casual)
     ;なぜか – for some reason (formal)
     {:eng "somewhere" :hir "どこか" :n 5}
     {:eng "some amount" :hir "いくらか"}
     {:eng "somehow" :hir "どうか"}
     {:eng "one way (of the two)" :hir "どちらか"}
     {:eng "sometime" :hir "いつか"}
     {:eng "some number of things" :hir "いくつか"}

     ; extent
     {:eng ["very" "too much"] :hir "すぎる" :n 5}
     {:eng ["quite" "rather" "fairly"] :hir "けっこう" :n 5} 
     {:eng ["completely" "exactly" "right"] :kan "真" :hir "っ" :n 4}
     {:eng ["(not) at all"] :hir "ぜんぜん" :n 4}
     {:eng ["hardly possible" "cannot be" "highly unlikely" "improbable"] :hir "はずがない" :n 4}
     {:eng ["might" "maybe" "probably"] :hir "かもしれない" :n 4}

     ; qualifiers
     {:eng ["almost all" "most" "hardly any" "few"] :pos :adverbial-noun :hir "ほとんど" :n 4}
     {:eng ["many" "a lot of" "plenty" "enough"] :hir "たくさん" :n 5}
     {:eng ["only" "just"] :hir "だけ" :n 5}
     {:eng ["still" "not yet"] :hir "まだ" :n 5}
     {:eng ["already" "anymore"] :hir "もう" :n 5} 
     {:eng ["gradually" "little by little" "step by step"] :hir "だんだん" :n 4}
     {:eng ["progressively" "rapidly increasing" "more and more"] :hir "どんどん" :n 4} 
     {:eng ["just by" "just with"] :hir "だけで" :n 4}
     {:eng ["around" "about"] :hir "ごろ" :n 4}
     {:eng ["most" "mostly" "adequately" "generally" "for the most part" "roughly" "approximately" "in the first place"] :hir "だいたい" :n 4}
     {:eng ["less than (or equal to)" "under" "blow" "any less" "fewer" "not exceeding"] :hir "いか" :n 4}
     {:eng ["except" "besides" "other than" "with the exception of"] :hir "いがい" :n 4}
     {:eng ["each" "every" "respective" "various"] :hir "各" :n 4}
     {:eng ["each" "every" "at intervals of"] :hir "ごとに" :n 4}
     {:eng ["more than" "over"] :hir "より" :n 4}

     ; event sequence
     {:eng ["after" "later"] :hir "あとで" :n 4}
     {:eng ["when" "from when" "just when" "if"] :hir "たら" :n 4}
     {:eng ["while" "during" "as" "in the process of"] :hir "ながら" :n 4}
     {:eng ["just did" "something just happened"] :hir "たばかり" :n 4}

     ; connectors (particles etc) 
     {:eng ["because" "so" "since" "the reason being"] :hir "ので" :n 5}
     {:eng ["because" "so" "since" "the reason being" "from"] :hir "から" :n 5}
     {:eng ["before" "in front of"] :hir "まえに" :n 5}
     {:eng ["but" "although"] :hir "けれども" :n 5}
     {:eng ["but" "however"] :hir "けど" :n 5}
     {:eng ["but" "however"] :hir "だけど" :n 5}
     {:eng ["but" "however"] :hir "が" :n 5}
     {:eng ["but" "still" "however" "nevertheless"] :hir "だが" :n 4}
     {:eng ["but" "still" "however" "nevertheless"] :hir "ですが" :n 4}
     {:eng ["so that" "in order to" "in such a way that"] :hir "ように" :n 4} 
     {:eng ["because of that" "so?" "and then?" "therefore" "with that"] :hir "それで" :n 4}
     {:eng ["but still" "and yet" "even so" "nevertheless"] :hir "それでも" :n 4}
     {:eng ["in" "among" "within"] :hir "のなかで" :n 4}
     {:eng ["in the event of" "in the case of"] :hir "ばあいは" :n 4}
     {:eng ["when" "at the time of"] :hir "とき" :n 4}
     {:eng ["even" "to even" "to the extent of"] :hir "まで" :n 4}
     {:eng ["by" "by the time that" "before"] :hir "までに" :n 4} 
     {:eng ["even if" "even though"] :hir "ても" :n 4}
     {:eng ["despite" "although" "even though"] :hir "のに" :n 4}

     ; lists, conjunctions
     {:eng "and" :hir "と" :n 5}
     {:eng ["also" "too" "as well" "even" "either" "neither"] :hir "も" :n 5}
     {:eng "or" :hir "か" :n 5}
     {:eng ["things like" "and the like"] :hir "や" :n 5}
     {:eng "with" :hir "と" :n 5}
     {:eng ["also" "as well" "moreover" "again" "additionally"] :hir "また" :n 4}
     {:eng ["among other things" "for example" "such as"] :hir "とか" :n 4}

     ; sequencing connectors
     {:eng ["after doing" "and once that's done" "and after that" "and once that happens"] :hir "てから" :n 5} 
     {:eng ["and" "besides" "moreover" "in addition"] :hir "それに" :n 4}
     {:eng ["to start with" "firstly"] :hir "まず" :n 4}
     {:eng ["for example"] :hir "たとえば" :n 4}
     {:eng ["finally" "after all"] :hir "とうとう" :n 4}

     ; qualifiers (adverbs etc)
     {:eng ["is alright" "is fine" "is okay even if" "can" "may" "is also okay"] :hir "てもいい" :n 5}
     {:eng ["must do" "have to do"] :hir "なくてはいけない" :n 5}
     {:eng ["must do" "have to do"] :hir "なくてはならない" :n 5}
     {:eng ["must not" "may not"] :hir "てはいけない" :n 5}
     {:eng ["should do" "it'd be better to"] :hir "たほうがいい" :n 5}
     {:eng ["shouldn't do" "it'd be better not to"] :hir "ないほうがいい" :n 5}
     {:eng ["must do" "have to do"] :hir "なくちゃ・なきゃ" :n 5}
     {:eng ["want to do"] :hir "たい" :n 5}
     {:eng ["like doing" "love doing"] :hir "のがすき" :n 5}
     {:eng ["like" "fond of"] :kat "好き" :n 5}
     {:eng ["dislike" "not fond of"] :hir "きらい" :n 5}
     {:eng "have done before" :hir "たことがある" :n 5}
     {:eng ["won't you" "would you not" "why don't we"] :hir "ませんか" :n 5}
     {:eng ["don't have to"] :hir "なくてもいい" :n 4}
     {:eng ["I think"] :hir "とおもう" :n 4}
     {:eng ["you could say" "you might say"] :hir "といってもいい" :n 4}
     {:eng ["I'm sorry for"] :hir "てすみません" :n 4}
     {:eng ["I hope" "I wish" "you should" "it would be good"] :hir "といい" :n 4}
     {:eng ["on the verge of" "about to"] :hir "るところだ" :n 4}
     {:eng ["easy to" "likely to"] :hir "やすい" :n 4}
     {:eng ["difficult to" "hard to"] :hir "にくい" :n 4}
     {:eng ["need" "necessary"] :hir "がひつよう" :n 4}
     {:eng ["why don't you" "what if you did"] :hir "たらどう" :n 4}
     
     ; requests
     {:eng ["please do"] :hir "てください" :f :polite :n 5}
     {:eng ["please don't"] :hir "ないでください" :f :polite :n 5}

     {:eng ["isn't it?" "right?"] :hir "ね" :n 5 :pos :sentence-ending-particle}
     {:eng ["definitely" "for sure"] :hir "よ" :n 5 :pos :sentence-ending-particle :info "emphasis"}

     {:eng ["well then" "okay then"] :hir "じゃ"}
     {:eng "no" :hir "いいえ"}
     {:eng "excuse me" :hir "すみません"}
     {:eng ["ah" "oh"] :hir "あ" :info "utterance expressing having just noticed something. It is also used to get someone’s attention"}
     {:eng ["yep" "yes"] :hir "ええ" :info "a softer way of saying はい"}
     {:eng ["uhh" "um"] :hir "さあ" :info "expresses the speaker’s hesitation about immediately answering"}
     {:eng "well then" :hir "では"}
     {:eng "huh" :hir "え" :info "utterance expressing uncertainty about what one has heard. It may be used when one is surprised"}]))

(defonce PREFIXES
  [])

(defonce SUFFIXES
  [{:eng ["-ity", "-ness"] :hir "さ" :attaches-to :adj :n 4 :info "converts adjectives to nouns"}
   {:eng "-ation" :hir "こと" :n 4 :attaches-to :verb :info "converts verbs to nouns"}
   {:eng ["-style" "way" "fashion"] :kan "風" :n 4 :attaches-to :noun}
   {:eng ["way of", "how to", "manner of", "method of"] :hir "かた" :n 4 :attaches-to :noun}])

(defn quiz-jisho-tag [tag]
  (fn []
    (p/let [terms (jisho/get-terms tag)
            quiz (quiz-terms (vec terms))]
      (quiz))))

; TODO: kanji readings
; TODO: verb conjugations
(defonce quizzes {"time" quiz-time
                  "phone numbers" quiz-phone
                  "numbers 1-100000" quiz-numbers
                  "vocab" (quiz-terms (concat OBJECTS PEOPLE LOCATIONS EVENTS SUFFIXES))
                  "expressions" quiz-expressions
                  "counters" (quiz-terms (num/num-terms))
                  "JLPT N5 vocab" (quiz-jisho-tag "jlpt-n5")})

; TODO: fix stats calculation
; TODO: fix errors when exiting during quiz selection
(defn -main []
  (println "Press Ctrl+C to exit.")
  ;(.on js/process "SIGINT" handle-sigint)
  (p/let [selection (prompt {:name "choice"
                             :type "list"
                             :message "What do you want to practice"
                             :choices (keys quizzes)})
          quiz (get quizzes (.-choice selection))
          is-list? (in? ["objects" "events" "locations" "expressions" "JLPT N5 vocab"] (.-choice selection))
          log (atom [])]
    (p/loop []
      (p/let [res (quiz)]
        (if (not @exit)
          (do
            (if is-list?
              (swap! log into res)
              (swap! log conj res))
            (p/recur))
          
          (println {:questions (count @log)
                    :avg-mistakes (sum (for [l @log] (:reattempts l)))
                    :avg-time (mean (for [l @log] (:duration l)))}))))))