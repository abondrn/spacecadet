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

(defn quiz-terms [terms]
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

    {:eng "bus" :kat "バス"}
    {:eng "airplane" :hir "ひこうき"}
    {:eng "superexpress train" :hir "しんかんせん"}

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
    
    {:eng "Friday" :hir "きんようび"}
    
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
   {:eng "Harris" :kat "ハリス"}])

(defonce ADJECTIVES
  {:size [{:eng ["big" "large"] :hir "おうきい"}
          {:eng ["small" "little"] :hir "ちいさい"}]
   :color [{:eng "red" :hir "あかい"}
           {:eng "white" :hir "しろい"}
           {:eng "black" :hir "くろい"}]})

(defonce VERBS
  [{:eng "go" :hir "いく"}
   {:eng "come" :hir "くる"}])

(defonce quiz-expressions
  (quiz-terms
    [{:eng ["have a good trip" "have a good day"] :hir "いってらっしゃい"}
     {:eng "nice to meet you" :hir "はじめまして"}
     {:eng ["it was a pleasure" "nice to meet you" "thank you for your time"] :hir "よろしく おねがいします" :info "usually combined with はじめまし て. It is also used when taking one’s leave after having asked a favor. よろしく means “well” and is used as a request for the other person’s favorable consideration in the future."}
     {:eng "good morning" :hir "おはようございます"}
     {:eng "goodbye" :hir "しつれいします" :info "a form of “good-bye” when hanging up the phone or leaving a house or room. It is also used when entering a house or room, passing in front of someone, leaving in the middle of a gathering, and so on, to mean “excuse me.”"}

     {:eng "that's right" :hir "そうです"}
     {:eng ["I see" "is that so"] :hir "そうですか"} ; spoken with falling intonation
     {:eng "same here" :hir "こちらこそ"}
     {:eng "where are you from?" :hir "おくには どちらですか"}
     {:eng ["idk" "I don't know"] :hir "わかりません"}
     {:eng "thank you" :hir "ありがとうございます"}
     {:eng "thank you very much" :hir "どうも ありがとうございます"}
     {:eng "thanks" :hir "ありがとう"}
     {:eng "thanks so much" :hir "どうも ありが とう"}
     {:eng "please tell me" :hir "〜を おしえてください"}
     {:eng "one more time please" :hir "もう いちど おねがいします"}

     {:eng ["last order" "last call"] :kat "ラストオーダー"}
     {:eng ["may I help you" "welcome"] :hir "いらっしゃいませ"}
     {:eng ["please show me" "may I see"] :hir "みせてください"}

     {:eng "alone" :hir "ひとりで"}

     {:eng "well then" :hir "じゃ"}
     {:eng "no" :hir "いいえ"}
     {:eng "excuse me" :hir "すみません"}
     {:eng ["ah" "oh"] :hir "あ" :info "utterance expressing having just noticed something. It is also used to get someone’s attention"}
     {:eng ["yep" "yes"] :hir "ええ" :info "a softer way of saying はい"}
     {:eng ["uhh" "um"] :hir "さあ" :info "expresses the speaker’s hesitation about immediately answering"}
     {:eng "well then" :hir "では"}]))

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
                  "vocab" (quiz-terms (concat OBJECTS PEOPLE LOCATIONS EVENTS))
                  "expressions" quiz-expressions
                  "counters" (quiz-terms (num/num-terms))
                  "JLPT N5 vocab" (quiz-jisho-tag "jlpt-n5")})

; TODO: fix stats calculation
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