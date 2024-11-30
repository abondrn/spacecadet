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
   [src.jisho :as jisho]
   [src.conjugate :as conjugate]))

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
    {:eng "file" :kat "ファイル" :counter "sheets"}
    {:eng "pen" :kat "ペン" :counter "poles"}
    {:eng "ballpoint pen" :kat "ボールペン" :counter "poles"}
    {:eng "mechnical pencil" :kat "シャーペン" :counter "poles"}
    {:eng "(credit) card" :kat "カード" :counter "sheets"}
    {:eng "refrigerator" :hir "れいぞうこ" :counter "machines"}
    {:eng "air conditioner" :kat "エアコン" :counter "machines"}
    {:eng "tablet" :kat "タブレット" :counter "sheets"}
    {:eng ["T-shirt" "tee"] :kat "T シャツ" :counter "sheets"}
    {:eng "towel" :kat "タオル" :counter "sheets"}

    {:eng "plate" :hir "さら" :counter "sheets"} ; often used with お
    {:eng "wine" :kat "ワイン" :counter "poles"}
    {:eng "red wine" :kat "あかワイン" :counter "poles"}
    {:eng "white wine" :kat "しろワイン" :counter "poles"}
    {:eng "beer" :kat "ビール" :counter "poles"}])

(defonce EVENTS
   [{:eng "lunchtime" :kat "ラチタイム"}
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
   {:eng "information (desk)" :kat "インフォメーション"}
   {:eng "(first-floor) basement" :hir "ちか いっかい"}
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
   {:eng "bicycle" :hir "じてんしゃ"}
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
  [
   {:eng "meet" :hir "あう"}
   {:eng "keep" :hir "あずかる"}
   {:eng "wash" :hir "あらう"}
   {:eng ["exist" "have" "take place"] :hir "ある"}
   {:eng "walk" :hir "あるく"}
   {:eng "say" :hir "いう"}
   {:eng "go" :hir "いく"}
   {:eng "sing" :hir "うたう"}
   {:eng ["put" "place"] :hir "おく"}
   {:eng "send" :hir "おくる"}
   {:eng "swim" :hir "およぐ"}
   {:eng "finish" :hir "おわる"}
   {:eng "buy" :hir "かう"}
   {:eng ["return" "come back"] :hir "かえる"}
   {:eng "take [time]" :hir "かかる"}
   {:eng ["write" "draw"] :hir "かく"}
   {:eng ["lend" "loan"] :hir "かす"}
   {:eng ["do one's best" "persevere"] :hir "がんばる"}
   {:eng ["listen to" "ask"] :hir "きく"}
   {:eng ["turn off" "shut off"] :hir "けす"}
   {:eng "touch" :hir "さわる"}
   {:eng "know" :hir "しる"}
   {:eng "smoke" :hir "タバコをすう"}
   {:eng "live" :hir "すむ"}
   {:eng "sit down" :hir "すわる"}
   {:eng "stand up" :hir "たつ"}
   {:eng "use" :hir "つかう"}
   {:eng "arrive" :hir "つく"}
   {:eng "make" :hir "つくる"}
   {:eng "take a photo" :hir "しゃしんをとる"}
   {:eng ["learn" "take lessons in"] :hir "ならう"}
   {:eng ["suit" "look good on"] :hir "にあう"}
   {:eng "climb" :hir "のぼる"}
   {:eng "drink" :hir "のむ"}
   {:eng ["get on" "take"] :hir "のる"}
   {:eng "enter" :hir "はいる"}
   {:eng "run" :hir "はしる"}
   {:eng ["talk" "speak"] :hir "はなす"}
   {:eng "turn" :hir "まがる"}
   {:eng "wait" :hir "まつ"}
   {:eng "receive" :hir "もらう"}
   {:eng ["rest" "take day off"] :hir "やすむ"}
   {:eng "read" :hir "よむ"}
   {:eng "understand" :hir "わかる"}

   {:eng "open" :hir "あける"}
   {:eng "give" :hir "あげる"}
   {:eng "take a shower" :hir "シャワーをあびる"}
   {:eng ["be" "exist" "stay"] :hir "いる"}
   {:eng ["put in" "add"] :hir "いれる"}
   {:eng ["teach" "tell"] :hir "おしえる"}
   {:eng "get off" :hir "のりる"}
   {:eng "borrow" :hir "かりる"}
   {:eng "close" :hir "しめる"}
   {:eng "eat" :hir "たべる"}
   {:eng "get tired" :hir "つかれる"}
   {:eng "turn on" :hir "つける"}
   {:eng "be careful" :hir "きをつける"}
   {:eng "relay a message" :hir "つたえる"}
   {:eng ["work for" "be employed"] :hir "つめとる"}
   {:eng "be done" :hir "できる"}
   {:eng "leave" :hir "でる"}
   {:eng "deliver" :hir "とどける"}
   {:eng ["stop" "park"] :hir "とめる"}
   {:eng ["sleep" "go to bed"] :hir "ねる"}
   {:eng ["start" "begin"] :hir "はじめる"}
   {:eng "show" :hir "みせる"}
   {:eng ["see" "watch"] :hir "みる"}

   {:eng "come" :hir "くる"}
   {:eng "bring" :hir "もってくる"}
   {:eng "do" :hir "する"}
   {:eng "worry about" :hir "きにする"}
   {:eng "recharge" :hir "じゅうでんする"}
   {:eng "download" :hir "ダウンロードする"}
   {:eng "check" :hir "チェックする"}
   {:eng "oversleep" :hir "ねぼうする"}
   {:eng "report" :hir "ほうこくする"}
   {:eng "make a reservation" :hir "よやくする"} 
   {:eng "have a preparatory meeting" :hir "うちあわせをする"}
   {:eng "exercise" :hir "うんどうをする"}
   {:eng "view cherry blossums" :hir "おはなみをする"}
   {:eng "have a meeting" :hir "かいぎをする"}
   {:eng "shop" :hir "かいものをする"}
   {:eng "make a copy" :hir "コピーをする"}
   {:eng "play golf" :hir "ゴルフをする"}
   {:eng "take a walk" :hir "さんぽをする"}
   {:eng "work" :hir "しごとをする"}
   {:eng "prepare" :hir "じゅんびをする"}
   {:eng "jog" :hir "ジョギングをする"}
   {:eng "have a meal" :hir "しょくじをする"}
   {:eng "ski" :hir "スキーをする"}
   {:eng "stretch" :hir "ストレッチをする"}
   {:eng "snowboard" :hir "スノーボードをする"}
   {:eng "explain" :hir "せつめいをする"}
   {:eng "clean" :hir "そうじおする"}
   {:eng "scuba dive" :hir "ダイビングをする"}
   {:eng "play tennis" :hir "テニスをする"}
   {:eng ["phone" "call"] :hir "でんわをする"}
   {:eng "go for a drive" :hir "ドライブをする"}
   {:eng "have a party" :hir "パーティーをする"}
   {:eng "give a presentation" :hir "プレゼンをする"}
   {:eng "study" :hir "べんきょうをする"}
   {:eng "cook" :hir "りょうりをする"}
   ])

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
     ; Things aren’t as consistent as one would hope however. For example, 「何も」 is usually not used to mean “everything”. And 「いつも」 always means “always” for both positive and negative forms. Other words can be used instead to express similar concepts.
     ;全然　【ぜん・ぜん】 – not at all (when used with negative)
     ;絶対 【ぜっ・たい】 – absolutely, unconditionally or never when used with negative

     ; The combination of two particles 「でも」 can be used with question words to indicate “any”.
     {:eng "anybody" :hir "だれ・でも" :kan "誰でも"}
     {:eng "any amount" :hir "いくらでも"}
     {:eng "any time" :hir "いつでも"}
     {:eng "any number of things" :hir "いくつでも"}

     ; The question marker can also be used with some question words to indicate “some”.
     {:eng ["somebody" "someone"] :hir "だれ・か" :kan "誰か" :n 5}
     {:eng ["something" "some kind of"] :hir "なに・か" :kan "何か" :n 5}
     ;どうしてか – for some reason
     ;なんでか – for some reason (casual)
     ;なぜか – for some reason (formal)
     {:eng "some amount" :hir "いくらか"}
     {:eng "sometime" :hir "いつか"}
     {:eng "some number of things" :hir "いくつか"}

     ; extent
     {:eng ["very" "too much"] :hir "すぎる" :n 5}
     {:eng ["quite" "rather" "fairly"] :hir "けっこう" :n 5} 
     {:eng ["completely" "exactly" "right"] :kan "真" :hir "っ" :n 4}
     {:eng ["(not) at all"] :hir "ぜんぜん" :n 4}
     {:eng ["hardly possible" "cannot be" "highly unlikely" "improbable"] :hir "はずがない" :n 4}
     {:eng ["might" "maybe" "probably"] :hir "かもしれない" :n 4}

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
     {:eng ["also" "as well" "moreover" "again" "additionally"] :hir "また" :n 4}
     {:eng ["among other things" "for example" "such as"] :hir "とか" :n 4}

     ; sequencing connectors
     {:eng ["after doing" "and once that's done" "and after that" "and once that happens"] :hir "てから" :n 5} 
     {:eng ["and" "besides" "moreover" "in addition"] :hir "それに" :n 4}
     {:eng ["to start with" "firstly"] :hir "まず" :n 4}
     {:eng ["for example"] :hir "たとえば" :n 4}
     {:eng ["finally" "after all"] :hir "とうとう" :n 4}
     
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

(defn quiz-verbs []
  (let [verb (rand-nth VERBS)
        form (rand-nth (keys conjugate/FORMS))
        fn (get conjugate/FORMS form)
        group (conjugate/verb-group (:hir verb))
        conjugated (fn (:hir verb) group)]
    (quiz-loop (k/romanize conjugated) (str (:eng verb) " " form))))

; TODO: kanji readings
; TODO: verb conjugations
(defonce quizzes {"time" quiz-time
                  "phone numbers" quiz-phone
                  "numbers 1-100000" quiz-numbers
                  "vocab" (quiz-terms (concat OBJECTS PEOPLE LOCATIONS EVENTS SUFFIXES))
                  "expressions" quiz-expressions
                  "counters" (quiz-terms (num/num-terms))
                  "verbs" quiz-verbs})

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