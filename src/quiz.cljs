(ns src.quiz
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]

   [promesa.core :as p]
   [applied-science.js-interop :as j]
   [nbb.repl :as repl]

   ["child_process$default" :as proc]
   ["inquirer$default" :as inquirer]
   ["chalk$default" :as chalk]
   ["fast-diff$default" :as diff]

   [src.kana :as k]
   [src.num :as num]))

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

(defn ne-change [change] (and (not= (get change 0) 0)
                              (not (str/blank? (get change 1)))))

(defn diffs [input answer-or-answers]
  (if (string? answer-or-answers)
    (diff input answer-or-answers)
    (apply min-key
           (fn [d] (sum (for [change d :when (ne-change change)] (count (get change 1)))))
           (map #(diff input %) answer-or-answers))))

(defn tts [text]
  (proc/execSync (str "say -v Kyoko '" text "'")))

(defn diff-attempt [input answer message]
  ;(println "\n" input answer)
  (let [raw-diff (diffs input answer)
        stripped-diff (filter ne-change raw-diff)]
    (cond ; no diff
      (== 0 (count stripped-diff)) true
          ; show answer
      (in? ["idk" "?"] input) (do (println "\n" answer) true)
          ; should quit
      (in? Q input) (do (reset! exit true) true)
      (in? ["rom"] input) (k/romanize message)
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
        (cond (not-empty curr-questions) (p/let [[msg target] (first curr-questions)
                                                 res (quiz-loop target msg)]
                                           (conj! log res)
                                           (if (zero? (:reattempts res))
                                             (p/recur (rest curr-questions) next-questions)
                                             (p/recur (rest curr-questions) (conj next-questions [msg target]))))
              (not-empty next-questions) (p/recur (shuffle next-questions) [])
              :else (persistent! log))))))

(defonce quiz-objects
  (quiz-terms
   [{:eng "business card" :hir "めいし"}
    {:eng "umbrella" :hir "かさ"}
    {:eng "book" :hir "ほん"}
    {:eng "file" :kat "ファイル"}
    {:eng "glasses" :hir "めがね"}
    {:eng "smart phone" :kat "スマホ"}
    {:eng "key" :hir "かぎ"}
    {:eng "wallet" :hir "さいふ"}
    {:eng "pen" :kat "ペン"}
    {:eng "telephone" :hir "でんわ"}
    {:eng "bag" :hir " かばん"}
    {:eng ["watch" "clock"] :hir "とけい"}
    {:eng "card" :kat "カード"}
    {:eng "ballpoint pen" :kat "ボールペン"}

    {:eng "television" :kat "テレビ"}
    {:eng "computer" :kat "パソコン"}
    {:eng "refrigerator" :hir "れいぞうこ"}
    {:eng "air conditioner" :kat "エアコン"}
    {:eng "microwave oven" :hir "でんし れんじ"}
    {:eng "toaster" :kat "トースター"}

    {:eng "coffee" :kat "コーヒー"}
    {:eng "juice" :kat "ジュース"}
    {:eng "sandwich" :kat "サンドイチ"}
    {:eng "black tea" :hir "こうちゃ"}
    {:eng "salad" :kat "サラダ"}]))

(defonce quiz-events
  (quiz-terms
   [{}]))

(defonce quiz-locations
  (quiz-terms
   []))

(defonce quiz-expressions
  (quiz-terms
   []))

(defn -main []
  (println "Press Ctrl+C to exit.")
  ;(.on js/process "SIGINT" handle-sigint)
  (p/let [selection (prompt {:name "choice"
                             :type "list"
                             :message "What do you want to practice"
                             :choices ["time"
                                       "phone numbers"
                                       "numbers 1-100000"
                                       "objects"]})
          quiz (case (.-choice selection)
                 "time" quiz-time
                 "phone numbers" quiz-phone
                 "numbers 1-100000" quiz-numbers
                 "objects" quiz-objects
                 "events" quiz-events
                 "locations" quiz-locations
                 "expressions" quiz-expressions)]
    (p/loop []
      (quiz)
      (when (not @exit)
        (p/recur)))))