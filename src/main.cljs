(ns src.main
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   
   [promesa.core :as p]
   [applied-science.js-interop :as j]
   [nbb.repl :as repl]
   
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
        jh (num/jnumber "hour" h)
        jm (num/jnumber "minute" m)
        hir (str (am-pm am-or-pm) " " jh " " jm)
        rom (k/romanize hir)]
    {:am-or-pm am-or-pm
     :h h
     :m m
     :eng (pprint/cl-format nil "~d:~2,'0d ~d" h m (get ["a.m." "p.m."] am-or-pm))
     :hir hir
     :rom rom
     }))

(defn prompt [questions]
  (inquirer/prompt (clj->js questions)))

(defn strip-whitespace [s]
  (str/replace s #"\s+" ""))

; https://teamjapanese.com/stop-in-japanese/
(defonce Q ["quit" "stop" "exit" "q" "yamete" "yamero" "tomare" "mouii" "sutoppu"])

(def results (atom []))
(def exit (atom false))

(defn in? [xs el] (some #(= % el) xs))

(defn diff-attempt [input answer message]
  ;(println "\n" input answer)
  (let [raw-diff (diff input answer)
        stripped-diff (filter (fn [change] (and (not= (get change 0) 0)
                                                (not (str/blank? (get change 1)))))
                              raw-diff)]
    (cond ; no diff
          (== 0 (count stripped-diff)) true
          ; show answer
          (in? ["idk" "?"] input) (do (println "\n" answer) true)
          ; should quit
          (in? Q input) (do (reset! exit true) true)
          (in? ["rom"] input) (k/romanize message)
          :else 
          (apply str (for [change raw-diff
                            :let [code (get change 0)
                                  tokens (get change 1)]]
                        (case code
                          0  (chalk/green tokens)
                          1  (->> "?" (repeat (count (strip-whitespace tokens))) (apply str) chalk/italic chalk/yellow)
                          -1 (-> tokens chalk/red chalk/strikethrough)))))))

(defn handle-sigint []
  (println "Exiting...")
  (reset! exit true))

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
    (swap! results conj {:target target
                         :message message
                         :duration (/ (- end start) 1000)
                         :reattempts (dec @attempts)})))

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

(defonce OBJECTS
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
   
   {:eng "television" :kat "テレビ"}
   {:eng "computer" :kat "パソコン"}
   {:eng "refrigerator" :hir "れいぞうこ"}
   {:eng "air conditioner" :kat "エアコン"}
   {:eng "microwave oven" :hir "でんし れんじ"}

   {:eng "coffee" :kat "コーヒー"}
   {:eng "juice" :kat "ジュース"}
   {:eng "sandwich" :kat "サンドイチ"}
   {:eng "black tea" :hir "こうちゃ"}
   ])

(defn quiz-objects []
  (let [obj (rand-nth OBJECTS)
        eng (if (string? (:eng obj)) (:eng obj) (first (:eng obj)))
        jap (or (:kat obj) (:hir obj))
        eng->jap? (zero? (rand-int 2))
        target (if eng->jap? (k/romanize jap) eng)
        msg (if eng->jap? (str "Translate to Japanese: " eng) (str "Translate to English: " jap))]
    (quiz-loop target msg)))

(defn -main []
  (println "Press Ctrl+C to exit.")
  (.on js/process "SIGINT" handle-sigint)
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
                 "objects" quiz-objects)]
    (p/loop []
      (quiz)
      (if (not @exit)
        (p/recur)
        (println @results)))))