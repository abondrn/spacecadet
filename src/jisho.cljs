; see https://mistval.github.io/unofficial-jisho-api/API.html
(ns src.jisho
  (:require
   [promesa.core :as p]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   
   ["node:fs" :as fs]
   ["node:fs/promises" :as fsPromises]
   ["unofficial-jisho-api$default" :as JoshiAPI]))

(defonce j (new JoshiAPI))

(defn search-phrase
  "Query the official Jisho API for a word or phrase.
   See here for discussion about the official API: https://jisho.org/forum/54fefc1f6e73340b1f160000-is-there-any-kind-of-search-api"
  ([phrase]
  (p/let [result (.searchForPhrase j phrase)]
    (js->clj result)))
  ([phrase page]
   (p/let [result (.searchForPhrase j phrase page)]
     (js->clj result))))

(defn search-phrase-until-page [phrase max-page]
  (p/loop [res [] page 0]
    (p/let [page-res (search-phrase phrase page)
            data (get page-res "data")]
      (println "Page" page)
      (if (or (zero? (count data)) (and (some? max-page) (>= page max-page)))
        res
        (p/recur (into res data) (inc page))))))

(defn write-json [path data]
  (->> data clj->js js/JSON.stringify (fsPromises/writeFile path)))

(defn read-json [path]
  (p/let [json (fsPromises/readFile path)]
    (js->clj (js/JSON.parse json))))

(defn json-cached [path fn]
  (if (fs/existsSync path)
    (read-json path)
    (p/let [res (fn)]
      (write-json path res)
      res)))

(defn get-tag [tag]
  (json-cached (str tag ".json") #(search-phrase-until-page (str "#" tag) nil)))

(defn scrape-phrase
  "Scrape the word page for a word/phrase.
   This allows you to get some information that isn't provided by the official API, such as part-of-speech and JLPT level.
   However, the official API should be preferred if it has the information you need.
   This function scrapes https://jisho.org/word/XXX.
   In general, you'll want to include kanji in your search term, for example 掛かる instead of かかる (no results)."
  [phrase]
  (p/let [result (.scrapeForPhrase j phrase)]
    (js->clj result)))

(defn scrape-kanji
  "Scrape Jisho.org for information about a kanji character."
  [kanji]
  (p/let [result (.searchForKanji j kanji)]
    (js->clj result)))

(defn scrape-examples
  "Scrape Jisho.org for examples."
  [phrase]
  (p/let [result (.searchForExamples j phrase)]
    (js->clj result)))

(defn find [pred coll]
  (first (filter pred coll)))

(defonce WK "wanikani")

(defn wanikani-progress
  "for a set of entries, finds the percentage of the vocab covered by each level of Wani Kani"
  [entries]
  (let [wk-levels (for [entry entries
                          :let [level-tag (find #(str/starts-with? % WK) (get entry "tags"))
                                level-num (if (nil? level-tag) nil (js/parseInt (subs level-tag (count WK))))]]
                      level-num)
          wk-counts (frequencies wk-levels)
          max-level (apply max (filter some? wk-levels))
          vocab-size (count entries)
          wk-cumcounts (loop [level 1 already-covered 0 res []]
                         (if (> level max-level)
                           res
                           (recur (inc level)
                                  (+ already-covered (get wk-counts level 0))
                                  (conj res [level (* 100 (/ (+ already-covered (get wk-counts level 0)) vocab-size))]))))]
    wk-cumcounts))

(defn sense-stats
  "Computes various summary statistics related to Jisho sense entries"
  [entries]
  (let [senses (for [entry entries sense (get entry "senses")] sense)]
    {:by-pos (frequencies (for [s senses pos (get s "parts_of_speech")] pos))
     :by-tag (frequencies (for [s senses t (get s "tags")] t))
     :has-antonyms (count (for [s senses :when (not-empty (get s "antonyms"))] s))
     :has-restrictions (count (for [s senses :when (not-empty (get s "restrictions"))] s))
     :has-info (count (for [s senses :when (not-empty (get s "info"))] s))
     :has-see-also (count (for [s senses :when (not-empty (get s "see_also"))] s))
     :by-lang-source (frequencies (for [sense senses src (get sense "source")] (get src "language")))}))

(defn <=na [a b]
  (if (or (nil? a) (nil? b))
    true
    (<= a b)))

(defn get-terms [tag]
  (p/let [data (get-tag tag)]
    (for [entry data
          :let [level-tag (find #(str/starts-with? % WK) (get entry "tags"))
                level-num (if (nil? level-tag) nil (js/parseInt (subs level-tag (count WK))))]
          :when (nil? level-num)]
      {:eng (for [sense (get entry "senses")
                  def (get sense "english_definitions")]
              def)
       :hir (get-in entry ["japanese" 0 "reading"])
       :full entry})))

#_(p/let [result (search-phrase-all "paper")]
    (print (count result)))

#_(p/let [result (scrape-phrase "紙")]
    (pprint/pprint result))

; TODO configure with CLI args
(defn -main []
  (p/let [n4 (get-tag "jlpt-n4")
          n5 (get-tag "jlpt-n5")
          data (concat n4 n5)]
  ;(pprint/pprint (for [entry data sense (get entry "senses") :when (or (not-empty (get sense "restrictions")) (not-empty (get sense "info")))] sense))
    (pprint/pprint (wanikani-progress data))
    (pprint/pprint (sense-stats
                    (for [entry data
                          :let [level-tag (find #(str/starts-with? % WK) (get entry "tags"))
                                level-num (if (nil? level-tag) nil (js/parseInt (subs level-tag (count WK))))]
                          :when (<=na 16 level-num)]
                      entry)))))