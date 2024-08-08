; see https://mistval.github.io/unofficial-jisho-api/API.html
(ns src.jisho
  (:require
   [promesa.core :as p]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   
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

#_(p/let [result (search-phrase-all "paper")]
  (print (count result)))

#_(p/let [result (scrape-phrase "紙")]
  (pprint/pprint result))

(defn find [pred coll]
  (first (filter pred coll)))

(defonce WK "wanikani")

; for a given JLPT level, finds the percentage of the vocab covered by each level of Wani Kani
#_(p/let [result (search-phrase-until-page "#jlpt-n5" nil)
        wk-levels (for [entry result
                        :let [level-tag (find #(str/starts-with? % WK) (get entry "tags"))
                              level-num (if (nil? level-tag) nil (js/parseInt (subs level-tag (count WK))))]]
                    level-num)
        wk-counts (reduce (fn [m wanikani-level]
                            (assoc m wanikani-level (inc (get m wanikani-level 0))))
                          {}
                          wk-levels)
        max-level (apply max (filter some? wk-levels))
        vocab-size (count result)
        wk-cumcounts (loop [level 1 already-covered 0 res []]
                       (if (> level max-level)
                         res
                         (recur (inc level)
                                (+ already-covered (get wk-counts level 0))
                                (conj res [level (* 100 (/ (+ already-covered (get wk-counts level 0)) vocab-size))]))))
        ]
  (pprint/pprint wk-cumcounts))