; see https://mistval.github.io/unofficial-jisho-api/API.html
(ns src.jisho
  (:require
   [promesa.core :as p]
   [clojure.pprint :as pprint]
   
   ["unofficial-jisho-api$default" :as JoshiAPI]))

(defonce j (new JoshiAPI))

(defn search-phrase
  "Query the official Jisho API for a word or phrase.
   See here for discussion about the official API: https://jisho.org/forum/54fefc1f6e73340b1f160000-is-there-any-kind-of-search-api"
  [phrase]
  (p/let [result (.searchForPhrase j phrase)]
    (js->clj result)))

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

#_(p/let [result (search-phrase "paper")]
  (pprint/pprint result))

#_(p/let [result (scrape-phrase "紙")]
  (pprint/pprint result))