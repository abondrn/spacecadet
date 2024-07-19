;; source: https://stackoverflow.com/questions/1452680/clojure-how-to-generate-a-trie#2511115
(ns src.trie)

(defn add-to-trie [trie x]
  (assoc-in trie x (merge (get-in trie x) {:val x :terminal true})))

(defn in-trie? [trie x]
  "Returns true if the value x exists in the specified trie."
  (:terminal (get-in trie x) false))

(defn prefix-matches [trie prefix]
  "Returns a list of matches with the prefix specified in the trie specified."
  (keep :val (tree-seq map? vals (get-in trie prefix))))

(defn longest-match [trie seq default]
  (loop [ks seq
         curr trie
         last-match [default (rest seq)]]
    (let [last-match' (if (:terminal curr) [(:val curr) ks] last-match)]
      (cond
        (empty? ks) last-match'
        (not (contains? curr (first ks))) last-match'
        :else (recur
               (rest ks)
               (get curr (first ks))
               last-match')))))

(defn tokenize [trie seq]
  (loop [ks seq
         out []]
    (if (empty? ks) out
        (let [[match ks'] (longest-match trie ks (first ks))]
          (recur ks' (conj out match))))))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))