(ns inhabitant.experiments
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]))

#_(-> "resources/war-and-peace.txt"
    slurp
    (str/split #"\r\n"))

(defn pmap-file
  [processing-fn input-file output-file]
  (with-open [rdr (jio/reader input-file)
              wtr (jio/writer output-file)]
    (let [lines (line-seq rdr)]
      (dorun
       (map #(.write wtr %)
            (pmap processing-fn lines))))))

(def newline-delimiter
  (let [os (System/getProperty "os.name")]
    (if (some? (re-find #"[w|W]in" os))
      "\r\n"
      "\n")))

(defn filter-words [freqs]
  (filter (fn [[word appears]] (and (< 5 (count word)) (<= 10 appears))) freqs))

(defn char-counts [row-string]
  (-> row-string
      frequencies
      (->> (into [])
           (filter-words)
           (map (fn [[ch counts]] (str ch " " counts newline-delimiter)))
                  ; _(str row-string ", char " ch ", counts: " counts newline-delimiter)
           (apply str))))

#_(pmap-file char-counts "resources/war-and-peace.txt" "resources/counts.txt")

(defn extract-words [row-string]
  (->> row-string
       (re-seq #"[а-яА-Яa-zA-Z]+")
       (map str/lower-case)
       (into #{})
       (into [])
       (str/join " ")
       (str " ")))

#_(pmap-file extract-words "resources/war-and-peace.txt" "resources/words.txt")

(-> "resources/words.txt"
    slurp
    (str/split #" ")
    frequencies
    (->>
     (into [])
     (filter-words)
     (shuffle)
     (take 10)
     #_(sort-by (fn [[_ n]] n))
     #_(map (fn [[word appears]] (str word " " appears newline-delimiter)))
     (map (fn [[w _]] w))
     (str/join " ")))

