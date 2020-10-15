(ns shellfish.dfs.sudoku-util
  (:import [java.io BufferedReader StringReader])
  (:require [clojure.java.io :as io]
            [shellfish.common.grid :as g :refer [rc->idx]]))


(defn file->lineparts [filepath by-grid noise-filter]
  (let [lines (BufferedReader. (StringReader. (slurp (io/resource filepath))))]
    (->> (line-seq lines)
         by-grid ;;(partition-by separator) 
         (filter noise-filter))))

(def digits (set "123456789"))

(defn char->1-9? [c]
  (when (digits c)
    (-> c str Integer/parseInt)))

(defn parse-line [line, digitize]
  (->> line (sequence 
             (comp (map-indexed (fn [i c]
                                  [i, (digitize c)]))
                   (filter second)))))

(defn lines->grid-pipeline [colsiz digitizer]
  (comp (map #(parse-line % digitizer))
        (map-indexed 
         (fn [row ivs]
           (->> ivs
                (mapv (fn [[col v]]
                        (let [idx (rc->idx colsiz [row col])]
                          [idx, v]))))))
        cat))

(defn lines->grid [lines colsiz digitizer]
  (into {} 
        (lines->grid-pipeline colsiz digitizer)
        lines))

(defn line->grid-pipeline [colsiz digitize]
  (comp #_(map #(partition colsiz %))
        (map-indexed (fn [idx c]
                       (when-let [d (digitize c)]
                         [idx d])))))

(defn line->grid [line colsiz digitizer]
  (into {} 
        (line->grid-pipeline colsiz digitizer)
        line))

(defn read-puzzles 
  ([file grid-separator noise-filter grid-processor]
   (->> (file->lineparts file grid-separator noise-filter)
        (map grid-processor))))


(defn config 
  ([] (config 9))
  ([size] 
   {:50-easy-puzzles 
    ["sudoku/50puzzles.txt" 
     (fn [lines] (partition-by #(.startsWith % "Grid")
                               lines))
     #(re-matches #"^\d+" (first %))
     #(lines->grid % size char->1-9?)]
    :95-hard-puzzles
    ["sudoku/95-hard-puzzles.txt"
     identity 
     #(re-matches #"^[1-9.]+\n?" %)
     #(line->grid % size char->1-9?)
     ]}))

(defn read-50-easy-puzzles []
  (apply read-puzzles (:50-easy-puzzles (config))))

(defn read-95-hard-puzzles []
  (apply read-puzzles (:95-hard-puzzles (config))))
