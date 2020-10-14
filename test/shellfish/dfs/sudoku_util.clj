(ns shellfish.dfs.sudoku-util
  (:import [java.io BufferedReader StringReader])
  (:require [clojure.java.io :as io]))

;; (def path "sudoku/50puzzles.txt")

(defn file->lineparts [filepath]
  (let [lines (BufferedReader. (StringReader. (slurp (io/resource filepath))))]
    (->> (line-seq lines)
         (partition-by #(.startsWith % "Grid"))
         (filter #(re-matches #"^\d+" (first %))))))


(defn parse-line [line]
  (->> line (map-indexed (fn [i c]
                           [i, (-> c str Integer/parseInt)]))))

(defn lines->grid-pipeline [colsiz]
  (comp (map parse-line)
        (map-indexed 
         (fn [row ivs]
           (->> ivs
                (mapv (fn [[i v]]
                        [(-> (* row colsiz) (+ i)), v])))))
        cat
        (remove (fn [[_ v]] (zero? v)))))

(defn ->grid [lines colsiz]
  (into {} (lines->grid-pipeline colsiz) lines))


(defn read-puzzles [size file]
  (->> (file->lineparts file)
       (map #(->grid % size))))
