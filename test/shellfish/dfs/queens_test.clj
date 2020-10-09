(ns shellfish.dfs.queens-test
  (:require [shellfish.dfs.core :as algo] 
            [clojure.test :as t :refer [deftest testing is are]]))

;; (defn rc->idx [n row col]
  ;; (-> (* row n) (+ col) ))
;; (defn idx->rc [n idx]
  ;; [(quot idx n), (rem idx n)])
(defn edges? [n rc & rcs]
  (every? (fn [[r c]]
            (or (zero? r)
                (= (dec n) c))) 
          (cons rc rcs)))

(defn in-bounds? [n xs]
  (every? #(< -1 % n) xs))

(defn get-low-edges 
  ([n [r c :as rc]]
   (if (some zero? rc)
     [rc rc]
     (get-low-edges n 
                    [(dec r),(dec c)] 
                    [(dec r),(inc c)])))
  ([n [r1 c1 :as rc1] [r2 c2 :as rc2]]
   (cond
     (edges? n rc1 rc2)
     [rc1 rc2]
     (edges? n rc1)
     (get-low-edges n rc1 [(dec r2),(inc c2)])
     (edges? n rc2)
     (get-low-edges n [(dec r1),(dec c1)] rc2)
     :else
     (get-low-edges n [(dec r1),(dec c1)] [(dec r2), (inc c2)]))))

(defn diagonals [n [r c :as rc]]
  (let  [[lo1 lo2] (get-low-edges n rc)]
    (->> [(->> lo1
               (iterate (fn [[r c]]
                          [(inc r) (inc c)]))),
          (->> lo2
               (iterate (fn [[r c]]
                          [(inc r) (dec c)])))] 
         (mapv (fn [diag] 
                 (->> diag (drop-while #(not (in-bounds? n %))) 
                      (take-while #(in-bounds? n %))))))))

(defn init [n]
  {:taken {:squares #{}
           :rows [] ;; use (inc (last ..)) as roow coord 
           :cols #{}}
   :n n})

(defn available [{{:keys [squares rows cols]} :taken n :n}]
  (->> (for [r (range (inc (or (last rows) -1)) n)
             c (range n)
             :when (not (cols c))
             :when (not (squares [r c]))]
         [r c])))

(defn goal-reached? [{{:keys [rows cols]} :taken n :n}]
  (= n (count rows) (count cols)))

(defn update-state [{{:keys [squares rows cols]} :taken n :n :as state}
                    [r c :as rc]]
  (merge state
         {:taken {:rows (conj rows r)
                  :cols (conj cols c)
                  :squares (into squares
                                 (apply concat (diagonals n rc)))}}))

(defn queens [n]
  (algo/dfs {:init-state (init n)
             :generate available
             :goal? goal-reached?
             :update update-state
             :options {:no-visited true}}))


(deftest diagonals-test
  (are [expected n r c] (= (set expected) 
                        (set (->> (diagonals n [r c])
                                  (filter seq))))
    [[[0 0] [1 1] [2 2] [3 3]],
     [[0 2] [1 1] [2 0]]] 
    4 1 1

    [[[0 0] [1 1] [2 2] [3 3]],
     [[1 3] [2 2] [3 1]]]
    4 2 2
    
    [[[0 0] [1 1] [2 2] [3 3]],
     [[3 3]]]
    4 3 3

    [[[0 1] [1 2]],
     [[1 2] [2 1]]]
    3 1 2

))


(defn valid-solution? [n sol]
  (->> sol
       (reduce (fn [[squares rs cs] [r c]]
                 (cond 
                   (rs r)
                   (reduced false)
                   (cs c)
                   (reduced false)
                   (squares [r c])
                   (reduced false)
                   :else
                   [(into squares 
                          (mapcat identity (diagonals n [r c])))
                    (conj rs r)
                    (conj cs c)]))
               [#{} #{} #{}])))

(deftest queens-test
  (are [n] (every? #(valid-solution? n %) (queens n))
    1 2 3 4 5))


(deftest queens-completeness-test
;; Based on
;; https://en.wikipedia.org/wiki/Eight_queens_puzzle#Counting_solutions
  (is (every? identity 
              (map #(= % (count (queens %2))) 
                   [1, 0, 0, 2, 10, 4]
                   (range 1 7)))))
