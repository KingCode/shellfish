(ns shellfish.dfs.sudoku-test
  (:require [shellfish.dfs.core :as algo]
            [clojure.set :as set]
            [clojure.test :as t :refer [deftest testing is are]]))

(def boxsiz 3)
(def gridsiz (* boxsiz boxsiz))
(def idxs (range (* gridsiz gridsiz)))
(def idxs-set (set idxs))
(def values (range 1 10))

(defn rc->idx 
  ([row col] 
   (rc->idx gridsiz row col))
  ([n row col]
   (-> (* row n ) (+ col) )))

(defn idx->rc 
  ([idx]
   (idx->rc gridsiz idx))
  ([n idx]
   [(quot idx n), (rem idx n)]))

(defn rc->box [[r c]]
  (rc->idx boxsiz (quot r boxsiz) (quot c boxsiz)))

(defn idx->box [idx]
  (rc->box (idx->rc idx)))

(defn box-rc->idx [bidx [r c]]
  (let [[br bc] (idx->rc boxsiz bidx)]
    (rc->idx (-> br (* boxsiz) (+ r)) 
             (-> bc (* boxsiz) (+ c)))))

(def idx->box (->> (for [i idxs]
                     [i (idx->box i)])
                   (into {})))

(def box->gidxs (->> (range gridsiz) 
                         (map (fn [i] 
                                [i, (for [r (range boxsiz)
                                          c (range boxsiz)]
                                      (box-rc->idx i [r c]))]))
                         (into {})))

(def idx->box-gidxs 
  (->> idxs
       (map (fn [idx]
              [idx, (-> idx idx->box box->gidxs)]))
       (into {})))

(defn idx->linemap [extract-f] 
  (->> idxs
       (map (fn [i]
              [i (extract-f (idx->rc gridsiz i))]))
       (into {})))

(def idx->row (idx->linemap first))
(def idx->col (idx->linemap second))

(defn fmt-as-idx->val [box-boxrc-vals]
  (->> box-boxrc-vals (partition 3)
       (map (fn [[box brc v]]
               [(box-rc->idx box brc), v]))
       (into {})))

(defn conj-fn [init]
  (fn f 
    ([] init)
    ([coll x]
     (conj (or coll (f)) x))))

(def conjs  (conj-fn #{})
  #_([] #{})
  #_([coll v]
   (conj (or coll (conjs)) v)))

(def conjm (conj-fn {}))

(defn populate-state [idx->val]
  (let [ init-constraints (fn [m] 
                            (reduce (fn [acc [i v]]
                                      (update acc (get m i) conjs v))
                                    {}
                                    idx->val))]
    {:occupied (set (keys idx->val))
     :rows (init-constraints idx->row)
     :cols (init-constraints idx->col)
     :boxes (init-constraints idx->box)}))

(defn init-state [puzzle]
  (populate-state (fmt-as-idx->val puzzle)))

(defn update-state [{:keys [rows cols boxes occupied] :as state} [idx v]]
  (merge state
         {:rows (update rows (idx->row idx) conjs v)
          :cols (update cols (idx->col idx) conjs v)
          :boxes (update boxes (idx->box idx) conjs v)
          :occupied (conj occupied idx)}))

(defn available [{:keys [rows cols boxes occupied] :as state}]
  (->> (set/difference idxs-set occupied)
       sort
       (mapcat (fn [idx]
                 (let [taken-vals (-> (get rows (idx->row idx) #{})
                                      (into (get cols (idx->col idx)))
                                      (into (get boxes (idx->box idx))))]
                   (->> values (remove taken-vals)
                        (map (fn [v]
                               [idx v]))))))))

(defn goal-reached? [{:keys [occupied fixed]}]
  (= (* gridsiz gridsiz) (count occupied)))

(defn sudoku [puzzle]
  (algo/dfs {:init-state (init-state puzzle)
             :generate available
             :goal? goal-reached?
             :update update-state
             :add conjm
             :options {:no-visited true}}))

(def problem-1
;; https://en.wikipedia.org/wiki/Sudoku_solving_algorithms#/media/File:Sudoku_Puzzle_by_L2G-20050714_standardized_layout.svg
  [0 [0 0] 5, 0 [0 1] 3, 0 [1 0] 6, 0 [2 1] 9, 0 [2 2] 8, 
   1 [0 1] 7, 1 [1 0] 1, 1 [1 1] 9, 1 [1 2] 5, 
   2 [2 1] 6, 
   3 [0 0] 8, 3 [1 0] 4, 3 [2 0] 7, 
   4 [0 1] 6, 4 [1 0] 8, 4 [1 2] 3, 4 [2 1] 2,
   5 [0 2] 3, 5 [1 2] 1, 5 [2 2] 6, 
   6 [0 1] 6, 
   7 [1 0] 4, 7 [1 1] 1, 7 [1 2] 9, 7 [2 1] 8,
   8 [0 0] 2, 8 [0 1] 8, 8 [1 2] 5, 8 [2 1] 7, 8 [2 2] 9])

(def solution-1 [0 [0 2] 4, 0 [1 1] 7, 0 [1 2] 2, 0 [2 0] 1, 
                 1 [0 0] 6, 1 [0 2] 8, 1 [2 0] 3, 1 [2 1] 4, 1 [2 2] 2,
                 2 [0 0] 9, 2 [0 1] 1, 2 [0 2] 2, 2 [1 0] 8, 2 [1 1] 4, 2 [ 1 2] 8,
                 2 [2 0] 5, 2 [2 2] 7, 
                 3 [0 1] 5, 3 [0 2] 9, 3 [1 1] 2, 3 [1 2] 6, 3 [2 1] 1, 3 [2 2] 3,
                 4 [0 0] 7, 4 [0 2] 1, 4 [1 1] 5, 4 [2 0] 9, 4 [2 2] 4,
                 5 [0 0] 4, 5 [0 1] 2, 5 [1 0] 7, 5 [1 1] 9, 5 [2 0] 8, 5 [2 1] 5,
                 6 [0 0] 9, 6 [0 2] 1, 6 [1 0] 2, 6 [1 1] 8, 6 [1 2] 7, 
                 6 [2 0] 3, 6 [2 1] 4, 6 [2 2] 5,
                 7 [0 0] 5, 7 [0 1] 3, 7 [0 2] 7, 7 [2 0] 2, 7 [2 2] 6,
                 8 [0 2] 4, 8 [1 0] 6, 8 [1 1] 3, 8 [2 0] 1])
(def problem-2 
  ;; https://en.wikipedia.org/wiki/Sudoku_solving_algorithms#/media/File:Sudoku_puzzle_hard_for_brute_force.svg
  [0 [2 2] 1, 1 [1 2] 3, 1 [2 1] 2, 2 [1 1] 8, 2 [1 2] 5, 
   3 [1 2] 4, 3 [2 1] 9, 4 [0 0] 5, 4 [0 2] 7, 5 [1 0] 1,
   6 [0 0] 5, 6 [1 2] 2, 7 [1 1] 1, 7 [2 1] 4, 8 [0 1] 7, 8 [0 2] 3, 8 [2 2] 9])

(deftest idx->box-test
  (are [exp idxs] (let [boxes (mapv idx->box idxs)]
                    (and (= 1 (count (set boxes)))
                         (= exp (first boxes))))
    0 [0 1 2 9 10 11 18 19 20]
    1 [3 4 5 12 13 14 21 22 23]
    2 [6 7 8 15 16 17 24 25 26]
    3 [27 28 29 36 37 38 45 46 47]
    8 [60 61 62 69 70 71 78 79 80]))

(deftest rc->box-test
  (are [exp rc] (= exp (rc->box rc))
    0 [0 2] 0 [1 1]
    1 [0 3] 1 [2 5]
    2 [0 7] 2 [2 6]
    3 [3 0] 3 [3 2] 3 [4 2] 3 [5 0]
    4 [3 3] 4 [4 4] 4 [5 5]
    5 [3 8] 5 [4 7]
    6 [6 0] 6 [8 1]
    7 [6 3] 7 [7 4]
    8 [8 8]))

(defn scaffold-state [problem solution removed-ks]
  (-> (fmt-as-idx->val problem)
      (merge (fmt-as-idx->val solution))
      (#(apply dissoc % removed-ks))
      populate-state))

(deftest available-test 
  (are [exp removed-from-sol prob sol] 
      (let [state (scaffold-state prob sol removed-from-sol)
            actual (available state)] 
        (= (into #{} exp) 
           (set (available state))))
    [[78 1]] [78] problem-1 solution-1))

(deftest sudoku-test
  (are [exp input] (= (fmt-as-idx->val exp) (first (sudoku input)))
    solution-1 problem-1))
