(ns shellfish.dfs.sudoku-test
  (:require [shellfish.dfs.core :as algo]
            [shellfish.dfs.sudoku-util :as su]
            [shellfish.common.grid :as g]
            [shellfish.common.util :as u :refer [cond-let conjs conjm]]
            ;; [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.test :as t :refer [deftest testing is are]]))

;; Adapted from: http://www.norvig.com/sudoku.html
(def boxsiz 3)
(def gridsiz (* boxsiz boxsiz))
(def idxs (range (* gridsiz gridsiz)))
(def idxs-set (set idxs))
(def values (set (range 1 10)))
(def init-allowed (let [all-vals (set values)]
                    (into {} 
                          (map (fn [i]
                                 [i all-vals]))
                          (range (* gridsiz gridsiz)))))

(defn rc->idx [[row col]] 
  (g/rc->idx gridsiz [row col]))

(defn idx->rc [idx]
  (g/idx->rc gridsiz idx))

(defn rc->box [[r c]]
  (g/rc->idx boxsiz [(quot r boxsiz) (quot c boxsiz)]))

(defn idx->box [idx]
  (rc->box (idx->rc idx)))

(defn box-rc->idx [bidx [r c]]
  (let [[br bc] (g/idx->rc boxsiz bidx)]
    (rc->idx [(-> br (* boxsiz) (+ r)) 
              (-> bc (* boxsiz) (+ c))])))

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
              [i (extract-f (g/idx->rc gridsiz i))]))
       (into {})))

(def idx->row (idx->linemap first))
(def idx->col (idx->linemap second))

(defn idx->line-gidxs [f] 
  (into {} 
       (map (fn [i]
              (let [[r c :as rc] (idx->rc i)]
                [i, (for [r-or-c (range gridsiz)]
                            (rc->idx (f rc r-or-c)))])))
       (range (* gridsiz gridsiz))))

(def idx->row-gidxs (idx->line-gidxs (fn [[r _] c] [r c])))
(def idx->col-gidxs (idx->line-gidxs (fn [[_ c] r] [r c])))
(def idx->units
  (into {} 
        (map (fn [idx]
               [idx, [(idx->box-gidxs idx)
                      (into (idx->row-gidxs idx))
                      (into (idx->col-gidxs idx))]])
             idxs)))

(def idx->peers 
  (into {}
        (map (fn [[idx units]] 
               [idx (->> units (mapcat seq) 
                         (remove #{idx}) distinct sort)]))
        idx->units))

(defn fmt-as-idx->val [box-boxrc-vals]
  (->> box-boxrc-vals (partition 3)
       (map (fn [[box brc v]]
               [(box-rc->idx box brc), v]))
       (into {})))

(declare assign)

(defn prune 
  "Prunes v from allowed values for idx, and if idx has only one
  remaining value w, all of idx's peers are pruned from w.
  If any of the units for idx contains a cell which can only have
  w (either because the cell has no other, or all other cells in 
  the unit are already assigned), that cell is assigned w.

  If an empty set of value choices is found, no solution exists  
  and nil is returned.
  "
  [allowed idx v]
  (let [allowed'  
        (cond-let 
         (not (contains? idx-vals v)) [idx-vals (allowed idx)]
         allowed ;; already removed
         
         (empty? remaining) [remaining (disj idx-vals v)]
         nil ;; contradiction, no value possible for idx: abort
         
         (= 1 (count remaining)) 
         [allowed- (assoc allowed idx remaining)
          idxv (first remaining)]
         (->> (idx->peers idx) ;; propagate to peers
              (reduce (fn [alwd pidx]
                        (if-let [less-alwd (and alwd (prune alwd pidx idxv))] 
                          less-alwd
                          (reduced nil)))
                      allowed-))

         :else 
         allowed-)]
    ;; find and assign units with only one place for v
    (->> idx idx->units
         (reduce 
          (fn [alwd unit]
            ;; (println :UNIT unit :IDX idx :V v)
            (if-let [[uidx & more] ;; if v has only one place in u, assign it 
                     (and alwd (->> unit (filter #(some #{v} (alwd %))) seq))]
              (if-not more
                (assign alwd uidx v) 
                alwd)
              (reduced nil))) ;; v MUST have a place in unit, but none found
          allowed'))))


(defn prune-1 [prv-allowed idx v]
  (prune prv-allowed idx v))

(defn assign 
  ([fixed]
   (->> fixed
        (reduce (fn [result [i v]]
                  (assign result i v))
                init-allowed)))
  ([allowed idx v]
   (let [dels (disj (allowed idx) v)]
     (->> dels 
          (reduce (fn [alwd x]
                    (or (prune alwd idx x)
                        (reduced nil)))
                  allowed)))))


(defn ->map [puzzle-str]
  (->> puzzle-str
       (map-indexed vector)
       (reduce (fn [gvs [i v]]
                 (if (not= \0 v)
                   (assoc gvs i (Integer/parseInt (str v)))
                   gvs))
               {})))



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

(deftest idx->peers-test
  (are [idx exp] (= (sort exp) (idx->peers idx))
    10
    [0 1 2 9 11 12 13 14 15 16 17 18 19 20 28 37 46 55 64 73]))


(defn ->str [sol]
  (->> sol sort (map #(first (second %))) (apply str)))

(def problem-1
;; https://en.wikipedia.org/wiki/Sudoku_solving_algorithms#/media/File:Sudoku_Puzzle_by_L2G-20050714_standardized_layout.svg
 "530070000600195000098000060800060003400803001700020006060000280000419005000080079")

(def solution-1 
  "534678912672195348198342567859761423426853791713924856961537284287419635345286179")

(def problem-2 
  ;; Has 3309 solutions, not well formed.
  ;; https://en.wikipedia.org/wiki/Sudoku_solving_algorithms#/media/File:Sudoku_puzzle_hard_for_brute_force.svg
  "000000000000003085001020000000507000004000100090000000500000073002010000000040009")

(def problem-3 
"003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(def solution-3
"483921657967345821251876493548132976729564138136798245372689514814253769695417382")


(def units (->> idx->units (mapcat second) set))

(defn possible-assignment? [values]
  (every? (fn [unit]
            (->> unit 
                 (map values)
                 (reduce (fn [[ok? seen :as acc] vs]
                           (if-not ok?
                             (reduced nil) 
                             (if (= 1 (count vs))
                               (let [v (first vs)]
                                 [(not (seen v)), (conj seen v)])
                               acc)))
                         [true #{}])))
          units))

(deftest assign-test
  (testing "against known solution"
      (are [exp puzzle]
          (= exp (->str (assign (->map puzzle))))
        solution-1 problem-1
        solution-3 problem-3))
  (testing "using validation"
    (are [input] (let [assigned (assign (->map input))]
                   (possible-assignment? assigned))
     problem-1 problem-3 problem-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  DFS test harness and tests below ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn search-priority [m] 
  (fn [k1 k2]
    (let [c1 (count (m k1))
          c2 (count (m k2))]
      (match [(= 1 c1) (= 1 c2)]
             [true true] (< k1  k2) ;; to deal with 'repeated use of assoc', 
                             ;; see clojure.core/sorted-map-by
             [true false] false
             [false true] true
             :else (< c1 c2)))))

#_(defn priority-map 
  ([m]
   (into (sorted-map-by (search-priority m))
         m)))

(defn init-state [grid]
  {:values  (assign grid) #_(let [assigned (assign grid)] 
                 (priority-map assigned))})

(defn update-state [{:keys [values] :as state} new-values]
  (assoc state :values new-values
         #_(priority-map new-values)))

(defn pick [{:keys [values]}]
  (->> values
       (remove #(= 1 (count (second %))))
       (reduce (fn [[kv kount :as acc] [idx vs]]
                 (cond-let
                  (= 2 kount-vs) [kount-vs (count vs)] (reduced [[idx vs]]) 
                  (< kount-vs kount) :>> [[idx vs] kount-vs]
                  :else acc))
               [nil Integer/MAX_VALUE])
       first
       ((fn [[idx vs]] 
           (keep #(assign values idx %)
                 vs)))))

(defn solved? [{:keys [values] :as state}]
  (and (not (empty? values))
       (every? #(and (= (* gridsiz gridsiz) (count values)) 
                     (= 1 (count (second %)))) values)))

(defn unwrap [values]
  (->> values 
       (map (fn [[idx vs]]
              [idx (first vs)])) 
       (into (sorted-map))))

(defn sudoku 
  ([puzzle]
   (sudoku puzzle true nil))
  ([puzzle unwrap?]
   (sudoku puzzle unwrap? nil))
  ([puzzle unwrap? formatter]
   (algo/dfs {:init-state (init-state 
                           ((or formatter identity) puzzle))
              :goal? solved?
              :update update-state
              :generate pick
              :add (fn add 
                     ([] nil) 
                     ([{:keys [values]}] (add nil values))
                     ([_ values] (if unwrap? 
                                   (unwrap values)
                                   values)))
              :options {:no-visited true}})))


(defn solution? [sol]
  (and (solved? {:values sol})
       (possible-assignment? sol)))

(deftest sudoku-test-local-puzzles
  (testing "against known solutions, 1-only (valid) puzzles"
      (are [exp puzzle]
          (let [search (sudoku puzzle nil ->map)]
            (and (= 1 (count search)) 
                 (= exp (->str (first search)))))
        solution-1 problem-1
        solution-3 problem-3))
  (testing "using validation, on multiple solutions (invalid) puzzles"
    (are [input n] (->> (take n (sudoku input nil ->map)) 
                        (every? solution?))
      problem-2 10)))

(defn test-pipe [n]
  (comp (map #(sudoku % false))
        (map #(->> % (take n)))
        (map #(every? solution? %))))

(defn test-puzzles-passed? [n k puzzles]
  (->> puzzles
       (sequence (test-pipe k))
       (take n)
       (every? identity)))

(deftest sudoku-test-50-easy-puzzles
  (testing "50 easy puzzles using validation"
    (is (->> (su/read-50-easy-puzzles)
             (test-puzzles-passed? 50 1)))))

(deftest sudoku-test-95-hard-puzzles
  (testing "95 hard puzzles using validation"
    (is (->> (su/read-95-hard-puzzles)
             (test-puzzles-passed? 5 1)))))
