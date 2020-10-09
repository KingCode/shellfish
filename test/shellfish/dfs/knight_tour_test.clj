(ns shellfish.dfs.knight-tour-test
  (:require [shellfish.dfs.core :as algo] 
            [clojure.test :as t]))

(defn rc->idx [n row col]
  (-> (* row n) (+ col) ))
(defn idx->rc [n idx]
  [(quot idx n), (rem idx n)])
(defn in-bounds [n [r c]]
  (every? #(< -1 % n) [r c]))

(def move-fns  (->> (for [f [inc dec] 
                          g [#(+ % 2) #(- % 2)]]
                      [f g])
                    (map #(apply juxt %))))

(defn neighbours [[r c]]
  (let [[r1s r2s] (->> move-fns 
                       (map #(% r))
                       (reduce (fn [[ones twos] [r1 r2]]
                                 [(conj ones r1), (conj twos r2)])
                               [[] []]))
        [c1s c2s] (->> move-fns 
                       (map #(% c))
                       (reduce (fn [[ones twos] [c1 c2]]
                                 [(conj ones c1) (conj twos c2)])
                               [[] []]))]
    (-> (mapv vector r1s c2s)
        (into (mapv vector r2s c1s)))))
   
(defn moves [n [r c]]
  (->> (neighbours [r c]) 
       (filter #(in-bounds n %)))) 

(defn valid-moves [n visited square]
  (->> (moves n square)
       (remove visited)))

(defn goal-reached? [n {:keys [visited]}]
  (= (* n n) (count visited)))

(defn update-state [{:keys [visited last]} square]
  {:visited (conj visited square)
   :last square})

(defn knight-tour [n]
  (algo/dfs {:init-state {:visited #{}
                          :last nil}
             :generate (fn [{:keys [visited last]}]
                         (if-not last
                           (for [x (range n) y (range n)] 
                             [x y])
                           (valid-moves n visited last)))
             :goal?  #(goal-reached? n %)
             :update update-state}))
