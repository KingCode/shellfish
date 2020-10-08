(ns shellfish.dfs.wolf-sheep-cabbage
  (:require [shellfish.dfs.core :as algo] 
            [clojure.test :as t]))

(def passengers [:wolf :sheep :cabbage])
(def loss-groups (->> passengers 
                      (partition 2 1 )
                      (map set)
                      set))

(defn next-direction [dir]
  (condp = dir
    :init :across
    :return :across
    :across :return))

(defn lossy? [group]
  (some (fn [xy]
          (every? group (seq xy)))
        (seq loss-groups)))

(defn src+dst [{:keys [from to]} direction]
  (let [[src-k src dst-k dst] (if (= :across direction)
                                [:from from :to to]
                                [:to to :from from])]
    {:src src, :src-k src-k, 
     :dst dst, :dst-k dst-k}))

(defn valid-move? [state passenger direction]
  (let [{:keys [src dst]} (src+dst state direction)]
    (if-not passenger
      (not (lossy? src))
      (and (src passenger)
           (not (lossy? (disj src passenger)))))))

(defn trip-candidates [{:keys [after] :as state}]
  (let [d (next-direction after)
        {pool :src} (src+dst state d)]
    (->> pool 
         (cons nil)
         (filter #(valid-move? state % d))
         (map #(hash-map :passenger % :direction d)))))

(def init-state {:from (set passengers) 
                 :to #{} 
                 :after :init})

(defn alive? [state visited]
  (not (visited state)))

(defn goal-reached? [{:keys [from to]}]
  (and (= nil (seq from)) 
       (= (set passengers) to)))

(defn add-trip 
  ([] [])
  ([history trip]
   (conj history trip)))

(defn update-state [ state
                    {:keys [passenger direction] :as trip}]
  (let [{:keys [src src-k dst dst-k]} (src+dst state direction)]
    (merge (if passenger
             {src-k (disj src passenger)
              dst-k (conj dst passenger)}
             state)
           {:after direction})))

(defn wolf-sheep-cabbage []
  (algo/depth-first-search init-state 
                      alive?
                      goal-reached?
                      trip-candidates
                      add-trip
                      update-state))
