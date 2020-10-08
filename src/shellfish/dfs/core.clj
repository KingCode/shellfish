(ns shellfish.dfs.core
  (:require [clojure.zip :as z]))

;; Definitions
;; - Solution to a DFS problem can be anything built of partial results,
;;   each of which is an Element.
;; - State: a client provided entity, together with update and predicate 
;;   functions, which encapsulates the client domain world at a given step.
;;   It also links back to all previously visited states
;;   It is used to generate elements and in deciding whether liveness is 
;;   impeded. 
;; - Validity: an element is valid as decided by the problem domain,
;;   as provided by the client (e.g. a function)
;; - Liveness: a partial solution is alive after contributing an element, 
;;   if it can't be shown that progress isn't being made after the 
;;   contribution. This is decided by a client function taking as 
;;   argument all states visited so far.
;; - Goal Reach: an individual solution is complete the client domain
;;   decides so, given the current-state.
;; - Candidate: a valid element according to the current state and 
;;   the client domain.
;;
;; Algo: Given a initial-state, validating, liveness and goal-reach predicates,
;;       as well as a generator function, lazily generate all solutions for which
;;       (goal-reached? <current-state>): for each candidate partial result,
;;       either end the current search if liveness is impeded, in which case
;;       the next candidate from the previous partial result is evaluated (if any); 
;;       or add the candidate to the current partial result and recurse. If a 
;;       solution has been reached, return to the previous partial result and 
;;       recurse on the next candidate.
;;       Laziness is on a per-solution basis: a computation produces the next
;;       solution or nil if none is found. 

(defn cands<-state [candidates state]
  (->> candidates 
       (mapv #(vector % state))))

(defn zload [candidates state]
  (-> candidates 
       (cands<-state state)
       z/vector-zip))

(defn zadd-znext [zipped new-cands state]
  (-> zipped
      (z/replace (cands<-state new-cands state))
      z/next))

(defn zinit [zcands]
  (z/down zcands))

(defn znext 
  "Removes the current element, then moves cursor to the first sibling, 
   or sibling of parent recursively, until one is found or hierarchy is exhausted, 
   in which case nil is returned"
[zcands]
  (let [znxt (-> zcands z/remove z/next)] 
    (when (not (z/end? znxt))
      znxt)))

(defn next-solution [zcands
                     {:keys [alive? goal? add-to-result 
                             update-state generate-cands]}]
  (loop [[[x {:keys [state visited result]}] _ :as zcands] zcands]
    (when zcands
      (let [next-state (update-state state x)]
        (cond 
          (goal? next-state)
          [(add-to-result result x) (znext zcands)]
          (not (alive? next-state visited))
          (recur (znext zcands))
          :else
          (recur (zadd-znext zcands 
                             (generate-cands next-state)
                             {:state next-state
                              :visited (conj visited next-state)
                              :result (add-to-result result x)})))))))

(defn depth-first-search 
  ([init-state, alive?-pred, goal-pred?, generate-candidates-fn,
    add-element-fn, update-state-fn]
   (let [candidates (generate-candidates-fn init-state)
         zcands (zinit (zload candidates {:state init-state
                                          :visited #{init-state}
                                          :result (add-element-fn)}))]
     (depth-first-search zcands  {:alive? alive?-pred
                                  :goal? goal-pred?
                                  :generate-cands generate-candidates-fn
                                  :add-to-result add-element-fn
                                  :update-state update-state-fn})))
  ([zcands cfg]
   ;; (println :ZCANDS zcands)
   (when-let [[solution next-zcands] (next-solution zcands cfg)]
     (cons solution
           (lazy-seq (depth-first-search next-zcands cfg))))))


#_(defn incr [n]
  (cons n
        (lazy-seq (incr (inc n)))))


