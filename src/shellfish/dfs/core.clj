(ns shellfish.dfs.core
  (:refer-clojure :exclude [update])
  (:require [clojure.zip :as z]))

;; Definitions
;; - Solution to a DFS problem can be anything built of partial results,
;;   each of which is an Element of the solution.
;; - State: a client provided entity, together with update and predicate 
;;   functions, which encapsulates the client domain world at a given step.
;;   It also links back to all previously visited states
;;   It is used to generate elements and in deciding whether liveness is 
;;   impeded. 
;; - Goal Reach: an individual solution is complete, as decided by a client 
;;   domain predicate on the current-state
;; - Candidate: a valid element according to the current state and 
;;   the client domain.
;;
;; Intent: to provide in isolation the low-level work of state traversal and 
;;         backtracking in a depth-first search, from problem domain specific 
;;         decisions such as what consitutes a solution, and validation/generation 
;;         of elements. A client then only needs to provide an initial state 
;;         together with a state update and result accumulator functions,
;;         an end-state predicate, and a generator of candidate solution elements.
;;

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
                     {:keys [goal? add update generate] :as cfg}]
  (loop [[[x {:keys [state visited result]}] _ :as zcands] zcands]
    (when zcands
      (let [next-state (update state x)]
        (cond 
          (goal? next-state)
          [(add result x) (znext zcands)]
          (visited next-state)
          (recur (znext zcands))
          :else
          (recur (zadd-znext zcands 
                             (generate next-state)
                             {:state next-state
                              :visited (conj visited next-state)
                              :result (add result x)})))))))

(defn dfs
"Given a domain context and parameter functions, returns a lazy sequence 
 of all solutions using depth-first search. 

 The domain context and parameters map argument consists of:
  :init-state, an initial state 
  :goal?, a predicate which returns true if its argument state represents a solution
  :update, a fn taking a state and a solution element, and yielding the next state
  :generate, a fn generating a sequence of candidates from the current state
  :add, a 0- arity and 2-arity fn initializing and adding an element to a solution,
        defaults to 'conj

 Note that outputs from the update function are assumed to be unique, or inaccurate 
 results or non-termination may occur. 
"
  ([{:keys [init-state goal? generate add update]
     :or {add conj} :as params}]
   (let [cfg (merge {:add add} 
                    (select-keys params [:goal? :generate :add :update])) 
         seed (-> init-state generate 
                  (zload {:state init-state
                          :visited #{init-state}
                          :result (add)}) 
                   zinit)
         f (fn f [zcands]
             (when-let [[sol next-zcands] (next-solution zcands cfg)]
               (cons sol (f next-zcands))))]
     (f seed)))

  ([init-state, goal-pred?, generate-candidates-fn,
    add-element-fn, update-state-fn]
   (depth-first-search {:init-state init-state
                        :goal? goal-pred?
                        :generate generate-candidates-fn
                        :add add-element-fn
                        :update update-state-fn})))
