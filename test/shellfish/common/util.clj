(ns shellfish.common.util)

;; https://gist.github.com/KingCode/773560f4ab5bf91e660a2a26e581b036
;; (cond-let 
;;   (odd? x) [x n] (inc x)  
;;   (< n 10) [y (inc n)] 10
;;   :else n))

;; we want the above to yield
;; (let [x n]
;;   (if (odd? x)
;;      (inc x)
;;      (let [y (inc n)]
;;           (if (< n 10)
;;               10
;;               (if :else
;;                   n
;;                   (throw ...."no matching clause"))))))

(defmacro cond-let 
  "Takes ternary clauses which can use bindings visible to both the test 
  and result expression, as well as all following clauses (except when shadowed);
  the last clause which can be binary and follows 'cond semantics. 
  Each ternary clause can be of the form
        text-expr binding-vector result-expr 
  or 
        test-expr :>> result-expr 
  if there are no new bindings added to the clause
  (:>> is an ordinary keyword) 
  "
  [& clauses]
  (let [emit (fn emit [args]
               (let [[[pred binds expr :as clause] more]
                     (split-at 3 args)
                     n (count args)]
                 (cond
                   (= n 0) `(throw (IllegalArgumentException. 
                                    (str "No matching clause: " ~expr)))
                   (< n 2) `(throw (IllegalArgumentException. 
                                    (str "Must have at least 2 arguments: " 
                                         ~clause)))
                   (= n 2)
                   `(if ~pred 
                      ~(second clause)
                      ~(emit more))
                   (= :>> (second clause))
                   `(if ~pred
                      ~expr
                      ~(emit more))
                   :else
                   `(let ~binds
                      (if ~pred 
                        ~expr
                        ~(emit more))))))]
    (emit clauses)))


;; (cond-let> 
;;     (odd? x) [x n] (inc x)
;;     (even? n) :>> (dec n) 
;;     (< 10 (+ y z)) [y (inc n) z 80] (* 2 n z)
;;     :else n

;; we want the above to yield:
;;     (or ((fn [x]
;;            (when (odd? x)
;;               (inc x))  n)
;;         ((fn []
;;          (when (even? n)
;;            (dec n))))
;;         ((fn [y z]
;;            (when (< 10 (+ y z))
;;               (* 2 n z))) (inc n) 80)  
;;         ((fn []
;;            (when :else
;;               n))) 
;;        (throw..."No matching clause.."))

(defmacro cond-let> 
  "Same as for cond-let, except bindings are local to each clause only."
  [& clauses]
  (let [params+args (fn [bindings]
                      (->> (partition 2 bindings)
                           (reduce (fn [[prms args] [sym expr]]
                                     [(conj prms sym), (conj args expr)])
                                   [[] []])))
        emit-fn (fn [params pred expr]
                  `(fn ~params
                     (when ~pred
                       ~expr)))
        emit-call (fn [fdecl args]
                    (list* fdecl args))
        emit-branch (fn [pred binds expr]
                      (let [[params args] (params+args binds)
                            fdecl (emit-fn params pred expr)]
                        (emit-call fdecl args)))
        emit (fn emit [args]
               (let [[[pred binds expr :as clause] more]
                     (split-at 3 args)
                     n (count args)]
                 (cond 
                   (= n 0) (list `(throw (IllegalArgumentException. 
                                     (str "No matching clause: "))))
                   (< n 2) `(throw (IllegalArgumentException. 
                            (str "Must have at least 2 arguments: " ~clause)))
                   (= n 2) (cons (emit-branch pred [] 
                                              (second clause)) 
                                 (emit more))
                   (= :>> (second clause)) (cons (emit-branch pred [] expr)
                                                 (emit more))
                   :else (cons (emit-branch pred binds expr) 
                               (emit more)))))]
    `(or ~@(emit clauses))))
