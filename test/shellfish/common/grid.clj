(ns shellfish.common.grid)

(defn rc->idx 
  ([n [row col]]
   (-> (* row n ) (+ col) )))

(defn idx->rc 
  ([n idx]
   [(quot idx n), (rem idx n)]))
