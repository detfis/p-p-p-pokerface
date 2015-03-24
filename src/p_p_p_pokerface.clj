(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn chain-of-five-numbers [sequence]
  (let [[first_number _] sequence]
    (= (range first_number (+ first_number 5)) sequence)))
  


(defn rank [card]
  (let[[frst _] card]
    (if (Character/isDigit frst)
      (Integer/valueOf (str frst))
      (replacements frst))))

(defn suit [card]
  (let [[_ scnd] card]
    (str scnd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2)) 

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3)) 

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4)) 

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5)) 

(defn full-house? [hand]
  (= [2 3] (seq (sort (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (= [1 2 2] (seq (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [values (seq (sort (vals (frequencies (map rank hand)))))
        values-with-lower-ace (if (= (last values) 14)
                                (sort (assoc values 1 14))
                                values)]
    (or (chain-of-five-numbers values) (chain-of-five-numbers values-with-lower-ace))))
    
(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  nil)
