(ns tictac-clojure.core)


(defn sel-three [v x y z]
  (vector (nth v x) (nth v y) (nth v z)))

(defn make-triples [board]
  ;turn the board into combinations of winning pos
  (let [diagnl1 (sel-three board 0 4 8) 
        diagnl2 (sel-three board 2 4 6)
        vert1   (sel-three board 0 3 6)
        vert2   (sel-three board 1 4 7)
        vert3   (sel-three board 2 5 8)
        hor1    (sel-three board 0 1 2) 
        hor2    (sel-three board 3 4 5)
        hor3    (sel-three board 6 7 8)]
        (vector diagnl1 diagnl2 vert1 vert2 vert3 hor1 hor2 hor3)))
        
(defn same? [triplet]
  (cond
    (every? #{2} triplet) true
    (every? #{1} triplet) true
    :else false))
  
(defn contain-same? [triples]
  (let [results 
        (for [t triples] (same? t))]
        (contains? (set results) true)))

(defn win? [board] 
  (let [triples (make-triples board)]
       (contain-same? triples)))

(defn empty-cells [board]
  ;indices of board that are empty
  (let [indexed (map-indexed vector board)]
        (let [filtered (filter #(= 0 (second %)) indexed)]
              (map first filtered))))

(defn tie? [board] 
  (if (not-any? #{0} board) true false))

(defn choose-rand [board sym]
  (let [cells (empty-cells board)]
       (assoc board (rand-nth cells) sym)))

(defn end? [board]
  (or (win? board) (tie? board)))

(defn get-winner [board]
  (let [triples (make-triples board)]
    (let [win-combo (first (filter #(same? %) triples))]
         (case win-combo
           [1 1 1] "computer"
           [2 2 2] "human")))) 

(defn leaf-val [board]
  (cond
    (win? board) (let [winner (get-winner board)]
                      (case winner
                        "human"   -1
                        "computer" 1))
    :else 0))  
                      
(defn get-poss-boards [board sym]
  (let [cells (empty-cells board)]
       (for [cell cells]
         (assoc board cell sym)))) 

(defn max-val-pair [eval-fn pairs] 
  (reduce (fn [x y] 
            (let [xval (eval-fn x) 
                  yval (eval-fn y)]  
                    (if (> yval xval) y x)))
           pairs))

(defn min-val-pair [eval-fn pairs] 
  (reduce (fn [x y] 
            (let [xval (eval-fn x) 
                  yval (eval-fn y)]  
                    (if (< yval xval) y x)))
           pairs))

(defn flip-sym [sym]
  (if (= sym 1) 2 1))

(defn minimax [board sym depth]
  (cond
    (end? board) (leaf-val board)
    :else (let [boards (get-poss-boards board sym)
                make-pair (fn [board] 
                           [board (minimax board (flip-sym sym) (inc depth))])
                pairs (map make-pair boards)]
             (case sym
              1 (let [max-pair (max-val-pair second pairs)]
                  (if (= depth 0) (first max-pair) (second max-pair)))
              2 (let [min-pair (min-val-pair second pairs)]
                  (if (= depth 0) (first min-pair) (second min-pair))))))) 

(defn computer [board sym]
  ;;sym -> player symbol. 1 for computer 
  (cond
    (win? board) "you won!!"
    (tie? board) "it's a tie!"
    :else (minimax board sym 0)))

(defn valid? [input board]
  (let [s (dec (read-string input))]
       (and (number? s)
            (contains? (set (empty-cells board)) s))))  

(defn input [board]
  (println "enter a number between 1 and 9 inclusive")
  (let [s (read-line)]
       (cond 
          (valid? s board) (dec (read-string s)) 
          :else "oops invalid input. try again")))

(defn human [board sym]
  ;;sym -> player symbol. 2 for human
  (let [cell (input board)]
       (cond
         (not (number? cell)) (human board sym)
         :else (assoc board cell sym))))

(defn to-str [board]
  (clojure.string/replace (apply str board)
                             #"1|2|0" {"1" "x" "2" "o" "0" "_"}))

(defn display [board-str]
  (doseq [x (partition 3 board-str)] (println x)))

(defn play [board]
  (display (to-str board))
  (cond
      (win? board) "you won!!"
      (tie? board) "it's a tie!"
      :else (let [curboard (human board 2)]
               (display (to-str curboard))
               (recur (computer curboard 1)))))

(defn -main []
  (play [0 0 0 0 0 0 0 0 0]))










