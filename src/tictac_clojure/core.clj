(ns tictac-clojure.core)


(defn full? [board] 
  (cond
    (not-any? #{0} board) true
    :else false))

(defn sel-three [v x y z]
  (vector (nth v x) (nth v y) (nth v z)))

(defn make-triples [board]
  ;;turn the board vector into combinations of winning pos
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
        (for [t triples]
          (same? t))]
        (contains? (set results) true)))

(defn win? [board] 
  (let [triples (make-triples board)]
       (contain-same? triples)))

(defn empty-cells [board]
  ;;indexes of board that are empty
  (let [indexed (map-indexed vector board)]
        (let [filtered (filter #(= 0 (second %)) indexed)]
              (map first filtered))))

(defn choose-rand [board symbl]
  (let [cells (empty-cells board)]
       (assoc board (rand-nth cells) symbl)))

(defn end? [board]
  (or (win? board) (tie? board)))

(defn get-winner [board]
  )

(defn leaf-val [board]
  (cond
    (win? board) (let [winner (get-winner board)]
                      (case winner
                        "human"   -1
                        "computer" 1))
    :else 0))  
                      
(defn get-poss-boards [board symbl]
  (let [cells (empty-cells board)]
       (for [cell cells]
         (assoc board cell symbl)))) 

(defn get-board [op boards sym depth]
;;returns best-move board
  (reduce (fn [ x y]
            (let [xval (minimax x sym (inc depth))
                  yval (minimax y sym (inc depth))]
              (case op
                "max" (if (> yval xval) y x)
                "min" (if (< yval xval) y x))))
                                        boards))

(defn minimax [board sym depth]
  (cond
    (end? board) (leaf-val board)
    :else (let [boards (get-poss-boards board sym)]
               (case sym 
                 1 (get-board "max" boards 2 depth)
                 2 (get-board "min" boards 1 depth))))) 

(defn computer [board symbl]
  (minimax board symbl 0))

(defn tie? [board]
  (if (empty? (empty-cells board)) true false))

(defn valid? [input board]
  (let [s (read-string input)]
       (and (number? s) (contains? (set (empty-cells board)) s))))  

(defn input [board]
  (println "enter a number between 1 and 9 inclusive")
  (let [s (read-line)]
  (cond 
    (valid? s board) (read-string s) 
    :else "oops invalid input. try again")))

(defn human [board symbl]
  (let [cell (input board)]
    (cond
     (not (number? cell)) (human board symbl)
     :else (assoc board cell symbl))))

(defn play [board]
  (println board)
  (cond
    (win? board) "you won!!"
    (tie? board) "it's a tie"
    :else (let [curboard (computer board 1)]
               (println curboard)
               (recur (human curboard 2)))))

(defn -main []
  (play [0 0 0 0 0 0 0 0 0]))










