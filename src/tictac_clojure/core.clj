(ns tictac-clojure.core)


(defn full? [board] 
  (cond
    ((not-any? #{0} board) true)
    false))

(defn make-triples [board]
  ;;turn the board vector into combinations of winning pos
  (let [diagnal1 (vector (nth board 0) (nth board 4) (nth board 8))
        diagnal2 (vector (nth board 2) (nth board 4) (nth board 6))]
    (cons diagnal1 (cons diagnal2 (partition 3 board)))))
        
(defn same? [triplet]
  (cond
    ((every? #{2} triplet) true)
    ((every? #{1} triplet) true)
    :else false))
  
(defn contain-same? [triples]
  (let [results 
        (for [t triples]
          ((same? t) true))]
    (or results)))

(defn check-win [board] 
  (let [triples (make-triples board)]
    (contain-same? triples)))

(defn empty-cells [board]
  ;;indexes of board that are empty
  (let [indexed (map-indexed vector board)]
    (let [filtered (filter #(= 0 (second %)) indexed)]
      (map first filtered))))

(defn choose-rand [board symbl]
  (let [cells (empty-cells board)]
    (assoc (rand-nth cells) symbl)))

(defn computer [board my-symbol]
  (cond
    ((full? board) (check-win board))
    (choose-rand board my-symbol)))

(defn valid? [text]
  ;;TODO modify so between 0-8. that cell free
  (if (number? (read-string text)) true false))

(defn input []
  (println "enter a number between 1 and 9 inclusive")
  (if-let [words (valid? (read-line))]
    words
    (println "oops invalid input. come again")))

(defn human [board my-symbol]
  (cond
    ((nil? (input)) (human board my-symbol))
    (let [cell (input)]
      (assoc cell my-symbol))))

(defn play [board]
  (let [curboard (computer board 1)]
    (println curboard)
    (recur (human curboard 2))))

(defn -main []
  (play [0 0 0 0 0 0 0 0 0]))










