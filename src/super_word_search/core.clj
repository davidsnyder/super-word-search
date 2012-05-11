(ns super-word-search.core
  (use [clojure.contrib.seq-utils :only [indexed]]))

;; 8 direction vectors, up, down, up-right, down-left, etc.
(def dirs {:l '(0,-1) :r '(0,1) :d '(1,0) :u '(-1,0) :ul '(-1,-1) :ur '(-1,1) :dl '(1,-1) :dr '(1,1)})

;; ======= HELPER FUNCS ==========

(defn add-coords [c1 c2] (map + c1 c2))

;;(get-element '(0 1) [['A 'B 'C] ['D 'E 'F] ['G 'H 'I]])) => B
(defn get-element [coord matrix] (reduce get matrix coord))

;; constrains @coord to the size of the board (n m) by wrapping out-of-bound values
(defn wrap-coord [coord n m] (map mod coord [n m]))

;; returns false if @wrap-mode is false and the new coordinate is out-of-bounds
;; otherwise returns the new coordinate
(defn try-wrap [coord n m wrap-mode]
  (let [wrapped-coord (wrap-coord coord n m)]
    (if wrap-mode
      wrapped-coord
      (and (= coord wrapped-coord) coord))))

;; ======== LOOKUP TABLE ROUTINES ========  

;; Returns a list of vectors [element,[i j]] where i,j are the position of each element in 2D matrix @grid      
;; Runs in O(NxM) where N is # rows and M is # columns in @grid@
(defn seq-pairs [grid]
  (map (fn [[row col val]] [val [row col]]) 
       (apply concat (map (fn [[idx row]] (map #(conj (seq %1) idx) (indexed row)))
                          (indexed grid)))))

;; Builds a map from a seq of [k v] pairs.  Values are appended to a list to
;; allow for duplicate key entries in @seq-pairs
(defn pairs-to-map [seq-pairs] (reduce (fn [m [k v]] (assoc m k (conj (m k) v))) {} seq-pairs))

;; Map of coordinate lists for each letter in word search grid
;; (letter-coord-table 'D) => ((1 0) (0 2))
(defn letter-coord-table [grid] (pairs-to-map (seq-pairs grid))) 
(def letter-coord-memo (memoize letter-coord-table))

;; ============= SOLVE ===============

(defn solve-word [word board]
  (or (reduce (fn [b a] (or b a)) false (map (fn [[coord dir]] ;map is lazy, so first will stop at the first solution
                    (let [start-coord coord] ;hold on to the coord of the first letter
                      (loop [word (rest word), cur-coord coord, dir dir, board board]
                        (if (and (empty? word) (not= start-coord cur-coord)) ; base case: we're out of letters and we haven't overlapped start-coord
                          [start-coord cur-coord] ; yield solution
                          (let [wrapped-coord (try-wrap (add-coords cur-coord dir) (board :n) (board :m) (board :wrap-mode))] ;else take a step in the direction of @dir@
                            (if (and wrapped-coord  ;step is valid
                                     (= (get-element wrapped-coord (board :grid))
                                        (first word))) ;the letter in this direction matches the next letter of word
                              (recur (rest word),wrapped-coord,dir,board) ;continue searching in direction @dir@ for the rest of the letters
                              false)))))) ; step was not valid
                  (for [coord ((letter-coord-memo (board :grid)) (first word)), dir (vals dirs)] [coord dir]))) ;enumeration of 8 directions from each start-coord
      "NOT FOUND"))

;; ========== MAIN =========

(defn parse-input [input]
  (reduce (fn [map line]
            (cond (re-matches #"(\d)\s(\d)" line) ; set the grid dimensions
                  (let [[_ n m] (re-matches #"(\d)\s(\d)" line)] (into map [[:n (read-string n)] [:m (read-string m)],[:grid-count (read-string n)]])) 
                  (re-matches #"\d" line) ;set the number of search words
                  (let [term-count (read-string (re-matches #"\d" line))] (assoc map :tc term-count)) 
                  (re-matches #"NO_WRAP|WRAP" line) ;set the wrap-mode
                  (let [wrap-mode (re-matches #"NO_WRAP|WRAP" line)] (assoc map :wrap-mode (= "WRAP" wrap-mode)))
                  (> (:grid-count map) 0 ) ;append the grid row and decrement the counter
                  (into map [[:grid-count (dec (:grid-count map))] [:grid (conj (or (:grid map) []) line)]]) 
                  (> (:tc map) 0) ;append the search term and decrement the counter
                  (into map [[:tc (dec (:tc map))] [:search-terms (conj (or (:search-terms map) []) line)]]))) 
          {}
          input))

(defn -main [& args]
  (let [board (parse-input (line-seq (java.io.BufferedReader. *in*)))]
    (doseq [result (map #(solve-word %1 board) (board :search-terms))]
      (if (= result "NOT FOUND")
        (printf "NOT FOUND\n")
        (apply printf "(%d,%d) (%d,%d)\n" (flatten result))
    ))))
