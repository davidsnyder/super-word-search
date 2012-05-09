(ns super-word-search.core (:use [clojure.contrib.seq-utils :only [indexed]]))

;; 8 direction vectors, up, down, up-right, down-left, etc.
(def dirs {:l [0,-1] 
           :r [0,1]
           :d [1,0]
           :u [-1,0]
           :ul [-1,-1]
           :ur [-1,1]
           :dl [1,-1]
           :dr [1,1]})

;; ======= HELPER FUNCS ==========

;; (add-coords '(0 2) '(-1 1)) => (-1 3)
(defn add-coords [c1 c2] (map + c1 c2))

;;(get-element '(0 1) [['A 'B 'C] ['D 'E 'F] ['G 'H 'I]])) => B
(defn get-element [coord matrix] (reduce get matrix coord))

;; constrains @coord to the size of the board (n m) by wrapping out-of-bound values
;; (wrap-coord '(-1 2) [3 3]) => (2 2)
;; (wrap-coord '(1 3) [3 3])  => (1 0)
;; (wrap-coord '(1 2) [3 3])  => (1 2)
(defn wrap-coord [coord n m] (map mod coord [n m]))

;; returns false if @wrap-mode is false and the new coordinate is out-of-bounds
;; otherwise returns the new coordinate
;; (try-wrap '(-1 2) 3 3 true) => '(2 2)
;; (try-wrap '(-1 2) 3 3 false) => false
(defn try-wrap [coord n m wrap-mode]
  (let [wrapped-coord (wrap-coord coord n m)]
    (if wrap-mode wrapped-coord (and (= coord wrapped-coord) coord))))

;; ======== LOOKUP TABLE ROUTINES ========  

;; Returns a list of vectors [element,[i j]] where i,j are the position of each element in 2D matrix @grid      
;;([A [0 0]] [B [0 1]] [D [0 2]] [D [1 0]] [E [1 1]] [F [1 2]] [G [2 0]] [H [2 1]] [I [2 2]])
;; Runs in O(NxM) where N is # rows and M is # columns in @grid@
(defn seq-pairs [grid]
  (map (fn [[row col val]] [val [row col]]) 
       (apply concat (map (fn [[idx val]] (map #(conj (seq %1) idx) (indexed val)))
                          (indexed grid)))))

;; Builds a map from a seq of [k v] pairs.  Values are appended to a list to
;; allow for duplicate key entries in @seq-pairs
;;{I ([2 2]), H ([2 1]), G ([2 0]), F ([1 2]), E ([1 1]), D ([1 0] [0 2]), B ([0 1]), A ([0 0])}
(defn pairs-to-map [seq-pairs] (reduce (fn [m [k v]] (assoc m k (conj (m k) (seq v))))
                                       {} seq-pairs))

;; Map of coordinate lists for each letter in word search grid
;; (letter-coord-table 'D) => ((1 0) (0 2))
(def letter-coord-table (pairs-to-map (seq-pairs grid)))

;; ============= SOLVE ===============

(def grid [[\A \B \C] [\D \E \F] [\G \H \I]])

;; board dimensions: [n m] -> [rows columns]
(def board [3 3])

(def input ["FED" "CAB" "GAD" "BID" "HIGH"])

;;FIXME: Doesn't work for single letter words, no way to know if you've overlapped yourself
(defn solve-word [word n m wrap-mode]
  (or (first (filter (fn [[coord dir]] ;filter is lazy, so first will stop at the first solution
                       (let [start-coord coord] ;hold on to the coord of the first letter
                         (loop [word (rest word), cur-coord coord, dir dir, n n, m m, wrap-mode wrap-mode]
                           (if (and (empty? word) (not= start-coord cur-coord)) ; base case: we're out of letters and we haven't overlapped start-coord
                             [start-coord cur-coord] ; yield solution
                             (let [wrapped-coord (try-wrap (add-coords cur-coord dir), n, m, wrap-mode)] ;take a step in the direction of @dir@                               
                               (if (and wrapped-coord  
                                        (= (get-element wrapped-coord grid)
                                           (first word))) ; step was valid
                                 (recur (rest word),wrapped-coord,dir,n,m,wrap-mode) ;continue searching in direction @dir@ for the rest of the letters
                                 false)))))) ; step was not valid
                     (for [coord (letter-coord-table (first word)), dir (vals dirs)] [coord dir]))) ;enumeration of 8 directions from each start-coord
      "NOT FOUND")) 
