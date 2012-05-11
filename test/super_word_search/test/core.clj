(ns super-word-search.test.core
  (:use [super-word-search.core])
  (:use [clojure.test]))

(def test-board {:wrap-mode true, :n 3, :m 3, :grid ["ABC" "DEF" "GHI"], :search-terms ["FED" "CAB" "GAD" "BID" "HIGH"]})

(deftest add-coords1 
  (is (= (add-coords '(0 2) '(-1 1)) '(-1 3))))

(deftest add-coords2
  (is (= (add-coords '(0 0) '(0 1)) '(0 1))))

(deftest add-coords3
  (is (= (add-coords '(3 7) '(-1 -10)) '(2 -3))))


(deftest get-element1
  (is (= (get-element '(0 1) [['A 'B 'C] ['D 'E 'F] ['G 'H 'I]]) 'B)))

(deftest get-element2
  (is (= (get-element '(0 0) [['A 'B 'C] ['D 'E 'F] ['G 'H 'I]]) 'A)))

(deftest get-element3
  (is (= (get-element '(0 0) [[]]) nil)))

(deftest get-element2
  (is (= (get-element '(0 1 2) [[[] ['A 'B 'C 'D] []] [[] [] []]]) 'C)))


(deftest wrap-coord1-less-than-board-dim
  (is (= (wrap-coord '(-1 2) 3 3) '(2 2))))

(deftest wrap-coord-greater-than-board-dim
  (is (= (wrap-coord '(1 3) 3 3) '(1 0))))

(deftest wrap-coord-inside-bounds
  (is (= (wrap-coord '(1 2) 3 3) '(1 2))))


(deftest try-wrap1
  (is (= (try-wrap '(-1 2) 3 3 true) '(2 2))))

(deftest try-wrap2
  (is (= (try-wrap '(-1 2) 3 3 false) false)))


(deftest seq-pairs1
  (is (= (seq-pairs ["ABC" "DEF"]) '([\A [0 0]]
                                    [\B [0 1]]
                                    [\C [0 2]]
                                    [\D [1 0]]
                                    [\E [1 1]]
                                    [\F [1 2]]))))

(deftest pairs-to-map1
  (is (= (pairs-to-map (seq-pairs ["DEF" "FAB"])) {\B '((1 2)),
                                                   \A '((1 1)),
                                                   \F '((1 0) (0 2)),
                                                   \E '((0 1)),
                                                   \D '((0 0))})))

(deftest pairs-to-map2
  (is (= (pairs-to-map '([\a 1] [\b 2] [\a 3] [\d 4])) {\a '(3 1),
                                                   \b '(2),
                                                   \d '(4)})))


  
