; David Snyder
; May 2012
; Super Word Search 

(def grid [[A B C] [D E F] [G H I]])

(def dirs {:l '(-1,0) ;left, right, etc. 
           :r '(1,0)
           :d '(0,-1)
           :u '(0,1)
           :ul '(-1,1) ;diagonals - up-left, up-right, etc. 
           :ur '(1,1)
           :dl '(-1,-1)
           :dr '(1,-1)})
           


