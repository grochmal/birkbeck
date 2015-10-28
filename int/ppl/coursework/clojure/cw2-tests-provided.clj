(ns user (:use clojure.test))

(deftest test-collatz
  (is (= 1 (collatz 1)))
  (is (= 1 (collatz 2)))
  (is (= 1 (collatz 3)))
  (is (= 1 (collatz 10)))
  (is (= 1 (collatz 11)))
  )

(deftest test-shallow-reverse
  (is (= '(5 4 3 2 1) (shallow-reverse '(1 2 3 4 5))))
  (is (= '(5 ((3) 4) 2 1) (shallow-reverse '(1 2 ((3) 4) 5))))
  (is (= '(5 (1 2 ((3) 4))) (shallow-reverse '((1 2 ((3) 4)) 5))))
  (is (= '() (shallow-reverse '())))
  (is (= '((())) (shallow-reverse '((())))))
  )

(deftest test-remove-duplicates
  (is (= '(1 2 3 4 5) (remove-duplicates '(1 2 3 4 5))))
  (is (or (= '(1 2 ((3 4)) 5 ()) (remove-duplicates '(1 2 1 ((3 4)) 5 ()))) (= '(2 1 ((3 4)) 5 ()) (remove-duplicates '(1 2 1 ((3 4)) 5 ())))))
  (is (= '(1) (remove-duplicates '(1 1 1))))
  (is (= '(1 (1)) (remove-duplicates '(1 1 1 (1)))))
  )

(deftest test-skeleton
  (is (= '((()) () ()) (skeleton '(1 (2 (3 4)) 5 6  (7) ( )))))
  (is (= '(()) (skeleton '(1 2 3 (5)))))
  (is (= '((()) () ()) (skeleton '(1 (2 () 3) () (5)))))
  (is (= '( ( ( ) ) ( ) ( ) ) (skeleton '(1 (2 (3 4)) 5 6 (7) () ))))
  (is (= '( ( ( ) ) ( ( ) ) ) (skeleton '((1 (2)) 3 4 ((5 6) 7)))))
  )

(deftest test-eliminate
  (is (= '(2 3 4 5) (eliminate 1 '(1 2 3 4 5 1))))
  (is (= '(() (())) (eliminate 1 '(1 (1) ((1))))))
  (is (= '(() ((2))) (eliminate 1 '(1 (1) ((1 2))))))
  (is (= '(1 2 3 4 5 1) (eliminate 6 '(1 2 3 4 5 1))))
  )

(deftest test-zap-gremlins
  (is (or (= '(\a \b \1 \2 \~) (zap-gremlins '(\a \b \1 \2 3 4 \~ \backspace \tab))) (= '"ab12~" (zap-gremlins '(\a \b \1 \2 3 4 \~ \backspace \tab))) ))
  (is (or (= '(\h \i \newline \M \r \. \A \~ \B \C) (zap-gremlins '(\h \i \newline \M \r \. \tab \A \~ \B \C))) (= '"hi\nMr.A~BC" (zap-gremlins '(\h \i \newline \M \r \. \tab \A \~ \B \C))) ))
  (is (or (= () (zap-gremlins ())) (= "" (zap-gremlins ())) ))
  )

(deftest test-rot-13
  (is (or (= '(\* \U \r \Y \y \B \space \$ \Z \l \@ \space \J \b \E \y \Q \*) (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*))) (= '"*UrYyB $Zl@ JbEyQ*") (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*))) )
  (is (or (= '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*) (rot-13 (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*)))) (= '"*HeLlO $My@ WoRlD*" (rot-13 (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*))) ) ) "Rotating the same text twice should result back to the original text")
  (is (or (= () (rot-13 ())) (= "" (rot-13 ())) ) )
  )

(defn absol [x]
  (cond (pos? x) x
    :else (- 0 x) ) )

(deftest test-sqrt
  (is (<= (absol (- 2 (sqrt 4))) 0.000015))
  (is (<= (absol (- 1.732050808 (sqrt 3))) 0.000015))
  )

(deftest test-longest-collatz
  (is (= 7 (longest-collatz 5 7)))
  (is (= 27 (longest-collatz 1 50)))
  (is (= 50 (longest-collatz 50 50)))
  (is (= 9 (longest-collatz 1 15)))
  (is (= 9 (longest-collatz 1 9)) "Boundary testing")
  (is (= 9 (longest-collatz 9 15)) "Boundary testing")
  (is (= 9 (longest-collatz 1 10)))
  (is (= 27 (longest-collatz 10 30)))
  (is (= 871 (longest-collatz 730 917)))
  )

(run-tests)
