#!/usr/bin/env clojure

; README
; In most functional languages the tail recursion is optimised without
; the need for any special construct, unlike the recur construct in clojure.
; I have left the code without recur (the code is commented out) in each
; function because it still hurt my eyes seeing "recur" in place of the
; actual function name.

; defines the return from the Collatz conjecture,
; i.e. it always converges to 1 (and therefore returns always 1)
(defn collatz [n]
  (cond (= n 1)   1
        ; using recur for tail recursion
       ;(even? n) (collatz (/ n 2))
       ;:else     (collatz (+ (* 3 n) 1))))
        (even? n) (recur (/ n 2))
        :else     (recur (+ (* 3 n) 1))))

; reverses a list using a very naive algorithm
(defn shallow-reverse [lst]
  (if (empty? lst)
      lst
      (concat (shallow-reverse (rest lst)) (list (first lst)))))

; using an accumulator and tail recursion for better performance
(defn shallow-reverse-acc
  ([lst]     (shallow-reverse-acc lst []))
  ([lst acc] (if (empty? lst)
                 acc
                ;(shallow-reverse-acc (rest lst) (cons (first lst) acc)))))
                 (recur (rest lst) (cons (first lst) acc)))))

; helper function, as from 3rd lecture on clojure
(defn member? [elem lst] (some (partial = elem) lst))

; Removes duplicate elements from a list keeping always the first element
; found.  Each element is taken from the head of the input list and consed
; to the accumulator i.e. in the end the accumulator contains the reversed
; list (shallow-reverse is then used to return the list in the original order).
;
; It is very similar to the deep-reverse example but with the extra accumulator
; added to bestow performance (and allow tail recursion).
(defn remove-duplicates
  ([lst]     (remove-duplicates lst []))
  ([lst acc] (cond (empty? lst)
                     (shallow-reverse-acc acc)
                   (member? (first lst) acc)
                    ;(remove-duplicates (rest lst) acc)
                     (recur (rest lst) acc)
                   :else
                    ;(remove-duplicates (rest lst) (cons (first lst) acc)))))
                     (recur (rest lst) (cons (first lst) acc)))))

; Returns only the structure of a list containing lists, removing all elements.
; It is very similar in structure to remove-duplicates.  As well, most of the
; exercises below are similar to remove-duplicates.  This form of using cond
; and recursion is a common way of designing functions that operate on lists
; in functional languages.
(defn skeleton
  ([lst]     (skeleton lst []))
  ([lst acc] (cond (empty? lst)
                     (shallow-reverse-acc acc)
                   (seq? (first lst))
                    ;(skeleton (rest lst) (cons (skeleton (first lst) []) acc))
                     (recur (rest lst) (cons (skeleton (first lst) []) acc))
                   :else
                    ;(skeleton (rest lst) acc))))
                     (recur (rest lst) acc))))

; eliminates all elements with value vl from the list lst or from any deeper
; list contained in lst
(defn eliminate
  ([vl lst]     (eliminate vl lst []))
  ([vl lst acc] (cond (empty? lst)
                        (shallow-reverse-acc acc)
                      (= vl (first lst))
                       ;(eliminate vl (rest lst) acc)
                        (recur vl (rest lst) acc)
                      (seq? (first lst))
                       ;(eliminate vl
                        (recur     vl
                                   (rest lst)
                                   (cons (eliminate vl (first lst) []) acc))
                      :else
                       ;(eliminate vl (rest lst) (cons (first lst) acc)))))
                        (recur vl (rest lst) (cons (first lst) acc)))))

; That's as far as we can go using recursion alone, from here higher
; order functions needs to be introduced.  Starting with revargs that
; was discussed in class.  This will be needed because we defined
; member? above using [elem lst] but we will need member [lst elem]
; to write zap-chars.
(defn revargs [f] (fn [x y] (f y x)))

; all valid characters, newline, linefeed and everything between 32 and 126
(def valid-chars (cons (int \newline)
                       (cons (int \return)
                             (range (int \space) (+ 1 (int \~))))))

; Remove all characters that are not members of valid-chars from txt.  Note
; the use of member? curried with the list valid-char inside.
(defn zap-chars
  ([txt]            (zap-chars txt (partial (revargs member?) valid-chars) []))
  ([txt valid? acc] (cond (empty? txt)
                            (shallow-reverse-acc acc)
                          (valid? (int (first txt)))
                           ;(zap-chars (rest txt)
                            (recur     (rest txt)
                                       valid?
                                       (cons (first txt) acc))
                          :else
                           ;(zap-chars (rest txt) valid? acc))))
                            (recur (rest txt) valid? acc))))

; Reasoning over algorithm needed for rot-13:
; For *x* in the range of *natural* numbers from *a* to *b*,
; right rotating *x* *y* units, where *y* is a natural number is:
;   f(x) = ((x - a + y) mod (b - a)) + a
; Testing:
;   (= 126 ((fn [n] (+ 97 (mod (+ (- n 97) 13) 26))) 103))
;   (= 103 ((fn [n] (+ 97 (mod (+ (- n 97) 13) 26))) 126))
;
; Right rotates n rot units inside the rng range.  If n is not in the rng
; range the original value of n is returned.
(defn rot-num [rng rot n]
  (cond (< n (first rng)) n
        (> n (last  rng)) n
        :else
          (+ (first rng) (mod (+ (- n (first rng)) rot) (count rng)))))

(def lower-chars (range (int \a) (+ 1 (int \z))))  ; list of all a-z chars
(def upper-chars (range (int \A) (+ 1 (int \Z))))  ; all A-Z chars

; right rotates every letter in txt 13 positions forward
(defn rot-13
  ([txt]
    (rot-13 txt
            (partial rot-num lower-chars 13)
            (partial rot-num upper-chars 13)
            []))
  ([txt rot-lower rot-upper acc]
    (if (empty? txt)
        (shallow-reverse-acc acc)
       ;(rot-13 (rest txt)
        (recur  (rest txt)
                rot-lower
                rot-upper
                (cons (char (rot-lower (rot-upper (int (first txt))))) acc)))))

; Reasoning over Newton's square root method (sqrt 3)
; (/ (+ 2 (/ 3 2.0)) 2) => 1.75
; (/ (+ 1.75 (/ 3 1.75)) 2) => 1.7321428571428572
; (/ (+ 1.7321428571428572 (/ 3 1.7321428571428572)) 2) => 1.7320508100147274
;
; returns a function that computes one iteration of Newton's method,
; i.e. it computes the average of some approximation x and n (the number
; we are seeking the square root of) divided by x
(defn newton [n] (fn [x] (/ (+ x (/ n x)) 2.0)))

; iterates newton (above) to produce an infinite list of approximations,
; uses 2.0 as the 1st approximation
(defn newton-seq [n] (iterate (newton n) 2.0))

; same as absol from tests.clj
(defn abs [x] (if (pos? x) x (- 0 x)))

; drops converging approximations from the infinite list iter until the
; difference between the approximations is less than err, returns the first
; not dropped approximation
(defn root
  ([iter err] (root (drop 2 iter) err (first iter) (second iter)))
  ([iter err past curr]
    (if (<= (abs (- past curr)) err)
        curr
       ;(root (drop 1 iter) err curr (first iter)))))
        (recur (drop 1 iter) err curr (first iter)))))

; returns the square root of n, uses root to get the first good (with error
; less than 0.000015) approximation generated by newton-seq
(defn sqrt [n] (root (newton-seq n) 0.000015))

; a different approach to the Collatz conjecture,
; returns the number of iterations needed until the conjecture reaches one
(defn collatz-iter
  ([n]   (collatz-iter n 0))
  ([n i] (cond (= n 1)   i
              ;(even? n) (collatz-iter (/ n 2) (+ 1 i))
               (even? n) (recur (/ n 2) (+ 1 i))
              ;:else     (collatz-iter (+ (* 3 n) 1) (+ 1 i)))))
               :else     (recur (+ (* 3 n) 1) (+ 1 i)))))

; Returns the number with the bigger number of iterations if the number
; is used as the input of the Collatz conjecture.   Higher order functions
; are heavily used in this exercise, the function shall be read from
; the bottom up.
(defn longest-collatz [lo hi]
  (first
    ; steps the list keeping the sublist with the bigger (second) element
    (reduce (fn [[x1 y1] [x2 y2]] (if (< y1 y2) [x2 y2] [x1 y1]))
      ; [[2 2] [3 3]] -> [[2 (collatz-iter 2)] [3 (collatz-iter 3)]]
      (map (fn [[x y]] [x (collatz-iter y)])
        ; [2 3] -> [[2 2] [3 3]]
        (map (fn [x] [x x]) (range lo (+ 1 hi)))))))

; The replace function needs a discussion.  There are many ways of solving
; the problem, many of these do not employ tail recursion or laziness but
; are solutions well suited in dealing with a specific case of the problem
; (e.g. when replace needs to deal with an infinite sequence).  Below are
; three versions of replace with heavy use of recursion and one version
; of replace using a map over a map.  As we go through each solution we
; will discuss it's good and bad points and whenever the solution fulfils
; the assignment.
;
; Not lazy-seq version of replace.  It is faster for finite lists that must
; be evaluated entirely.  It is faster because it uses cons in place of
; concat (or lazy-cat), concat must find the end of the list in each recursion
; (as opposite to cons which only needs to move the head of the list).  The
; disadvantage of using cons is that it renders the accumulator reversed in
; the end of the recursion, this accumulator then needs to be reversed.
; Reversing a list needs to pass through each element and laziness cannot
; be implemented when all elements must be known.  As laziness cannot be
; implemented in the version of replace it cannot be a solution to the
; assignment.
(defn rep-notlazy
  ([value new-value seqn]     (rep-notlazy value new-value seqn []))
  ([value new-value seqn acc]
    (cond (empty? seqn)
            (shallow-reverse-acc acc)
          (seq? (first seqn))
           ;(rep-notlazy value
            (recur       value
                         new-value
                         (rest seqn)
                         (cons (rep-notlazy value
                                            new-value
                                            (first seqn) [])
                               acc))
          (= value (first seqn))
           ;(rep-notlazy value
            (recur       value
                         new-value
                         (rest seqn)
                         (cons new-value acc))
          :else
           ;(rep-notlazy value
            (recur       value
                         new-value
                         (rest seqn)
                         (cons (first seqn) acc)))))

; Tail recursive and lazy version of replace (lail, lazy and tail).  This
; solution returns the accumulator as a concatenation of lazy sequences.
; The solution is lazy for sublists, for example evaluating:
;  (rep-lail 1 2 '(1 2 '(1 3) 3))
; Returns the following lazy sequence:
;  ((lazy-seq (concat (lazy-seq (concat (lazy-seq (concat (lazy-seq '())
;                                                         (lazy-seq (list 2))))
;                                       (lazy-seq (list 2))))
;                     (lazy-seq (rep-lail 1 2 '(1 3) []))))
;   (lazy-seq (list 3))
; In this case the recursive call in the line:
;                     (lazy-seq (rep-lail 1 2 '(1 3) []))))
; will only be executed if the value is needed.
; TO enforce the example let's take just some elements from the list e.g.:
;  (take 3 (rep-lail 1 2 '(1 2 '(1 3) 3)))
; will evaluate the recursive call in
;                     (lazy-seq (rep-lail 1 2 '(1 3) []))))
; but in
;  (take 2 (rep-lail 1 2 '(1 2 '(1 3) 3)))
; that call will never be evaluated.  This happens because the return value
; is never needed and lazy-seq only evaluates the value if it is needed.
; This replace function does solve the assignment as it uses tail recursion
; at the same time as laziness is used.  On the other hand it still have one
; big problem:  This version of replace is lazy but it cannot process infinite
; sequences.  For example:
;  (take 7 (rep-lail 2 3 (iterate inc 0)))
; Would result in an overflow of stack.  Let's continue the digression in the
; next version of replace below.
(defn rep-lail
  ([value new-value seqn]     (rep-lail value new-value seqn []))
  ([value new-value seqn acc]
    (cond (empty? seqn)
            acc
          (seq? (first seqn))
           ;(rep-lail value
            (recur    value
                      new-value
                      (rest seqn)
                      (lazy-cat acc (list (rep-lail value
                                                    new-value
                                                    (first seqn) []))))
          (= value (first seqn))
           ;(rep-lail value
            (recur    value
                      new-value
                      (rest seqn)
                      (lazy-cat acc (list new-value)))
          :else
           ;(rep-lail value
            (recur    value
                      new-value
                      (rest seqn)
                      (lazy-cat acc (list (first seqn)))))))

; Truly lazy version of replace.  It is slower than rep-notlazy for finite
; lists because it calculates each sequence element one by one.  Also it
; uses more stack with each recursive call because it is not tail recursive
; like rep-lail.  On the other hand when the list being processed do not need
; to be evaluated entirely the amount of work done by rep-lazy is reduced
; because it only evaluates until the last element needed.
; rep-lazy cannot be tail recursive because lazy-seq must be added before each
; recursion, in other words the return value of each recursive step must be
; wrapped inside (lazy-seq body) and for tail recursion this return must be
; (rep-lazy args) (or (recur args) in clojure).
; The return from
;  (rep-lail 1 2 '(1 2 '(1 3) 3))
; is
;  (lazy-seq (cons 1
;            (lazy-seq (cons 2
;                      (lazy-seq (cons     (rep-lazy 1 2 '(1 3))
;                                (lazy-seq (cons 3
;                                          nil))))))))
; and thanks to the laziness on each call of cons infinite sequences can
; be processed by rep-lazy i.e. the call:
;  (take 7 (rep-lazy 2 3 (iterate inc 0)))
; Returns
;  (0 1 3 3 4 5 6)
; Unfortunately rep-lazy is not tail recursive and therefore it cannot be used
; as the solution for the assignment.
(defn rep-lazy [value new-value seqn]
  (if (empty? seqn)
      '()
      (cond (seq? (first seqn))
              (lazy-seq (cons (rep-lazy value new-value (first seqn))
                              (rep-lazy value new-value (rest seqn))))
            (= value (first seqn))
              (lazy-seq (cons new-value
                              (rep-lazy value new-value (rest seqn))))
            :else
              (lazy-seq (cons (first seqn)
                              (rep-lazy value new-value (rest seqn)))))))

; The last version of replace is the one I would use if I had to write replace
; to solve real world problems.  It is lazy without the need of using lazy-seq
; or lazy-cat because map is lazy intrinsically.  This solution is not tail
; recursive in the strict meaning of it.  But again, map do not keep anything
; on the stack from mapping over a previous element, therefore rep-map uses
; the same amount of memory independent of list size.  And the reason of tail
; recursion is to keep stack usage the same independently of the processed list
; size.  Finally, this solution have no issues with infinite lists.
(defn rep-map [vl nvl seqn]
  (map (fn [x]
         (cond (= x vl) nvl
               (seq? x) (rep-map vl nvl x)
               :else    x))
       seqn))

; finally, bind the symbol replace to the solution
(def replace rep-map)

