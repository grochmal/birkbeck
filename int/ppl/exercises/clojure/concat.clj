
;(defn my-concat [x y]
;  (cond (empty y) (do (println "empty y") (list x))
;        true    (do (println "not empty y") (list x (concat (first y) (rest y))))))

(defn fib [n]
  (cond (= 0 n) 1
        (= 1 n) 1
        :else   (+ (fib (- n 1)) (fib (- n 2)))))


(defn fib-noloop
  ([n] (fib-noloop n 1 1))
  ([n a b] (cond (= n 0) a
                 (= n 1) a
                 :else (recur (- n 1) a (+ a b)))))

(defn fib-tail [n]
  (loop [number n a1 0 a2 1]
    (cond (= 0 number) a1
          :else    (recur (- number 1) a2 (+ a1 a2)))))

(defn next-pair [pair]
  (list (second pair) (+ (first pair) (second pair))))

(defn fib-seq [n]
  (last (map first (take n (iterate next-pair '(1 1))))))

