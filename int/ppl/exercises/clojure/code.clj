#!/usr/bin/env clojure

(defn big [st n] (< n (count st)))

(def st1 "quite long")
(if (big st1 6) (println (str "[" st1 "] is long")))
(def st2 "short")
(if (big st2 6) (println (str "[" st2 "] is long")))

(def class_map { (class '())    :list
               , (class '(nil)) :list
               , (class [])     :vector
               , (class {})     :map
               })

(defn collection-type [col] (class_map (class col)))
(println "The '() is:   "  (collection-type '()  ))
(println "The '(\\s) is: " (collection-type '(\s)))
(println "The [] is:    "  (collection-type []   ))
(println "The {} is:    "  (collection-type {}   ))
(println "The \\s is:    " (collection-type \s   ))

