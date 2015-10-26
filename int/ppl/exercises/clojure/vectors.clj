#!/usr/bin/env clojure

(def accbal (ref 120))

(defn debacc [x y] (- x y))
(defn crdacc [x y] (+ x y))

(println (str "Acc bal: " @accbal))
(def dval 50)
(println (str "Debit for " dval))
(dosync (alter accbal debacc dval))
(println (str "Acc bal: " @accbal))
(def cval 60)
(println (str "Credit for " cval))
(dosync (alter accbal crdacc cval))
(println (str "Acc bal: " @accbal))

