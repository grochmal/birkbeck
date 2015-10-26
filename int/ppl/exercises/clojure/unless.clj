#!/usr/bin/env clojure

(defmacro unless [test yes no]
          (list 'if (list 'not test) yes no))

; (if (not (> 1 2)) (println "We do correct math") (println "what?!"))
; (if (not (> 2 1)) (println "2 > 1?") (println "We do math alright"))
(unless (> 1 2) (println "We do correct math") (println "what?!"))
(unless (> 2 1) (println "2 > 1?") (println "We do math alright"))

(defprotocol Compass
  (direction [c] "doc")
  (left [c] "doc")
  (right [c] "doc"))

(def directions [:north :east :south :west])
(defn turn [base amount] (rem (+ base amount) (count directions)))

(defrecord SimpleCompass [bearing]
  Compass
  (direction [_] (directions bearing))
  (left [_] (SimpleCompass. (turn bearing 3)))
  (right [_] (SimpleCompass. (turn bearing 1))))

(defprotocol Starship
  (side  [x] ":alliance or :empire")
  (model [x] "x-wing, y-wing, ...")
  (hit   [x] "returns a damaged ship")
  (dmg?  [x] "is the ship operational?")
  (fire  [x] "weapon used"))

(defrecord RebelFighter [s m a d]
  Starship
  (side  [_] s)
  (model [_] m)
  (hit   [_] (RebelFighter. s m a (not d)))
  (dmg?  [_] d)
  (fire  [_] a)
  Object
  (toString [self] (str "ship: " (side self) " dmg? " (dmg? self))))

(def f (RebelFighter. :alliance "x-wing" "phaser" false))
(println (str "We have: " f))
(println "hit it")
(def d (hit f))
(println (str "Now we have: " d))

