
(defn switch [f] (fn [x y] (f y x)))
(def recons (switch cons))
(defn myrev [l] (reduce recons [] l))

