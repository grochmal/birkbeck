#!/usr/bin/env clojure

(defprotocol Crap
  (limit   [q])
  (push!   [q x])
  (push1!  [q])
  (shift!  [q])
  (qsize   [q])
  (push1-or-bail! [q])
  (shift-or-bail! [q])
)

(defrecord BlQueue [limit queue]
  Crap
  (limit   [_]   limit)
  (push!   [_ x] (.put queue x))
  (push1!  [q]   (push! q 1))
  (shift!  [_]   (.take queue))
  (qsize   [_]   (.size queue))
  (push1-or-bail! [q] (if (< (qsize q) limit) (do (push1! q) true) false))
  (shift-or-bail! [q] (if (> (qsize q) 0)     (do (shift! q) true) false))
)

(defn new-q [limit]
  (BlQueue. limit (java.util.concurrent.ArrayBlockingQueue. limit)))

(def shortwait 10)
(def longwait 20)
(def shavings (agent 0))

(defn apush1! [q] (do (if (push1-or-bail! q)
                          (println "client seated")
                          (println "client bailed, no space!"))
                      q))
(defn ashift! [q] (do (if (shift-or-bail! q)
                          (do (send shavings inc)
                              (println (str @shavings " shavings done"))))
                      q))
;(defn ashift! [q] (do (if (shift-or-bail! q)
;                          (do (send shavings inc)
;                              (Thread/sleep 200)
;                              (println (str @shavings " shavings done")))
;                          ;(do (Thread/sleep 10)
;                          ;    (println "barber sleeping, no clients!")))
;                          (Thread/sleep 100))
;                      q))

(def ch_q (new-q 4))
(def chairs (agent ch_q))
(def clients (Thread. #(while true (do (Thread/sleep (+ (rand longwait) shortwait))
                                       (println "client entering shop")
                                       (send chairs apush1!)))))
(def barber  (Thread. #(while true (if (< 0 (qsize ch_q))
                                       (do (Thread/sleep longwait)
                                           (send chairs ashift!))
                                       (Thread/sleep shortwait)))))

;(send chairs apush1!)
;(send chairs apush1!)
(.start clients)
(.start barber)
(Thread/sleep 10000)
(.stop clients)
(.stop barber)
(shutdown-agents)
(println (str "Done " @shavings " shavings in 10 seconds"))

;(defn shave! [ch] (while true (do (Thread/sleep 2000)
;                                  (println "shaving!")
;                                  (send ch shift-or-bail!)  ; it dies!
;                                  (println "shaved"))))
;(defn shave! [q]
;  (while true (if (shift-or-bail! q)
;                  (do (Thread/sleep 5000)
;                      (println "shaved!"))
;                  (do (Thread/sleep 5000)
;                      (println "bail!")))))
;(defn shave! [q] (while true (shift! q)
;                             (Thread/sleep 2000)
;                             (println "shaved!")
;                             q))

;(def accbal (ref 120))

;(defn debacc [x y] (- x y))
;(defn crdacc [x y] (+ x y))

;(def dval 50)
;(dosync (alter accbal debacc dval))
;(def cval 60)
;(dosync (alter accbal crdacc cval))

