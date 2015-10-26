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

(def ch_q (new-q 4))
(def chairs (agent ch_q))
(def clients (Thread. #(while true (do (Thread/sleep (+ (rand longwait) shortwait))
                                       (println "client entering shop")
                                       (send chairs apush1!)))))
(def barber  (Thread. #(while true (if (< 0 (qsize ch_q))
                                       (do (Thread/sleep longwait)
                                           (send chairs ashift!))
                                       (Thread/sleep shortwait)))))

(.start clients)
(.start barber)
(Thread/sleep 10000)
(.stop clients)
(.stop barber)
(shutdown-agents)
(println (str "Done " @shavings " shavings in 10 seconds"))

