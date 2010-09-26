;; Utility function for human to easily play in REPL.

(ns vsccp.repl
 (:use [vsccp.logic])
 (:import vsccp.logic.Move))

(def *max-ply* 4)

(def b (atom (make-board)))

(defn reset [] (reset! b (make-board)))

(print-board @b)

(defn hm [r1 c1 r2 c2]
  (let [from (+ (* r1 9) c1)
        to   (+ (* r2 9) c2)
        m    (Move. from to)]
    (swap! b move m)
    (print-board @b)))

(defn cm []
  (let [m (alpha-beta @b true *max-ply*)]
    (swap! b move m)
    (print-board @b)))
