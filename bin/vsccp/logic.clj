(ns vsccp.logic)

(defn make-table []
  (str "車馬象士將士象馬車"
       "         "
       " 砲     砲 "
       "兵 兵 兵 兵 兵"
       "         "
       "         "
       "卒 卒 卒 卒 卒"
       " 炮      炮 "
       "         "
       "俥傌相仕帥仕相傌俥"))

(defn black? [piece]
  (.contains "將士象車砲馬兵" (str piece)))

(defn print-piece [piece]
  (if (or (black? piece) (= piece \space))
      (print piece)
      (print "\033[31;1m" piece "\033[0m")))

(defn print-table [table]
  (map (fn [index]
         (let [piece (nth table index)]
           (print-piece piece)
           (if (zero? (rem (+ index 1) 9)) (println))))
       (range 90)))
