(ns vsccp.logic)

;; '　' = \u3000: zenkaku Japanese space character

(defn make-board []
  "Returns a board, which is a Java primitive array of 90 characters."
  (let [s (str "車馬象士將士象馬車"
               "　　　　　　　　　"  ; <- zenkaku Japanese space characters
               "　砲　　　　　砲　"
               "兵　兵　兵　兵　兵"
               "　　　　　　　　　"
               "　　　　　　　　　"
               "卒　卒　卒　卒　卒"
               "　炮　　　　　炮　"
               "　　　　　　　　　"
               "俥傌相仕帥仕相傌俥")]
    (.toCharArray s)))

;; Definition for a move
;; from, to: int
(defrecord Move [from to])

(defn black? [piece]
  (or (= piece \將)
      (= piece \士)
      (= piece \象)
      (= piece \車)
      (= piece \砲)
      (= piece \馬)
      (= piece \兵)))

(defn red? [piece]
  (or (= piece \帥)
      (= piece \仕)
      (= piece \相)
      (= piece \俥)
      (= piece \炮)
      (= piece \傌)
      (= piece \卒)))

(defn piece-score
  "Returns the score for the piece from the perspective of the RED player."
  [piece]
  (case piece
        \將  9999
        \帥 -9999
        \士  20
        \仕 -20
        \象  40
        \相 -40
        \車  90
        \俥 -90
        \砲  50
        \炮 -50
        \馬  45
        \傌 -45
        \兵  10
        \卒 -10
             0))

(defn board-score
  "Returns the score for the board for the RED player."
  [board]
  (let [scores (map piece-score board)
        sum    (reduce + scores)]
    sum))

;-------------------------------------------------------------------------------

(defn print-piece [piece]
  (if (or (black? piece) (= piece \u3000))
      (print piece)
      (print (str "\033[31;1m" piece "\033[0m"))))  ; Red color in *nix console

(defn print-board [board]
  (print " 0 1 2 3 4 5 6 7 8")
  (dotimes [idx 90]
    (let [piece (nth board idx)]
      (if (zero? (rem idx 9)) (do (println) (print (quot idx 9))))
      (print-piece piece)))
  (println))

;-------------------------------------------------------------------------------

(defn different-side?
  "Piece at idx1 or 2 may be blank."
  [board idx1 idx2]
  (let [piece1 (nth board idx1)
        piece2 (nth board idx2)]
    (or (and (black? piece1) (not (black? piece2)))
        (and (red?   piece1) (not (red?   piece2))))))

(defn enemy?
  "Returns true if piece at idx1 and 2 are not blank and of different side."
  [board idx1 idx2]
  (let [piece1 (nth board idx1)
        piece2 (nth board idx2)]
    (or (and (black? piece1) (red?   piece2))
        (and (red?   piece1) (black? piece2)))))

(defn in-black-palace? [idx]
  (let [row (quot idx 9)
        col (rem  idx 9)]
    (and (>= row 0) (<= row 2) (>= col 3) (<= col 5))))

(defn in-red-palace? [idx]
  (let [row (quot idx 9)
        col (rem  idx 9)]
    (and (>= row 7) (<= row 9) (>= col 3) (<= col 5))))

(defn elephant-move-not-blocked? [board from to]
  (let [idx (quot (+ from to) 2)
        piece (nth board idx)]
    (= piece \u3000)))

(defn in-black-side? [idx]
  (let [row (quot idx 9)
        col (rem  idx 9)]
    (and (>= row 0) (<= row 4) (>= col 0) (<= col 8))))

(defn in-red-side? [idx]
  (let [row (quot idx 9)
        col (rem  idx 9)]
    (and (>= row 5) (<= row 9) (>= col 0) (<= col 8))))

;-------------------------------------------------------------------------------

;; The below functions:
;; * Return indexes in the board of destinations of the generated moves
;; * Do not check for general faced when generating moves
;; * Moves are generated and checked from 3 o'clock anticlockwise

(defn moves-for-black-general [board from]
  (let [tos [(inc from) (- from 9) (dec from) (+ from 9)]]
    (filter
      #(and (in-black-palace? %) (different-side? board from %))
      tos)))

(defn moves-for-red-general [board from]
  (let [tos [(inc from) (- from 9) (dec from) (+ from 9)]]
    (filter
      #(and (in-red-palace? %) (different-side? board from %))
      tos)))

(defn moves-for-black-guard [board from]
  (let [tos [(- from 8) (- from 10) (+ from 8) (+ from 10)]]
    (filter
      #(and (in-black-palace? %) (different-side? board from %))
      tos)))

(defn moves-for-red-guard [board from]
  (let [tos [(- from 8) (- from 10) (+ from 8) (+ from 10)]]
    (filter
      #(and (in-red-palace? %) (different-side? board from %))
      tos)))

(defn moves-for-black-elephant [board from]
  (let [tos (case from
                  2  [18 22]
                  6  [22 26]
                  18 [2  38]
                  22 [6  2 38 42]
                  26 [6  42]
                  38 [22 18]
                  42 [26 22])]
    (filter
      #(and (different-side? board from %) (in-black-side? %) (elephant-move-not-blocked? board from %))
      tos)))

(defn moves-for-red-elephant [board from]
  (let [tos (case from
                  87  [71 67]
                  83  [67 63]
                  71  [87 51]
                  67  [83 87 51 47]
                  63  [83 47]
                  51  [67 71]
                  47  [63 67])]
    (filter
      #(and (different-side? board from %) (in-red-side? %) (elephant-move-not-blocked? board from %))
      tos)))

(defn moves-for-chariot [board from]
  (let [row     (quot from 9)
        col     (rem  from 9)
        rights  (if (=     col 8) nil (range (inc from) (* (inc row) 9)))
        ups     (if (zero? row)   nil (range (- from 9) (- col 9) -9))
        lefts   (if (zero? col)   nil (range (dec from) (dec (* row 9)) -1))
        downs   (if (=     row 9) nil (range (+ from 9) (+ 90 col) 9))

        rights2 (take-while #(different-side? board from %) rights)
        ups2    (take-while #(different-side? board from %) ups)
        lefts2  (take-while #(different-side? board from %) lefts)
        downs2  (take-while #(different-side? board from %) downs)]
    (concat rights2 ups2 lefts2 downs2)))

(defn moves-for-cannon [board from]
   (let [row     (quot from 9)
         col     (rem  from 9)
         rights  (if (=     col 8) nil (range (inc from) (* (inc row) 9)))
         ups     (if (zero? row)   nil (range (- from 9) (- col 9) -9))
         lefts   (if (zero? col)   nil (range (dec from) (dec (* row 9)) -1))
         downs   (if (=     row 9) nil (range (+ from 9) (+ 90 col) 9))

         rights2 (take-while #(= (nth board %) \u3000) rights)
         ups2    (take-while #(= (nth board %) \u3000) ups)
         lefts2  (take-while #(= (nth board %) \u3000) lefts)
         downs2  (take-while #(= (nth board %) \u3000) downs)

         rights3 (let [f  (if (empty? rights2) from (last rights2))
                       f2 (+ f  2)
                       r  (quot f2 9)]
                   (if (= r row) (range f2 (* (inc r) 9)) nil))
         lefts3  (let [f  (if (empty? lefts2)  from (last lefts2))
                       f2 (- f  2)
                       r  (quot f2 9)]
                   (if (= r row) (range f2 (dec (* r 9)) -1) nil))
         ups3    (let [f  (if (empty? ups2)   from (last ups2))
                       f2 (- f  18)]
                   (if (neg? f2)   nil (range f2 (- col 9) -9)))
         downs3  (let [f  (if (empty? downs2) from (last downs2))
                       f2 (+ f  18)]
                   (if (>= f2 90)  nil (range f2 (+ 90 col) 9)))

         right4  (first (for [to rights3, :when (enemy? board from to)] to))
         up4     (first (for [to ups3,    :when (enemy? board from to)] to))
         left4   (first (for [to lefts3,  :when (enemy? board from to)] to))
         down4   (first (for [to downs3,  :when (enemy? board from to)] to))

         ret1    (concat rights2 ups2 lefts2 downs2)
         ret2    (if (nil? right4) ret1 (conj ret1 right4))
         ret3    (if (nil? up4)    ret2 (conj ret2 up4))
         ret4    (if (nil? left4)  ret3 (conj ret3 left4))
         ret5    (if (nil? down4)  ret4 (conj ret4 down4))]
    ret5))

(defn moves-for-horse [board from]
  (let [row  (quot from 9)
        col  (rem  from 9)

        ; Angles in o'clock
        a2   (if (or (< row 1) (> col 6)) nil (if (= (nth board (+ from 1)) \u3000) [(- from 7)]  nil))
        a1   (if (or (< row 2) (> col 7)) nil (if (= (nth board (- from 9)) \u3000) [(- from 17)] nil))
        a11  (if (or (< row 2) (< col 1)) nil (if (= (nth board (- from 9)) \u3000) [(- from 19)] nil))
        a10  (if (or (< row 1) (< col 2)) nil (if (= (nth board (- from 1)) \u3000) [(- from 11)] nil))
        a8   (if (or (> row 8) (< col 2)) nil (if (= (nth board (- from 1)) \u3000) [(+ from 7)]  nil))
        a7   (if (or (> row 7) (< col 1)) nil (if (= (nth board (+ from 9)) \u3000) [(+ from 17)] nil))
        a5   (if (or (> row 7) (> col 7)) nil (if (= (nth board (+ from 9)) \u3000) [(+ from 19)] nil))
        a4   (if (or (> row 8) (> col 6)) nil (if (= (nth board (+ from 1)) \u3000) [(+ from 7)]  nil))

        tos (concat a2 a1 a11 a10 a8 a7 a5 a4)]
    (filter #(different-side? board from %) tos)))

(defn moves-for-soldier [board from]
  (let [row      (quot from 9)
        col      (rem  from 9)
        piece    (nth board from)

        rights   (if (or (and (black? piece) (> row 4)) (and (red? piece) (< row 5)))
                     (if (= col 8) nil [(inc from)])
                     nil)
        lefts    (if (or (and (black? piece) (> row 4)) (and (red? piece) (< row 5)))
                     (if (zero? col) nil [(dec from)])
                     nil)
        vertical (if (black? piece)
                     (if (=     row 9) nil [(+ from 9)])
                     (if (zero? row)   nil [(- from 9)]))
        tos      (concat rights lefts vertical)]
    (filter #(different-side? board from %) tos)))

;-------------------------------------------------------------------------------

(defn move
  "Returns a new board. The move should be valid."
  [board move]
  (let [piece (aget   board (:from move))
        ret   (aclone board)]
    (aset-char ret (:from move) \u3000)
    (aset-char ret (:to   move) piece)
    ret))

(defn black-idx [board]
  (if (= (nth board 3) \將)
      3
      (if (= (nth board 4) \將)
          4
          (if (= (nth board 5) \將)
              5
              (if (= (nth board 12) \將)
                  12
                  (if (= (nth board 13) \將)
                      13
                      (if (= (nth board 14) \將)
                          14
                          (if (= (nth board 21) \將)
                              21
                              (if (= (nth board 22) \將)
                                  22
                                  (if (= (nth board 23) \將) 23))))))))))

(defn red-idx [board]
  (if (= (nth board 66) \帥)
      66
      (if (= (nth board 67) \帥)
          67
          (if (= (nth board 68) \帥)
              68
              (if (= (nth board 75) \帥)
                  75
                  (if (= (nth board 76) \帥)
                      76
                      (if (= (nth board 77) \帥)
                          77
                          (if (= (nth board 84) \帥)
                              84
                              (if (= (nth board 85) \帥)
                                  85
                                  (if (= (nth board 86) \帥) 86))))))))))

(defn generals-faced? [board]
  (let [bi (black-idx board)]
    (if (nil? bi)
        false
        (let [ri (red-idx board)]
          (if (nil? ri)
              false
              (let [black-col (rem bi 9)
                    red-col   (rem ri 9)]
                (if-not (= black-col red-col)
                        false
                        (let [black-row (quot bi 9)
                              red-row   (quot ri   9)
                              rows      (range (+ black-row 1) red-row)
                              blocked   (some #(let [idx (+ (* % 9) black-col)
                                                     piece (nth board idx)]
                                                 (not (= piece \u3000)))
                                              rows)]
                          (not blocked)))))))))

(defn moves-for-piece
  "Returns all possible moves for a piece at from (index)."
  [board from]
  (let [piece (nth board from)
        fun   (case piece
                    \將 moves-for-black-general
                    \帥 moves-for-red-general
                    \士 moves-for-black-guard
                    \仕 moves-for-red-guard
                    \象 moves-for-black-elephant
                    \相 moves-for-red-elephant
                    \車 moves-for-chariot
                    \俥 moves-for-chariot
                    \砲 moves-for-cannon
                    \炮 moves-for-cannon
                    \馬 moves-for-horse
                    \傌 moves-for-horse
                    \兵 moves-for-soldier
                    \卒 moves-for-soldier
                        nil)
        tos   (if (nil? fun) nil (fun board from))]
    (filter
      #(let [m (Move. from %)
             b (move board m)]
         (not (generals-faced? b)))
      tos)))

(defn moves
  "Returns all possible moves from board"
  [board]
  (let [froms             (range 90)
        from-to-moves-fun (fn [from]
                            (let [tos (moves-for-piece board from)]
                              (map (fn [to] (Move. from to)) tos)))
        moves-coll        (map from-to-moves-fun froms)]
    (flatten moves-coll)))

;-------------------------------------------------------------------------------

(defn alpha-beta-recursive [board alpha beta depth]
  (if (zero? depth)
      [(board-score board) board]
      (let [coll (moves board)
            f    (fn [[best-value best-board improved-alpha] m]
                   (if (>= best-value beta)
                       [best-value best-board improved-alpha]
                       (let [improved-alpha2 (if (> best-value improved-alpha) best-value improved-alpha)
                             trial-board     (move board m)
                             [value b]       (alpha-beta-recursive trial-board (- beta) (- improved-alpha2) (dec depth))
                             value2          (- value)]
                         (if (> value2 best-value)
                             [value2     b           improved-alpha2]
                             [best-value best-board  improved-alpha2]))))
            [best-value best-board _] (reduce f [-9999 board alpha] coll)]
        [best-value best-board])))

(defn alpha-beta
  "black: true to play for BLACK player, false for RED player"
  [board black max-ply]
  (if black
    (alpha-beta-recursive board -9999 9999 max-ply)
    (alpha-beta-recursive board -9999 9999 max-ply)))

(def b (make-board))
(let [[x y] (alpha-beta-recursive b -9999 9999 4)]
  (println x)
  (print-board y))
