(ns vsccp.logic)

;; '　' = \u3000: zenkaku Japanese space character

(defn make-board []
  "Returns a board, which is a string of 90 zenkaku Japanese characters"
  (str "車馬象士將士象馬車"
       "　　　　　　　　　"  ; <- zenkaku Japanese space characters
       "　砲　　　　　砲　"
       "兵　兵　兵　兵　兵"
       "　　　　　　　　　"
       "　　　　　　　　　"
       "卒　卒　卒　卒　卒"
       "　炮　　　　　炮　"
       "　　　　　　　　　"
       "俥傌相仕帥仕相傌俥"))

;; Definition for a move
;; from, to: int
(defrecord Move [from to])

(defn black? [piece]
  (>= (.indexOf "將士象車砲馬兵" (int piece)) 0))

(defn red? [piece]
  (>= (.indexOf "帥仕相俥炮傌卒" (int piece)) 0))

;-------------------------------------------------------------------------------

(defn print-piece [piece]
  (if (or (black? piece) (= piece \u3000))
      (print piece)
      (print (str "\033[31;1m" piece "\033[0m"))))  ; Red color in *nix console

(defn print-board [board]
  (dotimes [index 90]
    (let [piece (nth board index)]
      (print-piece piece)
      (if (zero? (rem (+ index 1) 9)) (println)))))

;-------------------------------------------------------------------------------

(defn different-side? [board index1 index2]
  "Piece at index1 or 2 may be blank."
  (let [piece1 (nth board index1)
        piece2 (nth board index2)]
    (or (and (black? piece1) (not (black? piece2)))
        (and (red?   piece1) (not (red?   piece2))))))

(defn enemy? [board index1 index2]
  "Returns true if piece at index1 and 2 are not blank and of different side."
  (let [piece1 (nth board index1)
        piece2 (nth board index2)]
    (or (and (black? piece1) (red?   piece2))
        (and (red?   piece1) (black? piece2)))))

(defn in-black-palace? [index]
  (let [row (/   index 9)
        col (rem index 9)]
    (and (>= row 0) (<= row 2) (>= col 3) (<= col 5))))

(defn in-red-palace? [index]
  (let [row (/   index 9)
        col (rem index 9)]
    (and (>= row 7) (<= row 9) (>= col 3) (<= col 5))))

(defn elephant-move-not-blocked? [board from to]
  (let [index (/ (+ from to) 2)
        piece (nth board index)]
    (= piece \u3000)))

(defn in-black-side? [index]
  (let [row (/   index 9)
        col (rem index 9)]
    (and (>= row 0) (<= row 4) (>= col 0) (<= col 8))))

(defn in-red-side? [index]
  (let [row (/   index 9)
        col (rem index 9)]
    (and (>= row 5) (<= row 9) (>= col 0) (<= col 8))))

;-------------------------------------------------------------------------------

;; The below functions:
;; * Return indexes in the board of destinations of the generated moves
;; * Do not check for general faced when generating moves
;; * Moves are generated and checked from 3 o'clock anticlockwise

(defn moves-for-black-general [board from]
  (let [tos [(+ from 1) (- from 9) (- from 1) (+ from 9)]]
    (filter
      #(and (different-side? board from %) (in-black-palace? %))
      tos)))

(defn moves-for-red-general [board from]
  (let [tos [(+ from 1) (- from 9) (- from 1) (+ from 9)]]
    (filter
      #(and (different-side? board from %) (in-red-palace? %))
      tos)))

(defn moves-for-black-guard [board from]
  (let [tos [(- from 8) (- from 10) (+ from 8) (+ from 10)]]
    (filter
      #(and (different-side? board from %) (in-black-palace? %))
      tos)))

(defn moves-for-red-guard [board from]
  (let [tos [(- from 8) (- from 10) (+ from 8) (+ from 10)]]
    (filter
      #(and (different-side? board from %) (in-red-palace? %))
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
  (let [tos [(- from 8) (- from 10) (+ from 8) (+ from 10)]]
    (filter
      #(and (different-side? board from %) (in-red-side? %) (elephant-move-not-blocked? board from %))
      tos)))

(defn moves-for-chariot [board from]
  (let [rights  (if (= (rem from 9) 8) nil (map #(+ from 1) (range 1 9)))
        ups     (                           map #(- from 9) (range 1 10))
        lefts   (if (= (rem from 9) 0) nil (map #(- from 1) (range 1 9)))
        downs   (                           map #(+ from 9) (range 1 10))

        rights2 (take-while #(and (<= (rem % 9) 8) (different-side? board from %)) rights)
        ups2    (take-while #(and (>= % 0)         (different-side? board from %)) ups)
        lefts2  (take-while #(and (>= (rem % 9) 0) (different-side? board from %)) lefts)
        downs2  (take-while #(and (< % 90)         (different-side? board from %)) downs)]
    (flatten rights2 ups2 lefts2 downs2)))

(defn moves-for-cannon [board from]
   (let [rights  (if (= (rem from 9) 8) nil (map #(+ from 1) (range 1 9)))
         ups     (                           map #(- from 9) (range 1 10))
         lefts   (if (= (rem from 9) 0) nil (map #(- from 1) (range 1 9)))
         downs   (                           map #(+ from 9) (range 1 10))

         rights2 (take-while #(and (<= (rem % 9) 8) (= (nth board %) \u3000)) rights)
         ups2    (take-while #(and (>= % 0)         (= (nth board %) \u3000)) ups)
         lefts2  (take-while #(and (>= (rem % 9) 0) (= (nth board %) \u3000)) lefts)
         downs2  (take-while #(and (< % 90)         (= (nth board %) \u3000)) downs)

         right-most (last rights2)
         up-most    (last ups2)
         left-most  (last lefts2)
         down-most  (last downs2)

         rights3    (if (= (rem right-most 9) 8) nil (map #(+ from 1) (range 1 8)))
         ups3       (                                 map #(- from 9) (range 1 9))
         lefts3     (if (= (rem left-most  9) 0) nil (map #(- from 1) (range 1 8)))
         downs3     (                                 map #(+ from 9) (range 1 9))

         rights4    (first (for [to rights3, :when (and (<= (rem to 9) 8) (enemy? board from to))] to))
         ups4       (first (for [to ups3,    :when (and (>= to 0)         (enemy? board from to))] to))
         lefts4     (first (for [to lefts3,  :when (and (<= (rem to 9) 8) (enemy? board from to))] to))
         downs4     (first (for [to downs3,  :when (and (>= to 0)         (enemy? board from to))] to))]

     (flatten rights2 ups2 lefts2 downs2 rights4 ups4 lefts4 downs4)))

(defn moves-for-horse [board from]
  (let [row (/ from 9)
        col  (rem from 9)

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

(defn moves-for-black-soldier [board from]
  (let [row      (/ from 9)
        col      (rem from 9)
        right    (if (= col 8) nil [(+ from 1)])
        left     (if (= col 0) nil [(- from 1)])
        piece    (nth board from)
        vertical (if (black? piece)
                     (if (= row 9) nil [(+ from 9)])
                     (if (= row 0) nil [(- from 9)]))
        tos      (concat right left vertical)]
    (filter #(different-side? board from %) tos)))

;-------------------------------------------------------------------------------

(defn move [board move]
  "Returns a new board. The move should be valid."
  (let [piece   (nth board (:from move))
        builder (StringBuilder. board)]
    (doto builder
      (.setCharAt (:from move) \u3000)
      (.setCharAt (:to   move) piece)
      (.toString))))

(defn generals-faced? [board move]
  (let [board2      (move board move)
        black-index (.indexOf board2 (int \將))
        red-index   (.indexOf board2 (int \帥))
        black-col   (rem black-index 9)
        red-col     (rem red-index   9)]
    (if-not (= black-col red-col)
            false
            (let [black-row (/ black-index 9)
                  red-row   (/ red-index   9)
                  rows      (range (+ black-row 1) red-row)
                  blocked   (any? #(let [index (+ (* black-row 9) black-col)
                                         piece (nth board index)]
                                     (not (= piece \u3000)))
                                  rows)]
              (not blocked)))))

(defn moves-for-piece [board from]
  "Returns all possible moves for a piece at the index"
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
                    \兵 moves-for-black-soldier
                    \卒 moves-for-red-soldier
                    \u3000 nil)
        tos   (if (nil? fun) nil (fun board from))]
    (filter
      #(let [move (Move. from %)] (not (generals-faced? board move)))
      tos)))

(defn moves [board]
  "Returns all possible moves from board"
  (flatten
    (map
      (fn [from]
        (let [tos (moves-for-piece board from)]
          (map (fn [to] (Move. from to)) tos)))
      (range 90))))
