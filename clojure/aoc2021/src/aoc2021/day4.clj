(ns aoc2021.day4
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input [filename]
  (as-> (slurp filename) x
    (str/split-lines x)))

(defn convert-str-to-int-lst [str]
  (->> (str/split str #" ")
       (remove str/blank?)
       (map #(Integer/parseInt %))))

(defn draw-num [str]
  (map #(Integer/parseInt %) (str/split str  #",")))

(defn create-board
  "Create a 5x5 Bingo board from list of string."
  [lst]
  (loop [xs lst
         result []]
    (if (empty? xs)
      result
      (recur (drop 5 xs) ;; remove first 5 element
             (conj result (map #(convert-str-to-int-lst %) (take 5 xs)))))))
             

(defn check-board
  "If any number in board `b` matches with number `num`
  then set it to zero. Return the updated board.
  "
  [b num]
  (map (fn [itm]
         (map (fn [xtm]
                (if (= num xtm) 0 xtm)) itm))
       b))

(defn board-matched?
  "A board will be winner if sum of values of any row or column is zero.
  See: check-board.
  
  Returns true if sum of values of any one row or column is zero, else false."
  [board]
  (let [rows-sum (map #(apply + %) board)
        rotate-board (apply map vector board)
        cols-sum (map #(apply + %) rotate-board)]
    (or (some #(= 0 %) rows-sum)
        (some #(= 0 %) cols-sum))))
   
(defn process-board [boards num]
  (loop [[b & r :as xs] boards
         result []]
    (let [updated-board (check-board b num)]
      (cond
        ;; matching board found; done.
        (board-matched? updated-board) (list true num  updated-board result)
        ;;iteration done; board not found. return updated board list.
        (empty? b) result 
        :else (recur r (conj result updated-board))))))

(defn process-draw [boards nums]
  (loop [[n & r :as xs] nums
         result boards]
    (if (or (boolean? (first result)) (empty? r))
      result
      (recur r (process-board result n)))))   

(defn check-and-update
  "Check if board has matching value in any index.
  If yes, set to zero.

  Returns: list of
  true > if updated
  updated board."
  [b num]
  (let [ub (check-board b num)]
    (if (board-matched? ub)
      ;; if sum of values of any row/column is zero, add winner
      (list true ub)
      (list false ub))))
         
(defn process-winning-board
  "Check if any board contains `num`.
  If yes update value at that index to 'zero'.
  Also, if any board has sum of values of any row or column to zero
  add it to winning board list.
  "
  [boards num winners]
  (loop [[b & r :as xs] boards
         result []
         winn winners
         ]
    (if (empty? b) (list result winn)
        (let [[updated? ub] (check-and-update b num)]
          (cond
            (empty? b) (list result winn) ;; no board left. quit.
            ;; add winning board to winner list, continue with remaining
            updated? (recur r                          ;; remaining board
                            result                     ;; don't add updated board
                            (conj winn (list num ub))) ;; updated winning board list
            ;; board updated, but no match yet, update and continue with remaining
            :else (recur r                 ;; remaining board
                         (conj result ub)  ;; add updated board to boards
                         winn              ;; winning boards
                         ))))))

(defn process-last-draw
  "Find all boards which has atleast one matching row or column
  with last matching number.

  Will return list of:
  ((95 ((55 15 85 39 4)
       (0 0 0 0 0)
       (0 47 61 9 66)
       (82 32 0 0 16)
       (50 96 14 60 35)))
  86.. ((..)))
  "
  [boards nums]
  (loop [[n & r :as xs] nums
         result boards
         wb []]
    (if (or (empty? r) (empty? result))
      wb
      (let [[x y] (process-winning-board result n wb)]
        (recur r x y)))))


(defn ans-part-2 [filepath]
  (if (not (str/blank? filepath))
    (let [inp (read-input filepath)
          boards (create-board (rest (remove str/blank? inp)))
          nums (draw-num (first inp))
          result (process-last-draw boards nums)
          lst-el (last result)]
      (* (first lst-el)
         (apply + (map #(apply + %) (second lst-el)))))
    (str filepath " is blank.")))
    
    
    
