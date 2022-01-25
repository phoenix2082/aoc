(ns aoc2021.day3
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input [filename]
  (as-> (slurp filename) x
    (str/split-lines x)))

(defn convert-string-to-bin [v]
  (as-> (str/split v #"") x
    (map #(Integer/parseInt %) x)))

(defn convert-lst-to-bin [lst]
  (map #(convert-string-to-bin %) lst))

(defn counter-lst
  " Takes a map of bin value and list and returns a map with count.
  Example
  Input: {[0 [0 0 0 0]], [1, [1,1,1]]}
  Output {:0 4, :1 3}
  "
  [hmap]
  (let [[f1 s1] (first hmap)
        [f2 s2] (second hmap)]
    (-> (assoc {} (keyword (str f1)) (count s1))
        (assoc (keyword (str f2)) (count s2)))))

(defn count-one-and-zeroes [lst]      
  (as-> (apply map vector lst) ilist
    (map #(group-by identity %) ilist)
    (map (fn [k] (counter-lst k)) ilist)))

(defn gamma-rate [bin-map]
  (map (fn [kv]
         (if (> (:0 kv) (:1 kv))
           0
           1)) bin-map))

(defn epsilon-rate [bin-map]
  (map (fn [kv]
         (if (< (:0 kv) (:1 kv))
           0
           1)) bin-map))

(defn bin-list-to-decimal
  "Reverses the list of binary digits and
  convert it to decimal value.

  Ex. For binary number (1 1 1 1 1 1 1 1 1 0 0 1)

  > (bin-list-to-decimal '(1 1 1 1 1 1 1 1 1 0 0 1))
    4089
  "
  [lst]
  (->> (map-indexed (fn [idx itm] (* (Math/pow 2 idx) itm))
                    (reverse lst))
       (apply +)))

(defn part1-ans [filename]
  (let [bin-lst (-> filename
                    read-input
                    convert-lst-to-bin
                    count-one-and-zeroes)
        grate (-> (gamma-rate bin-lst)
                  (bin-list-to-decimal))
        erate (-> (epsilon-rate bin-lst)
                  (bin-list-to-decimal))]
    (* grate erate)))

;;;; Part 2 solution.
(defn filter-by-idx-binval
  "Select only those values for which element
  at index `idx` has `binval` in list `lst`.

  Example:
  (filter-by-idx-binval 0 1 [(0 1 1 0) (1 0 0 1) (1 1 0 1)])
   Returns - Only those list which has at index 0, binary 1
  [(1 0 0 1)]
  "
  [idx binval lst]
  (filter #(= (nth % idx) binval) lst))

(defn most-common-value [kv]
  (cond
    (> (:1 kv) (:0 kv)) 1
    (> (:0 kv) (:1 kv)) 0
    :else 1))

(defn least-common-value [kv]
  (cond
    (< (:1 kv) (:0 kv)) 1
    (< (:0 kv) (:1 kv)) 0
    :else 0))

(defn filter-list [lst idx rating-fn]
  (as-> (apply map vector lst) ilist
    (group-by identity (nth ilist idx))
    (counter-lst ilist)
    (rating-fn ilist)
    (filter-by-idx-binval idx ilist lst)))

(defn find-rating
  "Find rating recursively from list of ratings,
  filtering entries by rating function.

  Ex:
  To Filter by most common value in bit position.
  (find-rating lst most-common-value)

  Or, To Filter by least common value in current bit position.

  (find-rating lst least-common-value)

  where lst is list of `list of binary` value e.x.
  
  [(1 0 1 0 0 1) (0 1 0 1 1 0) (1 0 1 0 10).....]

  IMP: least-common-value is function.

  Returns: List of binary value
  ((0 1 0 1 1 0))
  
  "
  [lst rating-fn]
  (loop [n 0
         result lst]
    (if (= 1 (count result))
      result
      (recur (inc n) (filter-list result n rating-fn)))))

(defn calculate-rating
  "Calculate decimal rating by filtering list,
  and using rating function."
  [lst rating-fn]
  (-> (find-rating lst rating-fn)
      (first)
      (bin-list-to-decimal)))

(defn day3-part2-ans [filepath]
  (let [inp (-> (read-input filepath)
                (convert-lst-to-bin))
        ox-rating (calculate-rating inp most-common-value)
        co2-rating (calculate-rating inp least-common-value)]
    (* ox-rating co2-rating)))
      
