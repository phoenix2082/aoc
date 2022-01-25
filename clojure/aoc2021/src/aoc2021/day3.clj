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

(defn counter-lst [hmap]
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

(defn bin-list-to-decimal [lst]
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
    
      
