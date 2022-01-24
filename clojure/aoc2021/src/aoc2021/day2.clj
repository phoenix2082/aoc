(ns aoc2021.day2
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input [filename]
  (as-> (slurp filename) x
    (str/split-lines x)))

(defn move-marine [instr hp depth]
  (let [[dir unit] (str/split instr #" ")
        unit-val (Integer/parseInt unit)]
    (case dir
      "forward" [(+ hp unit-val) depth]
      "down" [hp (+ depth unit-val)]
      "up" [hp (- depth unit-val)])))
        
(defn navigation [lst]
  (loop [[x & r :as xs] lst
         [hp depth] [0 0]
         n 0]
    (if (nil? x)
      (* hp depth)
      (recur r (move-marine x hp depth) (inc n)))))

(defn part1-soln [filename]
  (-> filename
      read-input
      navigation))

(defn move-marine-aim [instr hp depth aim]
  (let [[dir unit] (str/split instr #" ")
        unit-val (Integer/parseInt unit)]
    (case dir
      "forward" [(+ hp unit-val) (+ depth (* aim unit-val)) aim]
      "down" [hp depth (+ aim unit-val)]
      "up" [hp depth (- aim unit-val)])))

(defn navigation-aim [lst]
  (loop [[x & r :as xs] lst
         [hp depth aim] [0 0 0]
         n 0]
    (if (nil? x)
      (* hp depth)
      (recur r (move-marine-aim x hp depth aim) (inc n)))))

(defn part2-soln [filename]
  (-> filename
      read-input
      navigation-aim))

