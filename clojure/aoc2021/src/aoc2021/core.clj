(ns aoc2021.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input [filename]
  (as-> (slurp filename) x
    (str/split-lines x)
    (map #(Integer/parseInt %) x)))

(defn change? [x y]
  (cond
    (> y x) "Increased"
    (< y x) "Decreased"
    :else "Equal"))

(defn check-height [lst]
  (loop [[x & r :as xs] lst
         result []]
    (if (= 1 (count xs))
      result
      (recur r (conj result (change? x (first r)))))))

(defn check-height-window [lst]
  (loop [[w x y z & r :as xs] lst
         result []]
    (if (nil? z)
      result
      (let [sum1 (+ w x y)
            sum2 (+ x y z)]
        (recur (concat [x y z] r) (conj result (change? sum1 sum2)))))))

(defn increase-count [lst]
  (count (filter #(= % "Increased") lst)))

(defn part1-ans [filepath]
  (-> filepath
      read-input
      check-height
      increase-count))

(defn part2-ans [filepath]
  (-> filepath
      read-input
      check-height-window
      increase-count))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

