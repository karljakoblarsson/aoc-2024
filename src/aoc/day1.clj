(ns aoc.day1
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 1)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (println testfile)

(defn parse-line [l]
  (let [[_ xa xb] (re-matches #"(\d+)\s+(-?\d+)" l)
          f #(Integer/parseInt %)
          ]
    [(f xa) (f xb)]))

(defn prepare-input [str-input]
  (map parse-line (s/split-lines str-input)))


(def test-input (prepare-input testfile))
(def input (prepare-input infile))

(def t1 test-input)

; (t/are [i o] (= i o)
;        [1 1])

(defn transpose [m]
  (apply mapv vector m))

(defn dist [[a b]] (abs (- a b)))

(defn sort-pairs [ps]
  (let [[l1 l2] (transpose ps)]
    (transpose [(sort l1) (sort l2)])
    ))

(println (sort-pairs t1))
(map dist (sort-pairs t1))

(defn part1 [input]
  (->> input
      (sort-pairs)
      (map dist)
      (sum)))

(part1 input)
; (println (time (part1 input)))



(defn part2 [input]
  (let [[l1 l2] (transpose input)
        f2 (frequencies l2)
        score (fn [n] (* n (get f2 n 0)))]
    (sum (map score l1))
    ))

(part2 input)

(get (frequencies [1 1 1 2 4 5 5]) 19 0)

(part2 t1)
; (prn (time (part2 input)))
; (part2 input)

(defn solve-problem [infile]
  (let [input-string (slurp infile)
        input (prepare-input input-string)]
    (println "Part 1:")
    (println (part1 input))
    (println "")
    (println "Part 2:")
    (println (part2 input))))

(solve-problem (mk-input-filename day))
