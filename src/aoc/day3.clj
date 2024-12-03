(ns aoc.day3
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

(def day 3)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (println testfile)

; (defn parse-line [l]
;   (let [[_ xa xb] (re-matches #"(\d+)\s+(-?\d+)" l)
;           f #(Integer/parseInt %)
;           ]
;     [(f xa) (f xb)]))

(def testfile2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn to-int [s] (Integer/parseInt s))
(defn prepare-input [str-input]
  (->> str-input
       (re-seq #"((mul)\((\d+),(\d+)\))|((do)(n't)?\(\))")
       (map (fn [[_ _ ifmul a b _ ifdo ifnt]]
              (if ifmul
                [:mul (to-int a) (to-int b)]
                [(if ifnt :dont :do)]
                )
              ))
       ))

; (pp/pprint (re-seq #"((mul)\((\d+),(\d+)\))|(do\(\))" testfile ))
; (re-seq #"((do)(n't)?\(\))" "ado()")
(pp/pprint (prepare-input testfile2))

(def test-input (prepare-input testfile))
(def input (prepare-input infile))
(def t2 (prepare-input testfile2))
; (println test-input)

(def t1 test-input)

; (t/are [i o] (= i o)
;        [1 1])


(defn part1 [input]
  (->> input
      (filter (fn [[op _ _]] (= op :mul)))
      (map rest)
      (map #(apply * %))
      (sum)
       ))

; (part1 input)
; (println (time (part1 input)))

(defn drop-dont [inst]
  (reduce
   (fn [[on acc] i]
     (case (first i)
       :do [true acc]
       :dont [false acc]
       :mul (if on
              [on (conj acc i) ]
              [on acc]
              )))
   [true []]
   inst
   ))


(defn part2 [input]
  (->> input
      (drop-dont)
      (second)
      (map (fn [[_ a b]] (* a b)))
      (sum)
       ))

; (part2 input)
; (part2 t1)
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
