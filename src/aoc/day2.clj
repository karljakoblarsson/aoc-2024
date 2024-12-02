(ns aoc.day2
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

(def day 2)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (println testfile)

; (defn parse-line [l]
;   (let [[_ xa xb] (re-matches #"(\d+)\s+(-?\d+)" l)
;           f #(Integer/parseInt %)
;           ]
;     [(f xa) (f xb)]))

(defn prepare-input [str-input]
  (map #(read-string (str "[" % "]")) (s/split-lines str-input)))

; (def a (first (s/split-lines testfile))) 
; (read (str "[" a "]"))
; (str "[" a "]")
; (read-string "[1 2 3]")
; (str "wr" "234")

(def test-input (prepare-input testfile))
(def input (prepare-input infile))
; (println test-input)

(def t1 test-input)

; (t/are [i o] (= i o)
;        [1 1])


(defn is-same [l]
  (or
   (every? pos? l)
   (every? neg? l)
   ))

(defn is-less-three [l]
  (every? #(and (<= (abs %) 3) (>= (abs %) 1))  l))

(defn row-valid [r]
  (and
   (is-same r)
   (is-less-three r)
   ))

(defn is-row-valid [l]
  (let [diffs (map - l (rest l))]
    (row-valid diffs)
    ))

; (map is-row-valid t1)

(defn part1 [input]
  (->> input
      (map is-row-valid)
      (filter identity)
      (count)))

; (part1 input)
; (println (time (part1 input)))


(defn rm-one [l n]
  (concat
    (take (dec n) l)
    (drop n l)
   ))

; (rm-one [1 2 3 4] 0)
; (range 1 (count [1 2 3 4]))

(defn permutations [l]
  (map #(rm-one l %) (range 0 (inc (count l)))))

; (permutations [1 2 3 4])

(defn is-2-row-valid [r]
  (some identity (map is-row-valid (permutations r)))
  )

; (filter identity (map is-2-row-valid t1))

(defn part2 [input]
  (->> input
      (map is-2-row-valid)
      (filter identity)
      (count)))

; (println (last (permutations [1 2 3 4 5 6 7 8 9 10])))

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
