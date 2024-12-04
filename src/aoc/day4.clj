(ns aoc.day4
  (:require
   [clojure.repl]
   [clojure.pprint :as pp]
   [clojure.string :as s]
   [clojure.test :as t]
   [clojure.math :as math]
   ; [clojure.set :as st]
   ; [clojure.core.matrix :as m]
   ; [clojure.core.reducers :as r]
   ; [clojure.math.numeric-tower :as m]
   )
  (:use aoc.core))

(def day 4)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (println testfile)

; (defn parse-line [l]
;   (let [[_ xa xb] (re-matches #"(\d+)\s+(-?\d+)" l)
;           f #(Integer/parseInt %)
;           ]
;     [(f xa) (f xb)]))


(defn to-int [s] (Integer/parseInt s))

(defn prepare-input [str-input]
  (->> str-input
       (s/split-lines)
       (mapv vec)
       ))


(def t1 (prepare-input testfile))
(def input (prepare-input infile))
; (println t1)

; (t/are [i o] (= i o)
;        [1 1])

(defn to-coords [mtr]
  (reduce-kv
   (fn [acc rc row]
     (reduce-kv
       (fn [racc cc e]
        (assoc racc [rc cc] e)
        )
       acc
       row
      ))
   {}
   mtr
   ))

; (reduce-kv (fn [acc i r] (println i r) acc) {} t1)
; (pp/pprint ((to-coords t1) [9 9]))

(defn diag-coords-line [l]
    (mapv vector
          (range l -1 -1)
          (range 0 (inc l))))

(defn diag-coords [maxl]
  (mapv diag-coords-line (range 0 (inc maxl))))

(pp/pprint (diag-coords 3))

(defn pyth [rc cc]
  (int (math/ceil (math/sqrt (+ (* rc rc) (* cc cc))))))

; (int 0.3)
; (pyth 8 8 )

(defn to-diags [mtr]
  (let [rc (count mtr)
        cc (count (first mtr))
        diags (diag-coords (+ rc cc))
        lookup (to-coords mtr)]
      (mapv (fn [ps] (filter identity (map #(lookup %) ps))) diags)
    ))

(pp/pprint (to-diags t1))

(defn count-xmas [v]
  (+
  (count (re-seq #"XMAS" (s/join v)))
  (count (re-seq #"SAMX" (s/join v)))
   ))

(defn count-row [mtr]
  (sum
   (map count-xmas mtr)
   )
  )
; (count-row t1)

(defn transpose [m]
  (apply mapv vector m))

(defn count-all [mtr]
  (let [
        trans (transpose mtr)
        diag1 (to-diags mtr)
        diag2 (to-diags (mapv #(vec (reverse %)) mtr))
        ]
    (+
     (count-row mtr)
     (count-row trans)
     (count-row diag1)
     (count-row diag2)
     )
    ))

; (count-xmas "SAMXXMAS")
; (pp/pprint (to-diags (transpose t1)))
; (pp/pprint (to-diags (mapv #(vec (reverse %)) t1)))
; (pp/pprint (to-diags t1))

(defn part1 [input]
  (->> input
      (count-all)
       ))

; (part1 input)
; (println (time (part1 input)))

(defn third [v] (nth v 2))

(defn is-mas-x [mtr3]
  (let [
        center (second (second mtr3))
        tl (first (first mtr3))
        tr (third (first mtr3))
        bl (first (third mtr3))
        br (third (third mtr3))
        ]
     (and
       (= \A center)
       (case [tl br]
         [\M \S] true
         [\S \M] true
         false
       )
       (case [bl tr]
         [\M \S] true
         [\S \M] true
         false
       )
     )))

; (case [1 2]
;   [1 3] true
;   false)

; (is-mas-x
;  [
;   [\M \E \M]
;   [\M \A \M]
;   [\S \E \S]
;   ]
;  )

; (count (partition 3 1 (first t1)))

(defn partition3x3 [mtr]
  (let [rows3 (partition 3 1 (map #(partition 3 1 %) mtr))]
    (for [
          r (range 0 (- (count mtr) 2) )
          c (range 0 (- (count mtr) 2) )
          ]
    (let [row (nth rows3 r)]
      (mapv #(nth % c) row)
      )
    )))

; (range 0 (count t1))

; (pp/pprint (partition3x3 t1))

(defn part2 [input]
  (->> input
       (partition3x3)
       (filter is-mas-x)
       (count)
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
