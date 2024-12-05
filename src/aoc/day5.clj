(ns aoc.day5
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

(def day 5)
(def infile (slurp (mk-input-filename day)))
(def testfile (slurp (mk-test-input-filename day)))

; (println testfile)

; (defn parse-line [l]
;   (let [[_ xa xb] (re-matches #"(\d+)\s+(-?\d+)" l)
;           f #(Integer/parseInt %)
;           ]
;     [(f xa) (f xb)]))

(defn parse-rule [l]
  (let [[_ xa xb] (re-matches #"(\d+)\|(\d+)" l)
          f #(Integer/parseInt %)
          ]
    [(f xa) (f xb)]))


(defn to-int [s] (Integer/parseInt s))
(defn read-vec [s] (read-string (str "[" s "]")))

(defn prepare-input [str-input]
  (let [
        [rules prints] (s/split str-input #"\n\n")
        ]
  { :rules (map parse-rule (s/split-lines rules))
    :prints (map read-vec (s/split-lines prints))
   }
    )
  )


(def t1 (prepare-input testfile))
(def input (prepare-input infile))
; (pp/pprint t1)

(defn check-rule [[a b] prints]
  (let [
        ai (.indexOf prints a)
        bi (.indexOf prints b)
        ]
    (or
     (= ai -1)
     (= bi -1)
     (< ai bi)
     )))

; (.indexOf [ 2 4 1] 9)


(defn check-print [rules row]
  (every? #(check-rule % row) rules))

; (check-rule (first (:rules t1)) (first (:prints t1)) )
; (check-print (:rules t1) (first (:prints t1)))

(defn get-middle [v]
  (nth v (int (math/floor (/ (count v) 2)))))

; (get-middle [1 2 3  ])

(defn part1 [{ :keys [rules prints] }]
  (->> prints
      (filter #(check-print rules %))
      (map get-middle)
      (sum)
       ))

; (part1 input)
; (println (time (part1 input)))


(defn rules-dict [input]
  (reduce
   (fn [acc [a b]]
     (if (acc a)
       (update acc a (fn [v] (conj v b)))
       (assoc acc a [b])
       )
     )
   {}
   (:rules input)))

; (pp/pprint (rules-dict t1))

(defn fix [rules-dict row]
  (sort
   (fn [b a]
     (let [av (rules-dict a)
           bv (rules-dict b)
           sort-val
       (cond
        (some #(= b %) av) 1
        (some #(= a %) bv) -1
        :defult 1)
           ]
       ; (println a b av bv sort-val)
       sort-val
       ))
   row
   ))

(some #(= 13 %) nil)
(some #(= 13 %) [13])

(fix (rules-dict t1) r1)

(def r1 (nth (:prints t1) 5))
; (print r1)
; (fix (rules-dict t1) r1)

(defn part2 [{ :keys [rules prints] }]
  (->> prints
      (filter #(not (check-print rules %)))
      (map #(fix (rules-dict { :rules rules }) %))
      (map get-middle)
      (sum)
       ))

(part2 input)
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
