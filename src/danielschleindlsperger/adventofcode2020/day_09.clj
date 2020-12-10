(ns danielschleindlsperger.adventofcode2020.day-09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day09.txt")))

(defn- parse-input
  [input]
  (let [lines (str/split input #"\n")]
    (map bigint lines)))

;; This is actually not correct since it contains number added to themselves
(defn- all-sums [xs]
  (set (for [x xs
             y xs]
         (+ x y))))

(defn- valid? [nums preamble-len]
  (let [[preamble [cur-seg]] (split-at preamble-len (take (inc preamble-len) nums))
        sums (all-sums preamble)]
    (contains? sums cur-seg)))

(defn solve-part-1 [input preamble-len]
  (let [nums (parse-input input)
        first-invalid (->> (range 0 (count nums))
                           (map #(drop % nums))
                           (filter #(not (valid? % preamble-len)))
                           first)]
    (nth first-invalid preamble-len)))

(defn- sub-vec-with-matching-sum [nums target-sum]
  (reduce (fn [init x]
            (let [sum (apply + (conj init x))]
              (cond (= sum target-sum) (reduced (conj init x))
                    (< target-sum sum) (reduced nil)
                    :else (conj init x))))
          []
          nums))

(defn- sum-first-last [xs] (+ (first xs) (last xs)))

(defn solve-part-2 [input preamble-len]
  (let [nums (parse-input input)
        first-invalid (solve-part-1 input preamble-len)]
    (->> (range 0 (count nums))
         (map #(drop % nums))
         (map #(sub-vec-with-matching-sum % first-invalid))
         (filter some?)
         (first)
         (sort)
         (sum-first-last))))

(comment
  (def example "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")
  (solve-part-1 example 5) ;; 127 (example)
  (solve-part-1 puzzle-input 25) ;; 105950735
  (solve-part-2 example 5) ;; 62 (example)
  (solve-part-2 puzzle-input 25)) ;; 13826915
