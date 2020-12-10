(ns danielschleindlsperger.adventofcode2020.day-10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day10.txt")))

(defn- parse-input
  [input]
  (let [lines (str/split input #"\n")]
    (map #(Integer/parseInt %) lines)))

(defn- add-charging-outlet [xs] (conj xs 0))
(defn- add-device-adapter [xs] (conj xs (+ (first xs) 3)))

(defn- multiply-joltage-diffs [jolt-count] (* (get jolt-count 1) (get jolt-count 3)))

(defn solve-part-1 [input]
  (let [joltages (parse-input input)]
    (->> joltages
         ;; add the charging outlet with 0 jolts at the start of the seq
         (add-charging-outlet)
         ;; sort in descending order
         (sort #(compare %2 %1))
         ;; add the device adapter to the start (always 3 jolts higher than the highest adapter joltage)
         (add-device-adapter)
         ;; map to tuples with one joltage overlapping
         (partition 2 1)
         ;; subtract them from each other
         (map #(apply - %))
         ;; count the joltage differences
         (frequencies)
         ;; multiply the counts
         (multiply-joltage-diffs))))

#_(defn solve-part-2 [input preamble-len]
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
  (def example "16
10
15
5
1
11
7
19
6
12
4
")
  (solve-part-1 example) ;; 35 (example)
  (solve-part-1 puzzle-input) ;; 1998
  #_(solve-part-2 example 5) ;; 62 (example)
  #_(solve-part-2 puzzle-input 25)) ;; 13826915
