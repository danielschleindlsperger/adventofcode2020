(ns danielschleindlsperger.adventofcode2020.day-03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day03.txt")))

(defn- tree? [x]
  (= x \#))

(defn- parse-input
  "Parse the puzzle input into a lazy sequence of vectors (slope rows) with trees/open squares:
  ([x . .]
   [. x x])"
  [input]
  (let [lines (str/split input #"\n")]
    (map #(into [] (flatten (partition 1 %)))
         lines)))

(comment
  (parse-input "..##
#..."))

(defn- tree-at-pos?
  "Determine if there's a tree at the given 0-index position of the slope segment.
   If the given position is outside of the row, it will overflow and start again from the left side of the segment."
  [row x-pos]
  (let [x (mod x-pos (count row))]
    (tree? (get row x))))

(comment
  (tree-at-pos? [\. \. \#] 0) ;; false
  (tree-at-pos? [\. \. \#] 2) ;; true
  (tree-at-pos? [\. \. \#] 5) ;; true
  )

(defn- keep-every-nth
  "Keeps every 1st, 2nd, 3rd, .. item in the list and removes other elements.
   To keep every item, pass 1"
  [xs n]
  (keep-indexed (fn [i row] (when (= 0 (mod i n)) row))
                xs))

(comment
  (keep-every-nth [1 2 3 4 5 6] 1) ;;(1 2 3 4 5 6)
  (keep-every-nth [1 2 3 4 5 6] 2) ;;(1 3 5)
  (keep-every-nth [1 2 3 4 5 6] 3) ;;(1 4)
  )

(defn- toboggan
  "Slide down the slope in the toboggan."
  [slope {:keys [x-step y-step]}]
  (let [length-adjusted-slope (keep-every-nth slope y-step)]
    (reduce (fn [{:keys [tree-count x-pos]} row]
              {:tree-count (if (tree-at-pos? row x-pos) (inc tree-count) tree-count)
               :x-pos (+ x-pos x-step)})
            {:tree-count 0 :x-pos 0}
            length-adjusted-slope)))

(defn solve-part-1 [input]
  (let [slope (parse-input input)]
    (toboggan slope {:x-step 3 :y-step 1})))

(defn solve-part-2 [input]
  (let [slope (parse-input input)
        slope-steps [[1 1] [3 1] [5 1] [7 1] [1 2]]
        results (map (fn [[x y]] (toboggan slope {:x-step x :y-step y})) slope-steps)]
    (->> results
         (map :tree-count)
         (reduce *))))

(comment
  (solve-part-1 puzzle-input) ;; 200
  (solve-part-2 puzzle-input) ;; 37379232000
  )
