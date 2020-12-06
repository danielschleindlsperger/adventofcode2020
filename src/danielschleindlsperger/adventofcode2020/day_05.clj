(ns danielschleindlsperger.adventofcode2020.day-05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day05.txt")))

(defn- parse-input
  [input]
  (let [boarding-passes (str/split input #"\n")]
    boarding-passes))


(defn- left-or-right
  "Return either the left or side half of the collection. Specified with :left or :right"
  [side coll]
  (let [middle (/ (count coll) 2)
        halves (split-at middle coll)]
    (if (= :left side)
      (nth halves 0)
      (nth halves 1))))

(defn- pass->seat-pos [pass]
  (let [rows (take 7 pass)
        row (first (reduce (fn [rem r] (left-or-right (if (= \F r) :left :right) rem))
                           (range 128)
                           rows))
        cols (subs pass 7)
        col (first (reduce (fn [rem r] (left-or-right (if (= \L r) :left :right) rem))
                           (range 8)
                           cols))]
    {:row row :col col}))

(comment
  (pass->seat-pos "FBFBBFFRLR")
  (pass->seat-pos "BFFFBBFRRR") ;; {:row 70 :col 7}
  (pass->seat-pos "FFFBBBFRRR") ;; {:row 14 :col 7}
  (pass->seat-pos "BBFFBBFRLL") ;; {:row 102 :col 4}
  )

(defn- seat-pos->seat-id [{:keys [row col]}]
  (-> row (* 8) (+ col)))

(comment
  (seat-pos->seat-id "BFFFBBFRRR") ;; 567
  (seat-pos->seat-id "FFFBBBFRRR") ;; 119
  (seat-pos->seat-id "BBFFBBFRLL") ;; 820
  )

(defn- boarding-passes->seat-ids [passes]
  (map (comp seat-pos->seat-id pass->seat-pos) passes))

(defn- all-seat-ids []
  (for [row (range 128)
        col (range 8)]
    (seat-pos->seat-id {:row row :col col})))

(defn solve-part-1 [input]
  (let [boarding-passes (parse-input input)
        seat-ids (boarding-passes->seat-ids boarding-passes)]
    (first (sort #(compare %2 %1) seat-ids))))

(defn solve-part-2 [input]
  (let [boarding-passes (parse-input input)
        seat-ids (set (boarding-passes->seat-ids boarding-passes))
        candidates (filter #(and (not (seat-ids %))
                                 (seat-ids (dec %))
                                 (seat-ids (inc %)))
                           (all-seat-ids))]
    (first candidates)))

(comment
  (solve-part-1 puzzle-input) ;; 866
  (solve-part-2 puzzle-input)) ;; 583
