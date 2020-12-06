(ns danielschleindlsperger.adventofcode2020.day-06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day06.txt")))

(defn- parse-input
  "Parses the groups' answers to a nested sequence with the hierarchy group > person > persons answers (as a set)"
  [input]
  (let [groups (str/split input #"\n\n")]
    (map (fn [group]
           (str/split group #"\n")) groups)))

(defn- unique-answers [group]
  (->> group
       (mapcat set)
       set))

(comment
  (unique-answers ["abc"]) ;; #{\a \b \c}
  (unique-answers ["a" "b" "c"]) ;; #{\a \b \c}
  (unique-answers ["ab" "ac"]) ;; #{\a \b \c}
  (unique-answers ["a" "a" "a" "a"]) ;; #{\a}
  )

(defn- same-answers [group]
  (->> group
       (map set)
       (apply set/intersection)))

(comment
  (same-answers ["abc"]) ;; #{\a \b \c}
  (same-answers ["a" "b" "c"]) ;; #{}
  )

(defn solve-part-1 [input]
  (let [groups (parse-input input)]
    (->> groups
         (map (comp count unique-answers))
         (reduce +))))

(defn solve-part-2 [input]
  (let [groups (parse-input input)]
    (->> groups
         (map (comp count same-answers))
         (reduce +))))

(comment
  (def example "abc

a
b
c

ab
ac

a
a
a
a

b")
  (solve-part-1 example) ;; 11 (example)
  (solve-part-1 puzzle-input) ;; 6549
  (solve-part-2 example) ;; 6 (example)
  (solve-part-2 puzzle-input)) ;; 3466
