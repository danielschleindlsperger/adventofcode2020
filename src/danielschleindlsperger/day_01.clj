(ns danielschleindlsperger.adventofcode2020.day-01
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def puzzle-input (slurp (io/resource "day01.txt")))

(defn- parse-input [input]
  (map bigint (str/split input #"\n")))

(defn solve-part-1 [input]
  (let [xs (parse-input input)
        pairs (for [x xs
                    y xs]
                (when (= 2020 (+ x y))
                  (* x y)))]
    (first (filter some? pairs))))

(defn solve-part-2 [input]
  (let [xs (parse-input input)
        triples (for [x xs
                      y xs
                      z xs]
                  (when (= 2020 (+ x y z))
                    (* x y z)))]
    (first (filter some? triples))))

(deftest day01
  (def example "1721
979
366
299
675
1456")
  (is (= 514579 (solve-part-1 example)))
  (is (= 241861950 (solve-part-2 example))))

(comment
  (solve-part-1 puzzle-input)
  (solve-part-2 puzzle-input))
