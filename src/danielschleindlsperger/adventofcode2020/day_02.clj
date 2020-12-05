(ns danielschleindlsperger.adventofcode2020.day-02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def puzzle-input (slurp (io/resource "day02.txt")))

(defn- parse-input [input]
  (let [lines (str/split input #"\n")]
    (map #(let [[policy pwd] (str/split % #": ")]
            {:policy policy
             :pwd pwd}) lines)))

(defn- parse-policy [policy]
  (let [[_ a b character] (re-matches #"(\d+)-(\d+)\s([a-z])" policy)
        a (bigint a)
        b (bigint b)]
    [a b character]))

(defn- matches-policy-1? [{:keys [policy pwd]}]
  (let [[min-count max-count character] (parse-policy policy)
        actual-count (count (re-seq (re-pattern character) pwd))]
    (<= min-count actual-count max-count)))

(comment
  (matches-policy-1? {:policy "5-6 x" :pwd "xxxxvxx"}))

(defn- at-pos
  "Finds character at 1-indexed position in string s and returns it, coerced to a string."
  [s pos]
  (when-let [x (get s (dec pos))]
    (str x)))

(defn- matches-policy-2? [{:keys [policy pwd]}]
  (let [[first-pos second-pos character] (parse-policy policy)
        matches-first (= character (at-pos pwd first-pos))
        matches-second (= character (at-pos pwd second-pos))
        matches [matches-first matches-second]]
    (or (= [true false] matches)
        (= [false true] matches))))

(comment
  (matches-policy-2? {:policy "5-6 x" :pwd "xxxxvxx"}) ;; false
  (matches-policy-2? {:policy "1-3 a" :pwd "abcde"}) ;; true
  (matches-policy-2? {:policy "1-3 b" :pwd "cdefg"})  ;; false
  )

(defn solve-part-1 [input]
  (->> input
       (parse-input)
       (filter matches-policy-1?)
       (count)))

(defn solve-part-2 [input]
  (->> input
       (parse-input)
       (filter matches-policy-2?)
       (count)))

(comment
  (solve-part-1 puzzle-input)
  (solve-part-2 puzzle-input))
