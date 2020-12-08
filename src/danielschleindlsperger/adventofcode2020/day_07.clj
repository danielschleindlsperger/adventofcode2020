(ns danielschleindlsperger.adventofcode2020.day-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day07.txt")))

(defn- parse-inner-bags [s]
  (when-not (= "no other bags" s)
    (into {} (map (fn [inner]
                    (let [[_ count color] (re-matches #"(\d+) (\w+\s\w+) bags?" inner)]
                      [color (Integer/parseInt count)]))
                  (str/split s #", ")))))

(comment
  (parse-inner-bags "no other bags") ;; nil
  (parse-inner-bags "1 bright white bag, 2 muted yellow bags"))

(defn- parse-input
  [input]
  (let [rules (str/split input #"\n")]
    (into {} (map (fn [rule]
                    (let [[_ color nested-bags] (re-matches #"(\w+\s\w+) bags contain (.*)." rule)]
                      [color (parse-inner-bags nested-bags)]))
                  rules))))

(defn- contains-color-eventually
  [needle-color parent-color bags]
  (when-let [children (get bags parent-color)]
    (or (contains? children needle-color)
        (some #(contains-color-eventually needle-color % bags)
              (keys children)))))

(defn solve-part-1 [input]
  (let [bags (parse-input input)
        needle "shiny gold"
        colors (keys bags)]
    (count (filter #(contains-color-eventually needle % bags) colors))))

(defn- count-bags [bags color]
  (let [children (get bags color)]
    (reduce + (map (fn [[col cnt]]
                     (+ cnt (* cnt (count-bags bags col))))
                   children))))

(defn solve-part-2 [input]
  (let [bags (parse-input input)]
    (count-bags bags "shiny gold")))

(comment
  (def example "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")
  (solve-part-1 example) ;; 4 (example)
  (solve-part-1 puzzle-input) ;; 246
  (solve-part-2 example) ;; 32 (example)
  (solve-part-2 puzzle-input)) ;; 2976
