(ns danielschleindlsperger.adventofcode2020.day-08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day08.txt")))

(defn- parse-input
  [input]
  (let [lines (str/split input #"\n")]
    (into [] (map (fn [l]
                    (let [[_ cmd offset] (re-matches #"(acc|jmp|nop) (.*)" l)]
                      {:cmd cmd :offset (Integer/parseInt offset)})) lines))))

(def ^:private init-state {:line 0 :acc 0})

(defn- nop [state _offset] (update state :line inc))

(defn- acc [state offset] (-> state
                              (update :line inc)
                              (update :acc + offset)))

(defn- jmp [state offset] (update state :line + offset))

(def ^:private instructions {"nop" nop
                             "acc" acc
                             "jmp" jmp})

(defn- exec-stmt
  [state register]
  (let [{:keys [cmd offset]} (->> state :line (get register))
        instruction (get instructions cmd)]
    (instruction state offset)))

(defn- exec-register [register]
  (loop [state init-state
         history #{}]
    (let [{:keys [line] :as new-state} (exec-stmt state register)]
      ;; if the line resulting in executing the statement was already seen, return the state that was valid beforehand
      (if (contains? history line)
        state
        (recur new-state (conj history line))))))

(defn solve-part-1 [input]
  (let [register (parse-input input)
        last-stmt (exec-register register)]
    (:acc last-stmt)))

(defn solve-part-2 [input]
  (let [register (parse-input input)]))

(comment
  (def example "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
  (solve-part-1 example) ;; 5 (example)
  (solve-part-1 puzzle-input) ;; 2014
  (solve-part-2 example) ;; 32 (example)
  (solve-part-2 puzzle-input)) ;; 2976
