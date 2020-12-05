(ns danielschleindlsperger.adventofcode2020.day-04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def puzzle-input (slurp (io/resource "day04.txt")))

(defn- parse-passport [s]
  (let [fields (str/split s #"\s")]
    (reduce
     (fn [passport field]
       (let [[key val] (str/split field #":")]
         (assoc passport key val)))
     {}
     fields)))

(comment
  (parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"))

(defn- parse-input
  [input]
  (let [passports (str/split input #"\n\n")]
    (map parse-passport passports)))

(def ^:private req-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])
(defn- valid-simple? [pp] (every? #(get pp %) req-fields))

(defn- field-valid?
  [k f]
  (fn [m] (let [v (get m k)]
            ;; short circuit when value is not defined (field is required)
            (and (some? v) (f v)))))
(defn- between [min max] (fn [x] (<= min (bigint x) max)))
(defn- matches [re] (fn [x] (some? (re-matches re x))))
(defn- one-of [xs] (fn [x] (some? ((set xs) x))))
(defn- height-between
  [opts]
  (fn [x]
    (when-let [[_ v unit] (re-matches #"(\d+)(cm|in)" x)]
      (when-let [[min max] (get opts (keyword unit))]
        (<= min (bigint v) max)))))

(defn- valid? [pp]
  (let [f (every-pred (field-valid? "byr" (between 1920 2002))
                      (field-valid? "iyr" (between 2010 2020))
                      (field-valid? "eyr" (between 2020 2030))
                      (field-valid? "hgt" (height-between {:cm [150 193] :in [59 76]}))
                      (field-valid? "hcl" (matches #"#([0-9]|[a-f]){6}"))
                      (field-valid? "ecl" (one-of ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))
                      (field-valid? "pid" (matches #"^[0-9]{9}$")))]
    (f pp)))

(comment
  ((between 1 10) "0")
  ((matches #"#([0-9]|[a-f]){6}") "#123abc") ;; true
  ((matches #"#([0-9]|[a-f]){6}") "#123abz") ;; false
  ((one-of ["a" "b" "c"]) "b") ;; true
  ((one-of ["a" "b" "c"]) "d") ;; false
  (valid? {"byr" "1920"}) ;; true
  (valid? {"byr" "1919"}) ;; false
  )

(defn solve-part-1 [input]
  (let [passports (parse-input input)
        valid-passports (filter valid-simple? passports)]
    (count valid-passports)))

(defn solve-part-2 [input]
  (let [passports (parse-input input)
        valid-passports (filter valid? passports)]
    (count valid-passports)))

(comment
  (solve-part-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in") ;; 2 (example)

  (solve-part-1 puzzle-input) ;; 182
  (solve-part-2 puzzle-input)) ;; 109
