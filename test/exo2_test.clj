(ns exo2-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as str]
            [clojure.tools.reader.impl.utils :refer [second']]))


(defn split-identifier [identifier]
  (let [middle (quot (.length identifier) 2)]
    {:firstPart  (subs identifier 0 middle)
     :secondPart (subs identifier middle)}))


(defn is-splittable [identifier]
  (= (rem (.length identifier) 2) 0))

(deftest test-split-identifier
  (testing "split number 12"
    (is (= {:firstPart "1" :secondPart "2"} (split-identifier "12"))))
  (testing "split number 123"
    (is (= {:firstPart "1" :secondPart "23"} (split-identifier "123")))))


(deftest test-is-splittable
  (testing "test on odd length number string 1 is not splittable"
    (is (= false (is-splittable "1"))))
  (testing "test on even length number string 12 is splittable"
    (is (= true (is-splittable "12")))))

(defn is-invalid-id [identifier]
  (if (is-splittable identifier)
    (let [split-id (split-identifier identifier)]
      (= (:firstPart split-id) (:secondPart split-id)))
    false))

(deftest test-is-invalid-id
  (testing "1 is a valid id"
    (is (= false (is-invalid-id "1"))))
  (testing "12 is a valid id"
    (is (= false (is-invalid-id "12"))))
  (testing "11 is an invalid id"
    (is (= true (is-invalid-id "11"))))
  (testing "1212 is an invalid id"
    (is (= true (is-invalid-id "1212"))))
  (testing "1213 is an invalid id"
    (is (= false (is-invalid-id "1213")))))

(defrecord identifier-range [start end])

(defn parse-identifier-range [string-range-id]
  (let [start-end (str/split string-range-id #"-")]
    (->identifier-range (BigInteger. (first start-end)) (BigInteger. (second start-end))))
  )

(defn make-identifiers [id-range]
  (map str (range (.start id-range) (inc (.end id-range)))))

(defn make-identifier-range [start end]
  (->identifier-range start end))

(defn keep-invalid-identifiers [ids]
  (map bigint (filter is-invalid-id ids)))

(defn parse-input-ranges-identifiers [input]
  (map parse-identifier-range (str/split input #",")))

(defn adding-up-invalid-identifiers [invalid-ids]
  (reduce + invalid-ids))


(defn decode-exo2-v1 [input-ranges-ids]
  (let [ranges-identifiers (map make-identifiers (parse-input-ranges-identifiers input-ranges-ids))
        invalid-identifiers (mapcat keep-invalid-identifiers ranges-identifiers)]
    (adding-up-invalid-identifiers invalid-identifiers)))

(deftest test-parse-identifier-range
  (testing "Parsing range identifier"
    (is (= (->identifier-range 11 22) (parse-identifier-range "11-22")))))


(deftest test-make-all-identifiers
  (testing "Make all identifier for range"
    (is (= ["11" "12" "13" "14"] (make-identifiers (make-identifier-range 11 14))))))

(deftest test-keep-only-invalid-identifiers
  (testing "keep only one invalid identifiers"
    (is (= [11] (keep-invalid-identifiers ["11" "12"]))))
  (testing "keep two invalid identifiers"
    (is (= [11 6464] (keep-invalid-identifiers ["11" "12" "6464"])))))

(deftest test-parse-input-ranges-identifiers
  (testing "parse the input to define all identifiers ranges"
    (is (= [(make-identifier-range 11 22)
            (make-identifier-range 95 115)
            (make-identifier-range 998 1012)]
           (parse-input-ranges-identifiers "11-22,95-115,998-1012"))))
  (testing "bug on overflow integer, so fix it with big integer"
    (is (= [(make-identifier-range 6328350434 6328506208)] (parse-input-ranges-identifiers "6328350434-6328506208")))))

(deftest test-adding-up-invalid-ids
  (testing "sum of invalid numbers 1 2 3"
    (is (= 6 (adding-up-invalid-identifiers [1 2 3])))))

(deftest test-decode-exo2-v1
  (testing "decode on an small example 11-22,95-115,998-1012"
    (is (= 1142 (decode-exo2-v1 "11-22,95-115,998-1012")))))

(deftest test-decode-full-sample-exo2-v1
  (testing "decode full sample gave in exercise 2"
    (is (= 1227775554 (decode-exo2-v1 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")))))

(deftest test-decode-my-input-puzzle-exo2-v1
  (testing "decode my puzzle exo2 v1"
    (is (= 28846518423 (decode-exo2-v1 "385350926-385403705,48047-60838,6328350434-6328506208,638913-698668,850292-870981,656-1074,742552-796850,4457-6851,138-206,4644076-4851885,3298025-3353031,8594410816-8594543341,396-498,1558-2274,888446-916096,12101205-12154422,2323146444-2323289192,37-57,101-137,46550018-46679958,79-96,317592-341913,495310-629360,33246-46690,14711-22848,1-17,2850-4167,3723700171-3723785996,190169-242137,272559-298768,275-365,7697-11193,61-78,75373-110112,425397-451337,9796507-9899607,991845-1013464,77531934-77616074")))))