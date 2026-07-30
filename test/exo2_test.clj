(ns exo2-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as str]))


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


;; TODO: parsing & generate all ids