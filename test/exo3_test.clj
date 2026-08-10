(ns exo3-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            ))

(defrecord bank-jotage [first second])

(defn create-bank-jotage [first second]
  (->bank-jotage first second))

(defn create-tuples-indexed [bank]
  (map-indexed (fn [index jotage-value] [index (Integer/parseInt (str jotage-value))])
               bank))

(defn convert-to-integers [bank]
  (map #(Integer/parseInt (str %)) bank))



(defn find-out-bank-voltage [bank]
  (let [integers-bank (convert-to-integers bank)
        bank-without-last (drop-last integers-bank)
        first-max (apply max bank-without-last)
        index-first-max (first (keep-indexed (fn [idx value] (when (= value first-max) idx)) bank-without-last))
        second-max (apply max (drop (inc index-first-max) integers-bank))]
    (create-bank-jotage first-max second-max)))

(defn convert-bank-jotable-to-number [bank]
  (Integer/parseInt (str (.first bank) (.second bank))))


(deftest get-max-jotable-for-one-bank
  (testing "on simple bank 151"
    (is (= (create-bank-jotage 5 1) (find-out-bank-voltage "151"))))
  (testing "for bank 811111111111119"
    (is (= (create-bank-jotage 8 9) (find-out-bank-voltage "811111111111119"))))
  (testing "for bank 234234234234278"
    (is (= (create-bank-jotage 7 8) (find-out-bank-voltage "234234234234278"))))
  (testing "bug for bank 195981 must have to return the first max value 9"
    (is (= (create-bank-jotage 9 9) (find-out-bank-voltage "195981"))))
  (testing "for bank bug"
    (is (= (create-bank-jotage 5 4) (find-out-bank-voltage "2232212212212222211221231124224222213132222133122224222123222112324122222122221322222225222342243112"))))
  )

(defn read-input-file
  [filename]
  (str/split-lines (slurp filename)))

(deftest test-convert-bank-jotable-to-number
  (testing "convert one bank-jotable"
    (is (= 34 (convert-bank-jotable-to-number (create-bank-jotage 3 4))))))


(defn sum-banks-jotable-v1 [banks]
  (reduce + (map convert-bank-jotable-to-number (map find-out-bank-voltage banks))))

(deftest test-sum-banks-jotable-v1
  (testing "sum example given"
    (is (= 357 (sum-banks-jotable-v1 ["987654321111111" "811111111111119" "234234234234278" "818181911112111"]))))
  (testing "sum from file with v1"
    (is (= 357 (sum-banks-jotable-v1 (read-input-file "test/exo3-sample.txt"))))))


(deftest test-exo3-decode-part-1
  (testing "decode real data part 1"
    (is (= 17095 (sum-banks-jotable-v1 (read-input-file "test/exo3-real-data.txt")))))
  (testing "count "
    (is (= 200 (count (read-input-file "test/exo3-real-data.txt"))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V2 algorithme for the PART 2
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord tmp-result [numbers choice-digits number-to-pickup])

(defn index-of-value [digits searched-value]
  (first (keep-indexed (fn [idx value] (when (= value searched-value) idx)) digits)))

(defn create-tmp-result [current-tmp-result max-digit-to-keep]
  (let [choice-digits (.-choice-digits current-tmp-result)
        index-max-digit (index-of-value choice-digits max-digit-to-keep)]
    (->tmp-result (conj (.-numbers current-tmp-result) max-digit-to-keep)
                  (drop (inc index-max-digit) choice-digits)
                  (dec (.-number-to-pickup current-tmp-result)))))


(defn compute-intermediate-result [current-tmp-result]
  (let [choice-digits (.-choice-digits current-tmp-result)
        number-to-take (- (count choice-digits) (dec (.-number-to-pickup current-tmp-result)))
        potential-digits (take number-to-take choice-digits)
        max-digit (apply max potential-digits)]
    (create-tmp-result current-tmp-result max-digit)))



(defn compute-all-steps-v2 [start-tmp-result]
  (loop [current-tmp-result start-tmp-result
         number-to-pickup (.-number-to-pickup current-tmp-result)]
    (if (= 0 number-to-pickup)
      current-tmp-result
      (recur (compute-intermediate-result current-tmp-result) (dec number-to-pickup)))))

(defn compute-bank-joltage-v2 [bank number-of-digits-for-output]
  (let [digits (convert-to-integers bank)
        joltage-computed (compute-all-steps-v2 (->tmp-result [] digits number-of-digits-for-output))]
    (str/join (.numbers joltage-computed))))

(defn compute-intermediate-result-v3 [current-tmp-result number-of-digits]
  (let [choice-digits (.-choice-digits current-tmp-result)
        count-digits-has-to-complete (- number-of-digits (count (.-numbers current-tmp-result)))
        number-to-take (- (count choice-digits) (dec count-digits-has-to-complete))
        potential-digits (take number-to-take choice-digits)
        max-digit (apply max potential-digits)]
    (create-tmp-result current-tmp-result max-digit)))


(defn compute-all-steps-v3 [start-tmp-result number-of-digits]
  (loop [current-tmp-result start-tmp-result
         current-number-of-digits (count (.-numbers start-tmp-result))]
    (if (= number-of-digits current-number-of-digits)
      current-tmp-result
      (let [new-result (compute-intermediate-result-v3 current-tmp-result number-of-digits)
            new-count-result (count (.numbers new-result))]
        (recur new-result new-count-result)))))

(defn compute-bank-joltage-v3 [bank number-of-digits-for-output]
  (let [digits (convert-to-integers bank)
        joltage-computed (compute-all-steps-v3 (->tmp-result [] digits number-of-digits-for-output) number-of-digits-for-output)]
    (str/join (.numbers joltage-computed))))

(deftest test-compute-intermediate
  (testing "test step intermediate 1"
    (is (= (->tmp-result [9] [6 4 1] 3) (compute-intermediate-result (->tmp-result [] [2 9 6 4 1] 4)))))
  (testing "test step intermediate 2"
    (is (= (->tmp-result [9 6] [4 1] 2) (compute-intermediate-result (->tmp-result [9] [6 4 1] 3)))))
  (testing "test step intermediate 3"
    (is (= (->tmp-result [9] [2 6 4 1] 3) (compute-intermediate-result (->tmp-result [] [9 2 6 4 1] 4))))))

(deftest test-compute-all-steps-joltage-v2
  (testing "test 1 all steps joltage v2"
    (is (= (->tmp-result [9 6 4 1] [] 0) (compute-all-steps-v2 (->tmp-result [] [2 9 6 4 1] 4))))))


(deftest test-compute-bank-joltage-v2
  (testing "test compute bank joltage v2 for 29641 with 4 digits"
    (is (= "9641" (compute-bank-joltage-v2 "29641" 4))))
  (testing "test compute bank joltage v2 for 987654321111111 with 12 digits"
    (is (= "987654321111" (compute-bank-joltage-v2 "987654321111111" 12))))
  (testing "test compute bank joltage v2 for 811111111111119 with 12 digits"
    (is (= "811111111119" (compute-bank-joltage-v2 "811111111111119" 12))))
  (testing "test compute bank joltage v2 for 234234234234278 with 12 digits"
    (is (= "434234234278" (compute-bank-joltage-v2 "234234234234278" 12))))
  (testing "test compute bank joltage v2 for 818181911112111 with 12 digits"
    (is (= "888911112111" (compute-bank-joltage-v2 "818181911112111" 12)))))

(defn sum-banks-jotable-v2 [banks]
  (reduce + (map biginteger (map (fn [bank]
                                   (compute-bank-joltage-v2 bank 12)) banks))))

(defn sum-banks-jotable-v3 [banks number-of-digits]
  (reduce + (map biginteger (map (fn [bank]
                                   (compute-bank-joltage-v3 bank number-of-digits)) banks))))


(deftest test-sum-banks-jotable-v2
  (testing "sum example given"
    (is (= 3121910778619 (sum-banks-jotable-v2 ["987654321111111" "811111111111119" "234234234234278" "818181911112111"]))))
  (testing "sum from file with v1"
    (is (= 3121910778619 (sum-banks-jotable-v2 (read-input-file "test/exo3-sample.txt")))))
  )


(deftest test-exo3-decode-part-2
  (testing "decode real data part 2"
    (is (= 168794698570517 (sum-banks-jotable-v2 (read-input-file "test/exo3-real-data.txt"))))))


(deftest test-compute-bank-joltage-v3
  (testing "test compute bank joltage v3 for 29641 with 4 digits"
    (is (= "9641" (compute-bank-joltage-v3 "29641" 4))))
  (testing "test compute bank joltage v3 for 987654321111111 with 12 digits"
    (is (= "987654321111" (compute-bank-joltage-v3 "987654321111111" 12))))
  (testing "test compute bank joltage v3 for 811111111111119 with 12 digits"
    (is (= "811111111119" (compute-bank-joltage-v3 "811111111111119" 12))))
  (testing "test compute bank joltage v3 for 234234234234278 with 12 digits"
    (is (= "434234234278" (compute-bank-joltage-v3 "234234234234278" 12))))
  (testing "test compute bank joltage v3 for 818181911112111 with 12 digits"
    (is (= "888911112111" (compute-bank-joltage-v3 "818181911112111" 12)))))


(deftest test-using-v3-instead-for-all-data
  (testing "sum example given"
    (is (= 357 (sum-banks-jotable-v3 ["987654321111111" "811111111111119" "234234234234278" "818181911112111"] 2))))
  (testing "sum from file with with size for part 1 is 2 output digits"
    (is (= 17095 (sum-banks-jotable-v3 (read-input-file "test/exo3-real-data.txt") 2))))
  (testing "decode real data with v3 part 2"
    (is (= 168794698570517 (sum-banks-jotable-v3 (read-input-file "test/exo3-real-data.txt") 12)))))
