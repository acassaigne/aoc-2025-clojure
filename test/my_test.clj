(ns my-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as str]))

(defrecord rec-dial-split [direction distance])
(defrecord dial-position [position zero-passed-counter])
(defrecord debug-reduce [current-accumulator all-previous-accumulators])

(defn split-dial
  [dial]
  (->rec-dial-split (subs dial 0 1)
                  (subs dial 1)))

(defn split-dial-to-value
  [dial]
  (let [R-or-L (.direction dial)
        distance (Integer/parseInt (.distance dial))]
    (if (= R-or-L "L")
      (- distance)
      distance)))

(defn remain-from-100
  [value]
  (rem value 100))

(defn get-new-position
  [current-value distance]
  (let [new-position (+ current-value (remain-from-100 distance))]
    (if (< new-position 0)
      (+ 100 new-position)
      (remain-from-100 new-position)))
  )

(defn get-new-counted-zero
  [new-position counted-zero]
  (if (= new-position 0)
    (inc counted-zero)
    counted-zero))

(defn next-position
  [current-position distance]
  (let
    [new-position (get-new-position (.position current-position) distance)
     new-counted-zero (get-new-counted-zero new-position (.-zero-passed-counter current-position))]
    (->dial-position new-position new-counted-zero)))


(defn convert-dials-to-values
  [dial]
  (map split-dial-to-value (map split-dial dial)))

(defn treat-dials-over
  [dials]
  (let [converted-dials (convert-dials-to-values dials)
        start-position (->dial-position 50 0)]
    (reduce next-position start-position converted-dials)))


;; TRY SOMETHING ON DEBUG REDUCE

(defn debug-get-result
  [result]
  (.-current-accumulator result))

(defn debug-new-accumulator
  [new-accumulator result]
  (->debug-reduce new-accumulator (conj (.-all-previous-accumulators result) (.-current-accumulator result))))

(defn next-position
  [current-position distance]
  (let
    [new-position (get-new-position (.position current-position) distance)
     new-counted-zero (get-new-counted-zero new-position (.-zero-passed-counter current-position))]
    (->dial-position new-position new-counted-zero)))

(defn debug-next-position
  [debug-result distance]
  (let
    [new-position (get-new-position (.position (debug-get-result debug-result)) distance)
     new-counted-zero (get-new-counted-zero new-position (.-zero-passed-counter (debug-get-result debug-result)))]
    (debug-new-accumulator (->dial-position new-position new-counted-zero) debug-result)))


(defn debug-treat-dials-over
  [dials]
  (let [converted-dials (convert-dials-to-values dials)
        start-position (->debug-reduce (->dial-position 50 0) [])]
    (reduce debug-next-position start-position converted-dials)))


(defn decode-password-v1
  [dials]
  (.-zero-passed-counter (treat-dials-over dials)))

(defn read-dials
  [filename]
  (str/split-lines (slurp filename)))



;;
;; TEST
;;

(deftest split-dial-example
  (testing "split dial L50"
    (is (= (->rec-dial-split "L" "50") (split-dial "L50")))))

(deftest split-dial-to-value-example
  (testing "with split dial L 50 get -50"
    (is (= -50 (split-dial-to-value (->rec-dial-split "L" "50")))))
  (testing "with split dial R 24 get 24"
    (is (= 24 (split-dial-to-value (->rec-dial-split "R" "24"))))))


(def start-dial-position (->dial-position 50 0))

(deftest next-position-examples
  (testing "start at 50 move directon 2 expect 0 zero-passed new current-position 52"
    (is (= (->dial-position 52 0) (next-position start-dial-position 2))))
  (testing "start at default position move directon 100 expect 1 zero-passed new current-position 50"
    (is (= (->dial-position 50 0) (next-position start-dial-position 100))))
  (testing "bug on modulo with mod function replace by rem"
    (is (= (->dial-position 0 1) (next-position (->dial-position 56 0) -56))))
  (testing "bug on left L68 from default starting point"
    (is (= (->dial-position 82 0) (next-position (->dial-position 50 0) -68))))
  (testing "bug on 100 has to be 0, starting point 52 apply dial R48 expected 0"
    (is (= (->dial-position 0 1) (next-position (->dial-position 52 0) 48))))
  )

(deftest convert-multiples-dials-to-their-values
  (testing "convert dials to values"
    (is (= [2 4 -12] (convert-dials-to-values ["R2" "R4" "L12"])))))


(deftest full-examples-treating-dials-to-new-position
  (testing "New position when treating those dials R2 R4 from starting point 50"
    (is (= (->dial-position 56 0) (treat-dials-over ["R2" "R4"]))))
  (testing "New position when treating those dials R2 R4 L56 from starting point 50"
    (is (= (->dial-position 0 1) (treat-dials-over ["R2" "R4" "L56"])))))

(deftest tests-sample-file-decode-password-v1
  (testing "password for two simple dials"
    (is (= 1 (decode-password-v1 ["R2" "L52"]))))
  (testing "password for example gave in exo1 from file"
    (is (= 3 (decode-password-v1 (read-dials "test/exo1-example-data.txt")))))
  )

(deftest tests-real-decode-password-v1
  (testing "decode real file with V1"
    (is (= 992 (decode-password-v1 (read-dials "test/exo1-real-data.txt"))))))

(def initial-debug-reduce (->debug-reduce nil []))

(deftest tests-debug-acc
  (testing "debug accumulator"
    (is (= (->debug-reduce 4 [nil]) (debug-new-accumulator 4 initial-debug-reduce))))
  (testing "get current result"
    (is (= "Hello" (debug-get-result (->debug-reduce "Hello" []))))))

(deftest test-debug-reduce
  (testing "New position when treating those dials R2 R4 from starting point 50"
    (is (=  (->debug-reduce (->dial-position 56 0) [(->dial-position 50 0)
                                                    (->dial-position 52 0) ])
            (debug-treat-dials-over ["R2" "R4"])))))
