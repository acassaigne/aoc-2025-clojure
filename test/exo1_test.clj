(ns exo1-test
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
  (let [new-position (remain-from-100 (+ current-value (remain-from-100 distance)))]
    (if (< new-position 0)
      (+ 100 new-position)
      new-position)))

(defn compute-new-zero-passed-counter
  [new-position counted-zero]
  (if (= new-position 0)
    (inc counted-zero)
    counted-zero))

(defn next-position
  [current-position distance]
  (let
    [new-position (get-new-position (.position current-position) distance)
     new-counted-zero (compute-new-zero-passed-counter new-position (.-zero-passed-counter current-position))]
    (->dial-position new-position new-counted-zero)))


(defn convert-dials-to-values
  [dial]
  (map split-dial-to-value (map split-dial dial)))

(defn treat-dials-over
  [dials]
  (let [converted-dials (convert-dials-to-values dials)
        start-position (->dial-position 50 0)]
    (reduce next-position start-position converted-dials)))

;; V2

(defn compute-new-position-v2
  [current-position distance]
  (let [new-position (+ current-position (remain-from-100 distance))
        tmp-counter-passed-zero (if (and (> current-position 0) (<= new-position 0)) 1 0)
        counter-passed-zero (+ tmp-counter-passed-zero (quot (abs distance) 100))]
    (if (< new-position 0)
      (->dial-position (+ 100 new-position) counter-passed-zero)
      (->dial-position (remain-from-100 new-position) counter-passed-zero))))

(defn get-zero-passed-counter [item]
  (.-zero-passed-counter item))

(defn get-position [item]
  (.position item))

(defn decode-password-v1
  [dials]
  (get-zero-passed-counter (treat-dials-over dials)))

(defn read-dials
  [filename]
  (str/split-lines (slurp filename)))

;; V2 ALGO


(defn count-if-pass-over-zero [start-position remain-distance]
  (let [new-position (+ start-position remain-distance)]
    (if (or (and (> start-position 0) (<= new-position 0))
            (>= new-position 100))
      1
      0))
  )

(defn count-multiple-rounds [distance]
  (abs (quot distance 100)))

(defn next-position-v2
  [current-dial-position distance]
  (let
    [start-position (get-position current-dial-position)
     actual-counter (get-zero-passed-counter current-dial-position)
     new-dial-position (compute-new-position-v2 start-position distance)
     tmp-counter-for-zero (+ actual-counter
                             (count-multiple-rounds distance)
                             (count-if-pass-over-zero start-position (remain-from-100 distance)))
     ]
    (->dial-position (get-position new-dial-position) tmp-counter-for-zero)))


(defn treat-dials-over-v2
  [dials]
  (let [converted-dials (convert-dials-to-values dials)
        start-position (->dial-position 50 0)]
    (reduce next-position-v2 start-position converted-dials)))

(defn decode-password-v2
  [dials]
  (get-zero-passed-counter (treat-dials-over-v2 dials)))

(defn create-dial-position [position zero-counter]
  (->dial-position position zero-counter))

(deftest bug-on-compute-new-position-v2
  (testing "bug new-position-v2 arrived on zero"
    (is (= (create-dial-position 0 2) (next-position-v2 (create-dial-position 52 1) 48))))
  (testing "bug new-position-v2 below zero"
    (is (= (create-dial-position 99 1) (next-position-v2 (create-dial-position 48 0) -49))))
  (testing "bug on zero new-position-v2"
    (is (= (create-dial-position 0 1) (next-position-v2 (create-dial-position 52 0) -52))))
  (testing "bug on 95 new-position-v2"
    (is (= (create-dial-position 95 0) (next-position-v2 (create-dial-position 0 0) -5))))
  (testing "bug on L652 new-position-v2"
    (is (= (create-dial-position 48 6) (next-position-v2 (create-dial-position 0 0) -652))))
  )


(deftest counter-when-pass-over-zero
  (testing "pass over zero with negative movement with start position at 48 and remain of distance -52 return 1"
    (is (= 1 (count-if-pass-over-zero 48 -52))))
  (testing "just arriving on zero with negative movement with start position at 48 and remain of distance -48 return 1"
    (is (= 1 (count-if-pass-over-zero 48 -48))))
  (testing "pass over zero with positive movement with start position at 96 and remain of distance 6 return 1"
    (is (= 1 (count-if-pass-over-zero 96 6))))
  (testing "just arriving at zero with positive movement with start position at 96 and remain of distance 4 return 1"
    (is (= 1 (count-if-pass-over-zero 96 4))))
  (testing "start at zero with negative movement of 2 has to return 0"
    (is (= 0 (count-if-pass-over-zero 0 -2))))
  )


(deftest counter-multiple-rounds
  (testing "count multiple rounds for distance of 76 is zero"
    (is (= 0 (count-multiple-rounds 76))))
  (testing "count multiple rounds for distance of 123 is one"
    (is (= 1 (count-multiple-rounds 123))))
  (testing "count multiple rounds for distance of 200 is two"
    (is (= 2 (count-multiple-rounds 200))))
  (testing "count multiple rounds for distance of 200 is two"
    (is (= 3 (count-multiple-rounds -352))))
  )

(deftest full-examples-treating-dials-to-new-position-V2
  (testing "V2 algo new position when treating those dials L2 L49 from starting point 50"
    (is (= (->dial-position 99 1) (treat-dials-over-v2 ["L2" "L49"]))))
  (testing "V2 algo new position when treating those dials R49 R2 from starting point 50"
    (is (= (->dial-position 1 1) (treat-dials-over-v2 ["R49" "R2"]))))
  )

;; TRY SOMETHING ON DEBUG REDUCE
(defn debug-get-result
  [result]
  (.-current-accumulator result))

(defn create-debug-reduce
  [new-accumulator result]
  (->debug-reduce new-accumulator (conj (.-all-previous-accumulators result) (.-current-accumulator result))))

(defn debug-next-position-v2
  [debug-result distance]
  (let
    [new-dial-position (next-position-v2 (debug-get-result debug-result) distance)]
    (create-debug-reduce new-dial-position debug-result)))


(defn debug-treat-dials-over
  [dials]
  (let [converted-dials (convert-dials-to-values dials)
        start-position (->debug-reduce (->dial-position 50 0) [])]
    (reduce debug-next-position-v2 start-position converted-dials)))

(def initial-debug-reduce (->debug-reduce nil []))

(deftest tests-debug-acc
  (testing "debug accumulator"
    (is (= (->debug-reduce 4 [nil]) (create-debug-reduce 4 initial-debug-reduce))))
  (testing "get current result"
    (is (= "Hello" (debug-get-result (->debug-reduce "Hello" []))))))

(deftest test-debug-reduce
  (testing "New position when treating those dials R2 R4 from starting point 50"
    (is (= (->debug-reduce (->dial-position 56 0) [(->dial-position 50 0)
                                                   (->dial-position 52 0)])
           (debug-treat-dials-over ["R2" "R4"]))))
  ;(testing "debug on sample"
  ;  (is (= (->debug-reduce (->dial-position 56 6) [])
  ;         (debug-treat-dials-over (read-dials "test/exo1-real-data.txt")))))
  )

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
  (testing "start at default position move direction 100 expect 1 zero-passed new current-position 50"
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

;; SOLUTION ALGO V2
(deftest tests-sample-file-decode-password-v2
  (testing "password for two simple dials"
    (is (= 1 (decode-password-v2 ["R2" "L52"]))))
  (testing "password for example gave in exo1 from file"
    (is (= 6 (decode-password-v2 (read-dials "test/exo1-example-data.txt"))))))

(deftest tests-real-decode-password-v1
  (testing "decode real file with V1"
    (is (= 992 (decode-password-v1 (read-dials "test/exo1-real-data.txt"))))))

(deftest tests-real-decode-password-v2
  (testing "decode real file with V2"
    (is (= 6133 (decode-password-v2 (read-dials "test/exo1-real-data.txt"))))))


