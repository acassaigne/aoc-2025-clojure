(ns my-test
  (:require [clojure.test :refer :all])
  (:require [exo1 :as e1] )
  (:require [clojure.string :as str]))


(defrecord dial-splited [direction distance])
(defrecord dial-position [position counted-zero-passed])


(defn split-dial
  [dial]
  (->dial-splited (subs dial 0 1)
                  (subs dial 1)))

(defn splited-dial-to-value
  [dial]
  (let [R-or-L (.direction dial)
        distance (Integer/parseInt  (.distance  dial)) ]
    (if (= R-or-L "L")
      (- distance)
      distance)))
  

(defn next-position
  [acc distance]
(let
    [new-position (+ (.position acc) (rem distance 100))
     new-positive-position (if (< new-position 0)
                             (+ 100 new-position)
                             (rem new-position 100))
     counted-zero (.counted-zero-passed acc)
     new-dial (if (= new-positive-position 0)
                (->dial-position new-positive-position (inc counted-zero))
                (->dial-position new-positive-position counted-zero))
     ]
      ;;(println new-dial)
    new-dial))

(def d (->dial-splited "L" "48"))

(deftest split-dial-example
  (testing "split dial L50"
    (is (= (->dial-splited "L" "50") (split-dial "L50")))))

(deftest splited-dial-to-value-example
  (testing "with splited dial L 50 get -50"
    (is (= -50 (splited-dial-to-value (->dial-splited "L" "50")))))
  (testing "with splited dial R 24 get 24"
    (is (= 24 (splited-dial-to-value (->dial-splited "R" "24"))))))

(defn convert-dials-to-values
  [dial]
  (map splited-dial-to-value  (map split-dial dial)))

(defn treat-dails-over
  [dials]
  (let [converted-dials (convert-dials-to-values dials)
        start-position (->dial-position 50 0)]
    (reduce next-position start-position converted-dials)))

(defn decode-password-v1
  [dials]
  (.counted-zero-passed (treat-dails-over dials)))

(defn read-dials
  [filename]
  (str/split-lines (slurp filename)))

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
    (is (= (->dial-position 56 0) (treat-dails-over ["R2" "R4"]))))
  (testing "New position when treating those dials R2 R4 L56 from starting point 50"
    (is (= (->dial-position 0 1) (treat-dails-over ["R2" "R4" "L56"])))))

(deftest tests-sample-file-decode-password-v1
  (testing "password for two simple dials"
    (is (= 1 (decode-password-v1 ["R2" "L52"]))))
  (testing "password for example gave in exo1 from file"
    (is (= 3 (decode-password-v1 (read-dials "test/exo1-example-data.txt")))))
  )

(deftest tests-real-decode-password-v1
  (testing "decode real file with V1"
    (is (= 992 (decode-password-v1 (read-dials "test/exo1-real-data.txt"))))))

