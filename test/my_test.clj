(ns my-test
  (:require [clojure.test :refer :all])
  (:require [exo1 :as e1] ))


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
       counted-zero (.counted-zero-passed acc)]
  (if (= new-position 0)
    (->dial-position new-position (inc counted-zero))
    (->dial-position new-position counted-zero))))

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

(def start-dial-position (->dial-position 50 0))

(deftest next-position-examples
  (testing "start at 50 move directon 2 expect 0 zero-passed new current-position 52"
    (is (= (->dial-position 52 0) (next-position start-dial-position 2))))
  (testing "start at default position move directon 100 expect 1 zero-passed new current-position 50"
    (is (= (->dial-position 50 0) (next-position start-dial-position 100))))
  (testing "bug on modulo with mod function replace by rem"
    (is (= (->dial-position 0 1) (next-position (->dial-position 56 0) -56))))
  )



(deftest convert-multiples-dials-to-their-values
  (testing "convert dials to values"
    (is (= [2 4 -12] (convert-dials-to-values ["R2" "R4" "L12"])))))


(deftest full-examples
  (testing "Treat those dails R2 R4 from starting point 50"
    (is (= (->dial-position 56 0) (treat-dails-over ["R2" "R4"]))))
(testing "Treat those dails R2 R4 L56 from starting point 50"
    (is (= (->dial-position 0 1) (treat-dails-over ["R2" "R4" "L56"]))))
  )
