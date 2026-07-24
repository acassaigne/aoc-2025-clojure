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
      [new-position (+ (.position acc) distance)
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
    (is (= 24 (splited-dial-to-value (->dial-splited "R" "24")))))
  )


(def start-dial-position (->dial-position 50 0))

(deftest count-zero-passed-dial-example
  (testing "start at 50 move directon 2 expect 0 zero-passed new current-position 52"
    (is (= (->dial-position 52 0) (next-position start-dial-position 2))))
  (testing "start at default position move directon 100 expect 1 zero-passed new current-position 50"
    (is (= (->dial-position 50 1) (next-position start-dial-position 100)))))
