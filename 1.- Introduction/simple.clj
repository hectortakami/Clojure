; Title:      Simple exercises
; Author:     Hector Manuel Takami Flores
; Student ID: A01377647@itesm.mx

(require '[clojure.test :refer [deftest is run-tests]])
(use 'clojure.math.numeric-tower)

;************* FUNCTIONS DEFINITIONS *************

(defn f2c [f]
  ; Takes x degrees Fahrenheit and converts them to degrees Celsius;
    (/ (* (- f 32) 5) 9))

(defn sign [n]
  ; Takes an integer value n. It returns -1 if n is negative, 1 if n is positive greater than zero, or 0 if n is zero
  (cond
    (= n 0) 0
    (< n 0) -1
    :else   1)
  )

(defn roots [a b c]
  ; Returns a vector containing the two possible roots that solve a quadratic equation given its three coefficients (a, b, c)
  (let [d (- b)
        e (sqrt (- (* b b) (* 4 a c)))
        f (* 2 a)
        x1 (/ (+ d e) f)
        x2 (/ (- d e) f)]
    [x1 x2])
  )


(defn bmi [weight height]
  ; Takes two arguments: weight and height. It should return a symbol that represents the corresponding BMI description computed from its input
  (let [bmi-value (/ weight (* height height) )]
    (cond
      (< bmi-value 20) 'underweight
      (< bmi-value 25) 'normal
      (< bmi-value 30) 'obese1
      (< bmi-value 40) 'obese2
      :else            'obese3)
    )
)


  ;************* TESTS DEFINITIONS *************
  (deftest test-f2c
    (is (= 100.0 (f2c 212.0)))
    (is (= 0.0 (f2c 32.0)))
    (is (= -40.0 (f2c -40.0))))

  (deftest test-sign
    (is (= -1 (sign -5)))
    (is (= 1 (sign 10)))
    (is (= 0 (sign 0))))

  (deftest test-roots
    (is (= [-1 -1] (roots 2 4 2)))
    (is (= [0 0] (roots 1 0 0)))
    (is (= [-1/4 -1] (roots 4 5 1))))

  (deftest test-bmi
    (is (= 'underweight (bmi 45 1.7)))
    (is (= 'normal (bmi 55 1.5)))
    (is (= 'obese1 (bmi 76 1.7)))
    (is (= 'obese2 (bmi 81 1.6)))
    (is (= 'obese3 (bmi 120 1.6))))



  (run-tests)
