;==========================================================
; Final Exam - Problem 1
; Date: December 05, 2019.
; Author:
;          A01377647 Hector Takami
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

;==========================================================
(defmacro tackle
  [[var1 expr1 expr2] [var2 expr3]]
  `(try
     (let [~var1 ~expr1] ~expr2)
     (catch Exception ~var2  ~expr3))
  )

;==========================================================
(deftest test-tackle
  (is (= '(try (clojure.core/let [x (/ 1 0)] (+ x 1))
               (catch java.lang.Exception e (.getMessage e)))
         (macroexpand-1 '(tackle
                           [x (/ 1 0) (+ x 1)]
                           [e (.getMessage e)]))))
  (is (= "Divide by zero"
         (tackle
           [x (/ 1 0) (+ x 1)]
           [e (.getMessage e)])))
  (is (= 3/2
         (tackle
           [x (/ 1 2) (+ x 1)]
           [e (.getMessage e)])))
  (is (= "The result is :c"
         (tackle
           [result ([:a :b :c] 2) (str "The result is " result)]
           [exception (str "An instance of the " (type exception) " was thrown.")])))
  (is (= "An instance of the class java.lang.IndexOutOfBoundsException was thrown"
         (tackle
           [result ([:a :b :c] 3) (str "The result is " result)]
           [exception (str "An instance of the " (type exception) " was thrown")]))))

;==========================================================
(run-tests)
