;==========================================================
; Final Exam - Problem 3
; Date: December 05, 2019.
; Author:
;          A01377647 Hector Takami
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

;==========================================================
(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn contains-all-digits?
  [n]
  (let [x (digits n)]
    (if (some #{1} x)
      (if (some #{2} x)
        (if (some #{3} x)
          (if (some #{4} x)
            (if (some #{5} x)
              (if (some #{6} x)
                (if (some #{7} x)
                  (if (some #{8} x)
                    (if (some #{9} x)
                      (if (some #{0} x) true
                                        false)
                      false)
                    false)
                  false)
                false)
              false)
            false)
          false)
        false)
      )
    )
  )




  ;==========================================================
  (deftest test-contains-all-digits?
    (is (contains-all-digits? 1023456789))
    (is (contains-all-digits? 5897230146))
    (is (contains-all-digits? 10123485679))
    (is (contains-all-digits? 1223334444555566666677777778888888889999999990))
    (is (not (contains-all-digits? 1236)))
    (is (not (contains-all-digits? 1112223334455)))
    (is (not (contains-all-digits? -587230462413578)))
    (is (not (contains-all-digits? -122333444455556666667777777888888888999999999))))

  ;==========================================================
  (run-tests)
