;==========================================================
; Final Exam - Problem 2
; Date: December 05, 2019.
; Author:
;          A01377647 Hector Takami
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.fd :as fd])

;==========================================================
(logic/defne
  increaseo
  [lst]
  ([[]])
  ([[x]])
  ([ [first second . tail] ]
   (fd/< first second)
   (logic/fresh [tmp]
                (logic/appendo [second] tail tmp)
                (increaseo tmp)
                )
   )
  )

;==========================================================
(deftest test-increaseo
  (is (= [:yes]
         (logic/run 1 [q]
                    (increaseo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (increaseo [7])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (increaseo [2 4 5 6 8])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
           (increaseo [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])
           (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
           (increaseo [3 4 2 6])
           (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
           (increaseo [1 2 3 4 5 6 7 8 9 11 12 13 14 14 15])
           (logic/== q :yes))))
  (is (= [3 4]
         (logic/run* [q]
           (fd/in q (fd/interval 1 10))
           (increaseo [1 2 q 5]))))
  (is (= [1 2 3 4]
         (logic/run* [q]
           (fd/in q (fd/interval 1 10))
           (increaseo [q 5]))))
  (is (= [6 7 8 9 10]
         (logic/run* [q]
           (fd/in q (fd/interval 1 10))
           (increaseo [5 q]))))
  (is (= #{[1 2 3]
           [1 2 4]
           [1 2 5]
           [1 3 4]
           [1 3 5]
           [1 4 5]
           [2 3 4]
           [2 3 5]
           [2 4 5]
           [3 4 5]}
         (set
           (logic/run* [q1 q2 q3]
             (fd/in q1 q2 q3 (fd/interval 1 5))
             (increaseo [q1 q2 q3])))))
  )

;==========================================================
(run-tests)
