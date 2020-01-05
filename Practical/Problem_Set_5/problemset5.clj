;----------------------------------------------------------
; Problem Set #5
; Date: November 11, 2019.
; Author:
;          A01377647 Hector Takami
;----------------------------------------------------------
(require '[clojure.test :refer [deftest is run-tests]])

;************* FUNCTIONS DEFINITIONS *************
(defmacro my-or
  "Evaluates its expressions one at a time, from left to right. If a form returns a
  logical true value, it returns that value and doesn't evaluate any of the other
  expressions, otherwise  it returns the value of the last expression. (or) returns nil."
  ([] nil)
  ([x & args]
   `(if ~x
      ~x
      (my-or ~@args)))
  )

(defmacro do-loop
  "This construct consists of a body (one or more expressions, presumably with side effects)
  and a final conditional form prefaced with a :while or :until keyword. First, the expressions
  in the body are evaluated sequentially, and then the condition is evaluated. If the final form
  uses a :while keyword, the body of the loop is repeated while the condition holds true. On the
  other hand, if the final form uses an :until keyword, the body of the loop is repeated while
  the condition holds false (or in other words, the loop terminates when the condition is true).
  Returns nil."
  [& args]
  (let [until (first (last args))
        action (second (last args))]
    `(do
       ~@(butlast args)
       (while
         ~(if
            (= until :while)
            action
            `(not ~action))
         ~@(butlast args))))
  )

(defmacro def-pred
  "Takes a name, an arg vector, and a body of one or more expressions.
  The macro should define two predicate functions: a regular one and its negated version.
  The name of the negated predicate should be the same as name but with a \"not-\" prefix,
  and its result should be negated using the not function. "
  [pred args & expr]
  `(do
     (defn ~pred
       ~args
       (do ~@expr))
     (defn ~(symbol (str "not-" pred))
       ~args
       (not (do ~@expr)))
     )
  )


(defmacro defn-curry
  "Performs a currying transformation to a function definition.
  https://en.wikipedia.org/wiki/Currying"
  [name args & body]
  `(defn ~name [~(first args)]
     ~(reduce
        (fn [nestedFns# currArg#]
          `(fn [~currArg#] ~nestedFns#)
          )
        `(do ~@body)
        (reverse (rest args))
        )
     )
  )

(defmacro IF
  "Its purpose is to provide a conditional statement that is syntactically a bit more similar
  to those found in languages like Pascal or Fortran.
  It has the following form.
      (IF (< 3 1)
          1 2 3
          :THEN 4 5 6)
          :ELSE 7 8 9))
          ⇒ (if (< 3 1) (do 4 5 6) (do 7 8 9))
  Almost everything is optional in the IF macro, except condition.
  Also, the :ELSE keyword may come before the :THEN keyword."

  [test & body]
  (let [
        not-else?# #(not= % :ELSE)
        not-then?# #(not= % :THEN)
        thenPart# (take-while not-else?# (drop-while not-then?# body))
        elsePart# (take-while not-then?# (drop-while not-else?# body))
        ]
    `(if ~test (do ~@(rest thenPart#)) (do ~@(rest elsePart#)))
    )
  )







;************* TESTS DEFINITIONS *************
(deftest test-my-or
  (is (= nil (my-or)))
  (is (= :one (my-or false :one nil :two false :three)))
  (is (= nil (my-or false false nil)))
  (is (= nil (my-or nil nil false))))

(deftest test-do-loop
  (println "\nDo-loop Test 1")
  (def i (atom 0))
  (is (= nil (do-loop
               (println @i)
               (swap! i inc)
               (:until (= @i 5)))))
  (println "\nDo-loop Test 2")
  (def j (atom 1))
  (is (= nil (do-loop
               (println @j)
               (swap! j inc)
               (:while (<= @j 5)))))
  )

(deftest test-def-pred
  (def-pred less-than-one? [x] (< x 1))

  (is (= true (less-than-one? 0)))
  (is (= false (less-than-one? 2)))
  (is (= false (not-less-than-one? 0)))
  (is (= true (not-less-than-one? 2)))

  (def-pred plural? [s] (println "check s in" s) (= \s (last s)))

  (println "\nDef-pred 'plural?' Tests")
  (is (= true (plural? "boys")))
  (is (= false (plural? "girl")))
  (is (= false (not-plural? "boys")))
  (is (= true (not-plural? "girl")))
  )

(deftest test-defn-curry
  (defn-curry sum
              [a b c d]
              (prn 'args a b c d)
              (+ a b c d))
  (is (= 10 ((((sum 1) 2) 3) 4)))
  (is (= 81 ((((sum 15) 8) 16) 42)))


  (defn-curry go [x y] (* x (+ y 1)))
  (is (= 8 ((go 2) 3)))
  (is (= 9 ((go 3) 2)))

  (defn-curry add1 [x] (+ x 1))
  (is (= 1 (add1 0)))
  (is (= 42 (add1 41)))
  )

(deftest test-IF
  (is (= 'ok (IF (> 3 1) :THEN 'ok :ELSE 'oops)))
  (is (= 'ok (IF (> 3 1) :THEN 'ok)))
  (is (= 'ok (IF (< 3 1) :ELSE 'ok)))
  (is (= nil (IF (> 3 1) :ELSE 'oops)))
  (is (= nil (IF (> 3 1) :THEN)))
  (is (= nil (IF (> 3 1))))
  )



(run-tests)