;----------------------------------------------------------
; Metacircular Evaluator
; Date: October 17, 2019.
; Author:
;          A01377647 Hector Takami
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])

"
Map Creation -> (def m {'key1 10, 'key2 20})
Map Getting Value -> (get m 'key1) or (m 'key1) = 10
"
(import 'clojure.lang.IFn)
(declare $eval)

(deftype Closure
  [env params body]
  IFn
  (applyTo [self args]
    ; We bind the variables with its values, extend the enviroment and evaluate the function
    ($eval body (merge env (zipmap params args))))
  )

(defn third
  "Returns the third element from a list"
  [lst]
  (nth lst 2))

(defn fourth
  "Returns the fourth element from a list"
  [lst]
  (nth lst 3))

(defn $eval
  "Evaluates expr using the bindings in the env map"
  [expr env]
  (cond
    ; Variable reference
    (symbol? expr) (if (contains? env expr)
                     (env expr)
                     (throw (RuntimeException. (str "Unbound variable: " expr)))
                     )

    ; Checks for special forms
    (list? expr) (case (first expr)
                   ; Found an empty list
                   ; Ex. ($eval '() {})
                   nil
                   ()
                   ; Found "quote" in the expression
                   ; Ex. ($eval '(quote (+ 1 2)) {})
                   quote
                   (second expr)
                   ; Found "if" in the expression
                   ; Ex. ($eval '(if 1 2 3 ) {}) -> 2
                   ; Ex. ($eval '(if false 2 3 ) {}) -> 3
                   if
                   (if ($eval (second expr) env)
                     ($eval (third expr) env)
                     ($eval (fourth expr) env)
                     )
                   ; Found an anonymous function to evaluate
                   ; Ex. ($eval '((lambda (x) (+ 1 x)) 4) {'+ +}) -> 5
                   ; ($eval '((lambda (x) (cons x (cons x ()))) 'a) {'cons cons}) -> (a a)
                   lambda
                   (->Closure (atom env) (second expr) (third expr))


                   label
                   (let [closure ($eval (third $eval) env)]
                     (swap! (.env closure)
                            #(assoc % (second expr) closure))
                     closure)

                   do
                   (let [x (first (rest expr))
                         y (second (rest expr))
                         z (third (rest expr))
                         q (fourth (rest expr))
                         ]
                     ($eval x env)
                     ($eval y env)
                     ($eval z env)
                     ($eval q env)
                     )

                   dotimes
                   (do (reduce (fn [p v] ($eval (third expr) (assoc env (first (second expr)) v))) 0
                               (range ($eval (second (second expr)) env)))
                       nil)
                   let
                   (do ($eval (third expr)
                              (assoc env (first (second expr))
                                         ($eval (second (second expr)) env))))

                   cond (cond
                          ($eval (second expr) env) ($eval (third expr) env)
                          ($eval (fourth expr) env) ($eval (nth expr 4) env)
                          ($eval (nth expr 5) env) ($eval (nth expr 6) env)
                          ($eval (nth expr 7) env) ($eval (nth expr 8) env)
                          :else nil)

                   ; Ordinary function application
                   ; Ex. ($eval '(plus 1 2) {'plus +}) -> 3
                   (apply ($eval (first expr) env) (map #($eval % env) (rest expr)))

                   )

    ; Anything that is not a symbol or a list evals to itself
    :else expr
    )
  )
(deftest test-do
  (is (= 4
         ($eval '(do (prn (+ -2 (+ 1 2)))
                     (prn (+ 1 1))
                     (prn 3)
                     (+ 2 2))
                {'+ +
                 'prn prn}))))

(deftest test-do-times
  (is (= "0123456789"
         (with-out-str ($eval '(dotimes (x 10)
                                 (pr x))
                              {'pr pr}))))
  (is (= (let [nl (System/lineSeparator)]
           (str "Line 0" nl "Line 1" nl "Line 2" nl "Line 3" nl))
         (with-out-str ($eval '(dotimes (i (+ 2 2))
                                 (println "Line" i))
                              {'println println, '+ +})))))

  (deftest test-let
  (is (= 42
         ($eval '(let (x 6)
                   (* 7 x))
                {'* *})))
  (is (= 111
         ($eval '(let (x (* 2 5))
                       (let (y (+ 1 x))
                         (+ 1 (* y x))))
                    {'+ +
                     '* *}))))

(deftest test-cond
  (is (= 'three
         ($eval '(cond
                   (= x 1) (quote one)
                   (= x 2) (quote two)
                   (= x 3) (quote three)
                   (= x 4) (quote four)
                   true    (quote other))
                {'x 3
                 '= =}))))

  (run-tests)