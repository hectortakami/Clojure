;----------------------------------------------------------
; Problem Set #1
; Date: September 20, 2019.
; Author:
;          A01377647 Hector Takami
;----------------------------------------------------------
(require '[clojure.math.numeric-tower :refer [abs]])
(require '[clojure.test :refer [deftest is run-tests]])

;************* FUNCTIONS DEFINITIONS *************


(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(defn there-exists-one
  " Takes two arguments: a one argument predicate function pred and a list lst.
    Returns true if there is exactly one element in lst that satisfies pred, otherwise returns false"
  [pred lst]
  (let [new (filter pred lst)]
    (cond
      (not (empty? new)) (if (= 1 (count new)) true
                                               false)
      :else false)
    )
  )

(defn my-drop-while
  " Takes two arguments: a function f and a list lst.
    It returns a list of items from lst dropping the initial items that evaluate to true when passed to f.
    Once a false value is encountered, the rest of the list is returned.
    Function f should accept one argument.
    Do not use the predefined drop-while function."
  [f lst]
  (loop [lst lst
         result ()]
    (if (not-empty lst)
      (if (f (first lst))
        (recur (rest lst)
               result)
        (recur (drop (count lst) lst)
               (concat lst result)))
      result)))

(defn bisection
  " Takes a, b, and f as arguments. It finds the corresponding root using the bisection method."
  [a b f]
  (loop [a a
         b b]
    (let [c (/ (+ a b) 2)]

      (if (< (abs (f c)) 1e-15)
        (float c)
        (if (pos? (* (f a) (f c)))
          (recur c
                 b)
          (recur a
                 c))
        )
      )
    )
  )

(defn linear-search
  " Takes three arguments: a vector vct, a data value x, and an equality function eq-fun.
    It sequentially searches for x in vct using eq-fun to compare x with the elements contained in vct.
    The eq-fun should accept two arguments, a and b, and return true if a is equal to b, or false otherwise.
    The linear-search function returns the index where the first occurrence of x is found in vct (the first element of the vector is at index 0), or nil if not found."
  [vct x eq-fun]
  (loop [vct vct
         n 0
         valor 0]
    (if (empty? vct)
      (if (= valor 0)
        'nil
        n)
      (if (eq-fun x (first vct))
        (recur (drop (count vct) vct)
               n
               1)
        (recur (rest vct)
               (+ n 1)
               valor)))
    )
  )

(defn deriv
  " Takes f and h as its arguments, and returns a new function that takes x as argument, and which represents the derivative of f given a certain value for h"
  [f h]
  (fn [x] (/ (- (f (+ h x)) (f x)) h))
  )


(defn integral
  "Takes as arguments a, b, n, and f. It returns the value of the integral, using Simpson’s rule"
  [a b n f]
  (let [h (/ (- b a) n)
        div (/ h 3)
        yk (fn [k] (f (+ a (* k h))))
        odd (filter odd? (range 1 n))
        even (filter even? (range 1 n))]
    (* div (+ (yk 0) (yk n) (reduce + (map #(* (yk %) 4) odd)) (reduce + (map #(* (yk %) 2) even))))
    )
  )

(defn binary-search
  " Binary search consists in searching a sorted vector by repeatedly dividing the search interval in half.
    You begin with an interval covering the whole vector. If the value being searched is less than the item in the middle of the interval, narrow the interval to the lower half.
    Otherwise narrow it to the upper half.
    Repeatedly check until the value is found or the interval is empty"
  [vct x lt-fun]
  (symbol? lt-fun)
  (if (not (some #(= x %) vct)) nil
                                (let [index-value (map first (filter #(= (second %) x) (map-indexed vector vct)))]
                                  (first index-value)
                                  )
                                )
  )





;************* TESTS DEFINITIONS *************
(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))

(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))


(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))


(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
  "Returns true if a is less than b, otherwise
   returns false. Designed to work with strings."
  [a b]
  (< (compare a b) 0))

(deftest test-binary-search
  (is (nil? (binary-search [] 5 <)))
  (is (= 3 (binary-search small-list 16 <)))
  (is (= 0 (binary-search small-list 4 <)))
  (is (= 5 (binary-search small-list 42 <)))
  (is (nil? (binary-search small-list 7 <)))
  (is (nil? (binary-search small-list 2 <)))
  (is (nil? (binary-search small-list 99 <)))
  (is (= 17 (binary-search big-list 43 <)))
  (is (= 0 (binary-search big-list 0 <)))
  (is (= 39 (binary-search big-list 99 <)))
  (is (nil? (binary-search big-list 12 <)))
  (is (nil? (binary-search big-list -1 <)))
  (is (nil? (binary-search big-list 100 <)))
  (is (= 5 (binary-search animals "pig" str<)))
  (is (= 0 (binary-search animals "dog" str<)))
  (is (= 11 (binary-search animals "tiger" str<)))
  (is (nil? (binary-search animals "elephant" str<)))
  (is (nil? (binary-search animals "alligator" str<)))
  (is (nil? (binary-search animals "unicorn" str<))))

(run-tests)