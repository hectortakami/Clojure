;----------------------------------------------------------
; Problem Set #1
; Date: September 20, 2019.
; Author:
;          A01377647 Hector Takami
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
;************* FUNCTIONS DEFINITIONS *************

(defn replic
  "Takes two arguments: a list lst and an integer number n, where n ≥ 0. It returns a new list that replicates n times each element contained in lst."
  [n lst]
  (cond
    (empty? lst) (empty lst)
    (zero? n) (empty lst)
    (> n 0) (mapcat #(repeat n %) lst)
    )
  )

(defn expand
  "Takes a list lst as its argument. It returns a list where the first element of lst appears one time, the second elements appears two times, the third element appears three times, and so on"
  [lst]
  (cond
    (= (count lst) 1) lst
    :else (mapcat repeat (range 1 (+ (count lst) 1)) lst)
    )
  )

(defn insert
  "Takes two arguments: a number n and a list of numbers lst in ascending order. It returns a new list with the same elements as lst but inserting n in its corresponding place"
  [n lst]
  (let [[a b] (split-with #(>= n %) lst)]
    (concat a (list n) b))
  )

(defn my-sort
  "Takes an unordered list of numbers as an argument, and returns a new list with the same elements but in ascending order. You must use the insert function defined in the previous exercise to write my-sort. You must NOT use the predefined sort function"
  [lst]
  (loop [lst lst
         result ()]
    (if (empty? lst)
      result
      (recur (rest lst)
             (insert (first lst) result))))
  )

(defn rotate-left
  "Takes two arguments: an integer number n and a list lst. It returns the list that results from rotating lst a total of n elements to the left. If n is negative, it rotates to the right"
  [n lst]
  (if (empty? lst)
    ()
    (concat (drop (mod n (count lst)) lst) (take (mod n (count lst)) lst))))

(defn binary
  "Takes an integer n as input (assume that n ≥ 0). If n is equal to zero, it returns an empty list. If n is greater than zero, it returns a list with a sequence of ones and zeros equivalent to the binary representation of n."
  [x]
  (if (>= x 0)
    (if (zero? x)
      ()
      (concat (binary (quot x 2)) (list (rem x 2)))
      )
    )
  )

(defn prime-factors
  "Takes an integer n as input (assume that n > 0), and returns a list containing the prime factors of n in ascending order. The prime factors are the prime numbers that divide a number exactly. If you multiply all the prime factors you get the original number"
  [n]
  (loop [q n
         d 2
         result ()]
    (if (> d q)
      (reverse result)
      (if (zero? (rem q d))
        (recur (quot q d)
               d
               (cons d result))
        (recur q
               (inc d)
               result)))))

(defn gcd
  "Takes two positive integer arguments a and b as arguments, where a > 0 and b > 0. It returns the greatest common divisor (GCD) of a and b"
  [a b]
  (first (last
           (let [major (if (> a b) a b)]
             (take major
                   (iterate (fn [[a b]]
                              (cond
                                (> a b) [(- a b) b]
                                (< a b) [a (- b a)]
                                :else [a b]))
                            [a b]))))))

(defn insert-everywhere
  "Takes two arguments as input: an object x and a list lst. It returns a new list with all the possible ways in which x can be inserted into every position of lst"
  [x lst]
  (for [i (range 0 (+ 1 (count lst)))]
    (concat (take i lst) (list x) (drop i lst))))

(defn deep-reverse [lst]
  "Takes a list as its input. It returns a list with the same elements as its input but in reverse order. If there are any nested lists, these too should be reversed"
  (if (seq? lst)
    (if (empty? lst)
      ()
      (cons
        (deep-reverse (last lst))
        (deep-reverse (butlast lst))))
    lst))

(defn pack
  "Takes a list lst as its argument. If lst contains consecutive repeated elements they should be placed in separate sublists"
  [lst]
  (partition-by identity lst))

(defn compress
  "Takes a list lst as its argument. If lst contains consecutive repeated elements, they should be replaced with a single copy of the element"
  [lst]
  (map (fn [x] (first x)) (pack lst)))

(defn encode
  "Takes a list lst as its argument. Consecutive duplicates of elements in lst are encoded as vectors [n e], where n is the number of duplicates of the element e"
  ([lst] (encode lst 0))
  ([lst cnt]
   (if (empty? lst)
     ()
     (if (= (first lst) (first (rest lst)))
       (encode (rest lst) (inc cnt))
       (cons (vector (+ cnt 1) (first lst)) (encode (rest lst) 0))))))

(defn encode-modified
  "Takes a list lst as its argument. It works the same as the previous problem, but if an element has no duplicates it is simply copied into the result list"
  ([lst] (encode-modified lst 0))
  ([lst cnt]
   (if (empty? lst)
     ()
     (if (= (first lst) (first (rest lst)))
       (encode-modified (rest lst) (inc cnt))
       (if (= cnt 0)
         (cons (first lst) (encode-modified (rest lst) 0))
         (cons (vector (+ cnt 1) (first lst)) (encode-modified (rest lst) 0)))))))

(defn decode
  "Takes as its argument an encoded list lst that has the same structure as the resulting list from the previous problem. It returns the decoded version of lst"
  [lst]
  (reduce concat
          (map (fn [x] (cond
                         (vector? x) (repeat (first x) (first (rest x)))
                          :else (list x))
                 )
            lst)))



;************* TESTS DEFINITIONS *************
(deftest test-replic
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
