;----------------------------------------------------------
; Problem Set #6
; Date: November 28, 2019.
; Author:
;          A01377647 Hector Takami
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.fd :as fd])

(declare oddsizeo)

(logic/defne
  reverso
  "Logical function that succeeds if reverse
  of lst is result."
  [lst result]
  ([[] []])
  ([[head . tail] result]
   (logic/fresh [temp]
                (reverso tail temp)
                (logic/appendo temp [head] result)))

  )

(logic/defne
  removeo
  "This logic function succeeds if it’s able to remove
  the first occurrence of x from lst giving result"
  [x lst result]

  ([x [x . tail] tail])
  ([x [head . tail] result]
   (logic/!= x head)
   (logic/fresh [temp]
                (removeo x tail temp)
                (logic/appendo [head] temp result)))
  )

(logic/defne
  palindromeo
  "This logic function succeeds if lst is a palindrome
  list (it reads the same from left to right than from
  right to left)"
  [lst]
  ([lst]
   (reverso lst lst))
  )


(logic/defne
  rotateo
  "This logic function succeeds when lst is rotated left
  one position giving result. In other words, the first
  element of lst becomes the last element of result."
  [lst result]
  ([[head . tail] result]
   (logic/appendo tail [head] result))
  )

(logic/defne
  evensizeo
  "These two logic functions should be defined in a mutually
  recursive fashion. That is, each one should be defined in
  terms of the other one. These functions succeed if the number
  of elements in lst is even or odd, respectively."
  [lst]
  ([[]])
  ([[head . tail]]
   (oddsizeo tail))
  )


(logic/defne
  oddsizeo
  "These two logic functions should be defined in a mutually
  recursive fashion. That is, each one should be defined in
  terms of the other one. These functions succeed if the number
  of elements in lst is even or odd, respectively."
  [lst]
  ([[head . tail]]
   (evensizeo tail))
  )


(logic/defne
  splito-b
  "Helper function for spliting the values"
  [lst a b]
  ([[] [] []])
  ([[head . tail] a b]
   (logic/fresh [temp]
                (splito tail a temp)
                (logic/conso head temp b))))

(logic/defne
  splito
  "These two logic functions should be defined in a mutually
  recursive fashion. That is, each one should be defined in
  terms of the other one. These functions succeed if the number
  of elements in lst is even or odd, respectively."
  [lst a b]
  ([[] [] []])
  ([[head . tail] a b]
   (logic/fresh [temp]
                (splito-b tail temp b)
                (logic/conso head temp a)))
  )

(logic/defne
  equalo
  "This logic function succeeds if all the elements contained
  in lst unify to the same value, otherwise fails. The function
  should always succeed if lst is empty or has only one element"
  [lst]
  ([[]])
  ([[_]])
  ([[head head . tail]]
   (logic/fresh [temp]
                (logic/appendo [head] tail temp)
                (equalo temp)))
  )

(logic/defne
  counto
  "This logic function unifies result with the number of
  elements contained in lst"
  [lst result]
  ([[] 0])
  ([[head . tail] result]
   (logic/fresh [temp]
                (counto tail temp)
                (fd/+ 1 temp result)))
  )

(logic/defne
  facto
  "This logic function succeeds if the factorial of n is
  equal to result. Mathematically, the factorial of an
  integer number n greater or equal to zero can be defined
  recursively."
  [n result]
  ([0 1])
  ([n result]
   (logic/fresh [temp sum]
                (fd/+ 1 sum n)
                (facto sum temp)
                (fd/* n temp result)))
  )

(logic/defne
  powo
  "This logic function succeeds if base raised to the
  power exp is equal to result"
  [base exp result]
  ([n 0 1])
  ([n b result]
   (logic/fresh [temp expo]
                (fd/in temp (fd/interval 0 100))
                (fd/in expo (fd/interval 0 100))
                (fd/+ 1 expo b)
                (powo n expo temp)
                (fd/* n temp result)))
  )

(logic/defne
  rangeo
  "This logic function unifies result with a sequence
   of incremental integers from start to end (inclusive)"
  [start end result]
  ([x x [x]])

  ([start end result]
   (fd/> start end)
   (logic/== [] result))

  ([start end result]
   (fd/>= end start)
   (logic/fresh [temp newEnd]
                (fd/- end 1 newEnd)
                (rangeo start newEnd temp)
                (logic/appendo temp [end] result)))
  )



(deftest test-rangeo
  (is (= [[3 4 5 6 7 8 9 10]]
         (logic/run 1 [q]
                    (rangeo 3 10 q))))
  (is (= [[7]]
         (logic/run 1 [q]
                    (rangeo 7 7 q))))
  (is (= [[]]
         (logic/run 1 [q]
                    (rangeo 10 1 q))))
  (is (= [6]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 1 10))
                    (rangeo 2 q [2 3 4 5 6]))))
  (is (= [[2 6]]
         (logic/run 1 [q1 q2]
                    (fd/in q1 q2 (fd/interval 1 10))
                    (rangeo q1 q2 [2 3 4 5 6]))))
  (is (= #{[]
           [1] [1 2] [1 2 3] [1 2 3 4]
           [2] [2 3] [2 3 4]
           [3] [3 4]
           [4]}
         (set
           (logic/run* [q]
                       (logic/fresh [start end]
                                    (fd/in start end (fd/interval 1 4))
                                    (rangeo start end q))))))
  )

(deftest test-powo
  (is (= [:yes]
         (logic/run 1 [q]
                    (powo 3 2 9)
                    (logic/== q :yes))))
  (is (= [32]
         (logic/run 1 [q]
                    (powo 2 5 q))))
  (is (= [5]
         (logic/run 1 [q]
                    (powo q 2 25))))
  (is (= [3]
         (logic/run 1 [q]
                    (powo 2 q 8))))
  (is (= [1]
         (logic/run 1 [q]
                    (powo q q q))))
  (is (= #{[64 1] [8 2] [4 3] [2 6]}
         (set
           (logic/run* [a b]
                       (powo a b 64)))))
  (is (= '[_0]
         (logic/run 1 [q]
                    (powo q 0 1))))
  (is (= (set (range 101))
         (set
           (logic/run* [q]
                       (fd/in q (fd/interval 0 100))
                       (powo q 1 q))))))

(deftest test-facto
  (is (= [1]
         (logic/run 1 [q]
                    (facto 0 q))))
  (is (= [1]
         (logic/run 1 [q]
                    (facto 1 q))))
  (is (= [720]
         (logic/run 1 [q]
                    (facto 6 q))))
  (is (= [2432902008176640000]
         (logic/run 1 [q]
                    (facto 20 q))))
  (is (= [0 1]
         (logic/run 2 [q]
                    (facto q 1))))
  (is (= [5]
         (logic/run 1 [q]
                    (facto q 120))))
  (is (= [10]
         (logic/run 1 [q]
                    (facto q 3628800))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (facto 4 24)
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (facto 15 1307674368000)
                    (logic/== q :yes))))
  (is (= [[0 1]
          [1 1]
          [2 2]
          [3 6]
          [4 24]
          [5 120]
          [6 720]
          [7 5040]
          [8 40320]
          [9 362880]]
         (logic/run 10 [n r]
                    (facto n r)))))

(deftest test-counto
  (is (= [0]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [] q))))
  (is (= [1]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [:a] q))))
  (is (= [2]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [:a :b] q))))
  (is (= [3]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto [:a :b :c] q))))
  (is (= [10]
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto (repeat 10 :x) q))))
  (is (= '([_0])
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto q 1))))
  (is (= '([_0 _1 _2 _3 _4])
         (logic/run 1 [q]
                    (fd/in q (fd/interval 0 10))
                    (counto q 5))))
  (is (= '([[] 0]
           [(_0) 1]
           [(_0 _1) 2]
           [(_0 _1 _2) 3]
           [(_0 _1 _2 _3) 4]
           [(_0 _1 _2 _3 _4) 5]
           [(_0 _1 _2 _3 _4 _5) 6])
         (logic/run 7 [q1 q2]
                    (fd/in q1 q2 (fd/interval 0 10))
                    (counto q1 q2))))
  )

(deftest test-equalo
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [:x])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [:x :x])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (equalo [:x :x :x :x :x])
                    (logic/== q :yes))))
  (is (= [:x]
         (logic/run 1 [q]
                    (equalo [:x :x q :x]))))
  (is (= '[_0]
         (logic/run 1 [q]
                    (equalo [q q q q q q]))))
  (is (= '([_0 _0 _0 _0 _0])
         (logic/run 1 [q1 q2 q3 q4 q5]
                    (equalo [q1 q2 q3 q4 q5]))))
  (is (= []
         (logic/run 1 [q]
                    (equalo [:x :y])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q1 q2]
                    (equalo [q1 q1 q2 q1 q1])
                    (logic/!= q1 q2))))
  (is (= '([]
           [_0]
           [_0 _0]
           [_0 _0 _0]
           [_0 _0 _0 _0]
           [_0 _0 _0 _0 _0]
           [_0 _0 _0 _0 _0 _0])
         (logic/run 7 [q]
                    (equalo q)))))

(deftest test-splito
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [] [] [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a] [:a] [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a :b] [:a] [:b])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a :b :c :d :e :f]
                            [:a :c :e]
                            [:b :d :f])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (splito [:a :b :c :d :e :f :g]
                            [:a :c :e :g]
                            [:b :d :f])
                    (logic/== q :yes))))
  (is (= [[[:a :c :e] [:b :d :f]]]
         (logic/run 1 [q1 q2]
                    (splito [:a :b :c :d :e :f] q1 q2))))
  (is (= [[:a :b :c :d :e :f :g]]
         (logic/run 1 [q]
                    (splito q [:a :c :e :g] [:b :d :f]))))
  (is (= '[[[] [] []]
           [[_0] [_0] []]
           [[_0 _1] [_0] [_1]]
           [[_0 _1 _2] [_0 _2] [_1]]
           [[_0 _1 _2 _3] [_0 _2] [_1 _3]]
           [[_0 _1 _2 _3 _4] [_0 _2 _4] [_1 _3]]
           [[_0 _1 _2 _3 _4 _5] [_0 _2 _4] [_1 _3 _5]]]
         (logic/run 7 [q1 q2 q3]
                    (splito q1 q2 q3))))
  )

(deftest test-evensizeo-oddsizeo
  (is (= [:yes]
         (logic/run 1 [q]
                    (evensizeo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (oddsizeo [:x])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (evensizeo [:x])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (oddsizeo [])
                    (logic/== q :yes))))

  (is (= [:yes]
         (logic/run 1 [q]
                    (evensizeo [:a :b :c :d :e :f])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (oddsizeo [:a :b :c :d :e])
                    (logic/== q :yes))))

  (is (= '[[]
           [_0 _1]
           [_0 _1 _2 _3]
           [_0 _1 _2 _3 _4 _5]
           [_0 _1 _2 _3 _4 _5 _6 _7]]
         (logic/run 5 [q]
                    (evensizeo q))))
  (is (= '[[_0]
           [_0 _1 _2]
           [_0 _1 _2 _3 _4]
           [_0 _1 _2 _3 _4 _5 _6]
           [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
         (logic/run 5 [q]
                    (oddsizeo q))))
  )

(deftest test-rotateo
  (is (= [:yes]
         (logic/run 1 [q]
                    (rotateo [:a :b :c :d :e]
                             [:b :c :d :e :a])
                    (logic/== q :yes))))

  (is (= []
         (logic/run 1 [q]
                    (rotateo [:a :b :c :d :e]
                             [:a :b :c :d :e])
                    (logic/== q :yes))))

  (is (= []
         (logic/run 1 [q]
                    (rotateo [] q))))

  (is (= [[:a]]
         (logic/run 1 [q]
                    (rotateo [:a] q))))
  (is (= [[:b :c :d :e :a]]
         (logic/run 1 [q]
                    (rotateo [:a :b :c :d :e] q))))
  (is (= [[:e :a :b :c :d]]
         (logic/run 1 [q]
                    (rotateo q [:a :b :c :d :e]))))

  (is (= '[[[_0] [_0]]
           [[_0 _1] [_1 _0]]
           [[_0 _1 _2] [_1 _2 _0]]
           [[_0 _1 _2 _3] [_1 _2 _3 _0]]
           [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
           [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
           [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
         (logic/run 7 [q1 q2]
                    (rotateo q1 q2))))

  )

(deftest test-palindromeo
  (is (= [:yes]
         (logic/run 1 [q]
                    (palindromeo [])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (palindromeo [:a])
                    (logic/== q :yes))))
  (is (= [:yes]
         (logic/run 1 [q]
                    (palindromeo [:a :b :c :b :a])
                    (logic/== q :yes))))
  (is (= []
         (logic/run 1 [q]
                    (palindromeo [:a :b :c :d])
                    (logic/== q :yes))))
  (is (= '[[]
           [_0]
           [_0 _0]
           [_0 _1 _0]
           [_0 _1 _1 _0]
           [_0 _1 _2 _1 _0]
           [_0 _1 _2 _2 _1 _0]]
         (logic/run 7 [q]
                    (palindromeo q))))

  )

(deftest test-removeo
  (is (= [[:b :c :d :e]]
         (logic/run 1 [q]
                    (removeo :a [:a :b :c :d :e] q))))
  (is (= [[:a :b :d :e]]
         (logic/run 1 [q]
                    (removeo :c [:a :b :c :d :e] q))))
  (is (= [:d]
         (logic/run 1 [q]
                    (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (is (= []
         (logic/run 1 [q]
                    (removeo :x [:a :b :c :d :e] q))))
  (is (= [[:x :a :b :c :d :e]
          [:a :x :b :c :d :e]
          [:a :b :x :c :d :e]
          [:a :b :c :x :d :e]
          [:a :b :c :d :x :e]
          [:a :b :c :d :e :x]]
         (logic/run 6 [q]
                    (removeo :x q [:a :b :c :d :e]))))
  (is (= [[:a [:b :c :d :e]]
          [:b [:a :c :d :e]]
          [:c [:a :b :d :e]]
          [:d [:a :b :c :e]]
          [:e [:a :b :c :d]]]
         (logic/run* [q1 q2]
                     (removeo q1 [:a :b :c :d :e] q2))))
  )


(run-tests)