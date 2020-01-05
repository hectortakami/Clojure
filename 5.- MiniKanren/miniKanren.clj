(require '[clojure.core.logic :as logic])

(logic/defne
  lasto
  "Logical function that succeeds if
  the last element of lst is x"
  [lst x]
  ( [ [x] x ] )
  ( [ [head . tail] x ] (lasto tail x))

  )

(logic/defne
  dupo
  "Logical function that succeeds if every
   element of lst is duplicated in the result"
  [lst result]
  ([ [] [] ])
  ([[head . tail]
    [head head . temp]] (dupo tail temp))

  )

(logic/defne
  reverso
  "Logical function that succeeds if reverse
  of lst is result."
  [lst result]
  ([ [] [] ])
  ([[ head . tail ] result]
   (logic/fresh [temp]
                (reverso tail temp)
                (logic/appendo temp [head] result)))

  )


