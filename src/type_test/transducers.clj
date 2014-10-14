(ns type-test.transducers
  (:use clojure.walk clojure.pprint)
  (:require [clojure.core.typed :as t]))


;;type Reducer a r = r -> a -> r
(t/defalias ReducingFn (t/TFn [[a :variance :contravariant]
                               [r :variance :invariant]]
                              [r a -> r]))
;; type Transducer a b = forall r . Reducer a r -> Reducer b r
(t/defalias Transducer3 (t/TFn [[a :variance :covariant]
                               [b :variance :contravariant]
                               [r :variance :invariant]]
 [;(ReducingFn a r) -> (ReducingFn b r)
  [r a -> r] -> [r b -> r]
  ]))

(t/defalias Transducer3 (t/TFn [[a :variance :covariant]
                               [b :variance :contravariant]
                               [r :variance :invariant]]
 [(ReducingFn a r) -> (ReducingFn b r)
  ;;[r a -> r] -> [r b -> r]
  ]))


(t/defalias Transducer (t/TFn [[a :variance :covariant]
                               [b :variance :contravariant]]
                              (t/All [r] [(ReducingFn a r) -> (ReducingFn b r)])))

(t/ann double2 (Transducer t/Int t/Int))
(defn double2 [reduction-function]
  (fn [result input]
    (reduction-function result (* 2 input))))

(t/ann double2-bad (Transducer t/Int t/Int))
(defn double2-bad [reduction-function]
  (fn [result input]
    (str (reduction-function result (* 2 input)))))

(t/ann double3 (Transducer3 t/Int t/Int t/Int))
(defn double3 [reduction-function]
  (fn [result input]
    (reduction-function result (* 2 input))))

(t/ann double3-bad (Transducer3 t/Int t/Int t/Int))
(defn double3-bad [reduction-function]
  (fn [result input]
    (str  (reduction-function result (* 2 input)))))

(t/ann ^:no-check s->i [String -> Integer])
(t/defn           s->i [s] (Integer/parseInt s))

(t/ann parse3 (Transducer3 t/Int t/Str t/Int))
(defn parse3 [reduction-function]
  (fn [result input]
    (reduction-function result (s->i input))))

(t/ann parse3 (Transducer3 t/Int t/Str t/Int))
(defn parse3 [reduction-function]
  (fn [result input]
    (reduction-function result (s->i input))))

(t/ann parse2 (Transducer t/Int t/Str))
(defn parse2 [reduction-function]
  (fn [result input]
    (reduction-function result (s->i input))))

(t/ann plus (ReducingFn t/Int t/Int))
(def plus +)

(t/ann ^:no-check compt (t/All [a b c] (t/IFn [(Transducer a b) (Transducer c a)  ->
                                               (Transducer c b)])))
(def compt comp)

(t/ann ^:no-check compi (t/All [a]

                               (t/IFn [(Transducer a a) (Transducer a a) -> (Transducer a a)])))
(def compi comp)

(t/ann ^:no-check kwlen (t/IFn [t/Keyword -> t/Int]))
(defn kwlen [x] (count (str x)))

(t/ann ^:no-check printi (t/IFn [t/Int -> t/Str]))
(def printi str)

;(t/ann )

#_(t/ann ^:no-check incthen (t/All [a [aa :< a :> a]] (t/IFn [(t/IFn [t/Int -> a]) -> (t/IFn [t/Int -> aa])])))
(t/ann ^:no-check incthen (t/All [a] (t/IFn [(t/IFn [t/Int -> a]) -> (t/IFn [t/Int -> a])])))
(defn incthen [fun] (fn [i] (fun (inc i))))

(t/ann ^:no-check parsedthen (t/All [a] (t/IFn [[Double -> a] -> [t/Str -> a]])))
(defn parsedthen [fun] (fn [s] (fun (Double/parseDouble s))))

(t/ann ^:no-check kwlenthen (t/All [a] (t/IFn [[t/Int -> a] -> [t/Keyword -> a]])))
(defn kwlenthen [fun] (fn [kw] (fun (count (str kw)))))

(t/ann ^:no-check printithen (t/All [a] (t/IFn [[t/Str -> a] -> [t/Int -> a]])))
(defn printithen [fun] (fn [i] (fun (str i))))

(t/ann ^:no-check printdthen (t/All [a] (t/IFn [[t/Str -> a] -> [Double -> a]])))
(defn printdthen [fun] (fn [i] (fun (str i))))

(t/ann ^:no-check strlenthen (t/All [a] (t/IFn [[t/Int -> a] -> [t/Str -> a]])))
(defn strlenthen [fun] (fn [s] (fun (count s))))


;; (t/cf ( ((comp printdthen strlenthen) inc) 5.5)) ;; t/Any
;; (t/cf ( ((comp (t/inst printdthen t/Int) (t/inst strlenthen t/Int)) inc) 5.5)) ;; ==> t/Int
;; (t/cf ((comp (t/inst printdthen t/Int) (t/inst strlenthen t/Int)) inc)) ;; [java.lang.Double -> t/Int]
;; (t/cf (comp (t/inst printdthen t/Int) (t/inst strlenthen t/Int))) ;; [[t/Int -> t/Int] -> [java.lang.Double -> t/Int]]





(t/ann ^:no-check comp2fa (t/All [a] (t/IFn [  [[a -> a] -> [a -> a]]
                                               [[a -> a] -> [a -> a]]   ->
                                               [[a -> a] -> [a -> a]] ])))
(def comp2fa comp)


(t/ann ^:no-check comp2fh (t/All [a b c] (t/IFn [  [[a -> a] -> [b -> b]]
                                                  [[c -> c] -> [b -> b]]   ->
                                                  [[c -> c] -> [b -> b]] ])))

(t/ann ^:no-check comp2f (t/All [a b c d] (t/IFn [  [[a -> b] -> [c -> b]]
                                                    [[c -> b] -> [a -> b]]   ->
                                                    [[c -> d] -> [b -> b]] ])))
(def comp2fh comp)
#_(t/cf (comp2fh incthen incthen))



(t/ann ^:no-check comp2 (t/All [a b c ] (t/IFn [[a -> b] [c -> a] -> [c -> b]])) )
(def comp2 comp)
#_(t/cf (comp2 incthen incthen))  ;; ==> [[t/Int -> t/Any] -> [t/Int -> t/Any]]
#_(t/cf ((comp (t/inst incthen t/Str) (t/inst incthen t/Str)) str)) ;; 

(t/ann ^:no-check comp22 (t/All [a b c [aa :< a :> a] [bb :< b :> b] [cc :< c :> c]]
                               (t/IFn [[a -> b] [c -> aa] -> [cc -> bb]])) )
(def comp22 comp)


(t/ann ^:no-check comp2 (t/All [a b c ] (t/IFn [[a -> b] [c -> b] -> [c -> b]])) )


;; (t/cf (reduce ((comp (t/inst parse2 t/Int) (t/inst double2 t/Int)) +) 0 ["1" "2" "3"]))
;; (t/U Short Byte Integer BigInteger Long BigInt)




(comment

  (t/cf (t/fn [x :- (t/IFn [t/Int -> t/Int])] :- (t/IFn [t/Int -> t/Int]) (incthen (incthen x))))
  
  (t/cf (comp2 incthen s->i))


)


(defn -main []
  (println  (reduce (double2 plus) 0 [1 2 3]))
  (println  (reduce (double3 plus) 0 [1 2 3]))
  #_(println (reduce (double3-bad plus) 0 [1 2 3]))



  (println  (reduce (parse3  plus) 0 ["1" "2" "3"]))

  ;; correct failures
  ;; (println  (reduce (parse3 plus) 0 [1 2 3])) ;; fails
  ;; Domains:
  ;; [a c -> (t/U (Reduced a) a)] a (t/Option (clojure.lang.Seqable c))
  ;; Arguments:
  ;; (ReducingFn t/Str t/Int) (t/Val 0) (t/HVec [(t/Val 1) (t/Val 2) (t/Val 3)])
  ;;(println  (reduce (parse2 plus) 0 [1 2 3])) ;; fails
  ;;Domains:
  ;;[a c -> (t/U (Reduced a) a)] a (t/Option (clojure.lang.Seqable c))
  ;;Arguments:
  ;;(ReducingFn t/Str (t/U Short Byte Integer BigInteger Long BigInt)) (t/Val 0) (t/HVec [(t/Val 1) (t/Val 2) (t/Val 3)])


)

