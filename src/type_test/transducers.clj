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
 [[r a -> r] -> [r b -> r]]))

(t/defalias Transducer3 (t/TFn [[a :variance :covariant]
                               [b :variance :contravariant]
                               [r :variance :invariant]]
 [(ReducingFn a r) -> (ReducingFn b r)
  ;;[r a -> r] -> [r b -> r]
  ]))


(t/defalias Transducer (t/TFn [[a :variance :covariant]
                               [b :variance :contravariant]]
                              (t/All [r] [[r a -> r] -> [r b -> r]])))

(t/ann double2 (Transducer t/Int t/Int))
(defn double2 [reduction-function]
  (fn [result input]
    (reduction-function result (* 2 input))))

(t/ann double2-bad (Transducer t/Int t/Int))
(defn double2-bad [reduction-function]
  (fn [result input]
    (str (reduction-function result (* 2 input)))))

(t/ann ^:no-check pow [Double Double -> Double])
(defn pow [x y] (Math/pow x y))

(t/ann root-of-2 (Transducer Double t/Int))
(defn root-of-2 [rf]
  (fn [acc in]
    (rf acc (pow 2.0 (/ 1.0 (double in))))))

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

(t/ann t-parsei (Transducer t/Int t/Str))
(defn t-parsei [rf]
  (fn [result input]
    (rf result (Integer/parseInt input))))

(t/ann t-dub (Transducer t/Int t/Int))
(defn t-dub [rf]
  (fn [result input]
    (rf result (* 2 input))))

(comment
  (t/cf (compt t-parse (t/inst t-rep t/Int)))

  (t/cf (compt (t/inst t-rep t/Str) t-parse))

  

  )

(t/ann ^:no-check fixt
       (t/All [a b] [[[t/Any a -> t/Any] -> [t/Any b -> t/Any]] ->
                     (t/All [r] [[r a -> r] -> [r b -> r]])]))
(def fixt identity)

;;(t/ann t-rep (Transducer t/Int t/Int))
;;(t/ann t-rep (Transducer t/Any t/Any))
(t/ann ^:no-check t-rep (t/All [a] (Transducer a a)))
(defn t-rep [rf]
  (fn [result input]
    (rf (rf result input) input)))

(t/ann t-repi (Transducer t/Int t/Int))
(defn t-repi [rf]
  (fn [result input]
    (rf (rf result input) input)))

(t/ann t-repn (Transducer Number Number))
(defn t-repn [rf]
  (fn [result input]
    (rf (rf result input) input)))

(t/ann t-root (Transducer Double Number))
(defn t-root [rf]
  (fn [acc in]
    (rf acc (pow 2.0 (/ 1.0 (double in))))))

;; 



#_(t/ann ^:no-check compt (t/All [a b c] (t/IFn [[[r a -> r] -> [r b -> r]]         ;; Double  String
                                               [[r c -> r] -> [r a -> r]] ->      ;; Int     Double
                                               [[r c -> r] -> [r b -> r]]])))

#_(t/ann ^:no-check compt (t/All [a b c] (t/IFn [(Transducer a b) (Transducer c a) -> (Transducer c b)])))
#_(t/ann compt TransducerComposer2)
#_(def compt comp)

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

;;  (t/cf (compt t-parse t-rep t-root))  cannot be applied
;;  (t/cf (compt t-parse (t/inst t-rep t/Int) t-root))

;; (reduce ((compt t-parse (t/inst t-rep t/Int) t-root) +) 0 ["1" "2" "3"])



#_(t/cf (reduce ((compt parse2 double2) (t/ann-form + [t/Int t/Int -> t/Int])) 0 ["1" "2"]))

;;double inc read-string

(t/ann comp1 (t/All [a b c] [[b -> c] [a -> b] -> [a -> c]]))
(defn comp1 [f1 f2]
  (fn [x] (f1 (f2 x))))

(defmacro comp* [& [f1 f2 & fs]]
  (if-not fs
    `(comp ~f1 ~f2)
    `(comp ~f1 (comp* ~f2 ~@fs))))
(defmacro compt [& tds]
  (let [its (map #(list 't/inst % 't/Any) tds)]
    `(fixt (comp* ~@its))))

#_(t/cf (reduce ((compt parse2 double2 root-of-2) (t/ann-form + [Double Double -> Double])) 0.0 ["1" "2"]))

#_(t/ann plus (ReducingFn t/Int t/Int))
(t/ann ^:no-check plusd (ReducingFn Double Double) )
(def plusd +)

#_(fn* ([x__25481__auto__]
        ((clojure.core.typed/inst-poly parse2 (quote (t/Any)))
         ((clojure.core.typed/inst-poly double2 (quote (t/Any)))
          ((clojure.core.typed/inst-poly root-of-2 (quote (t/Any)))
           x__25481__auto__)))))

(t/ann ^:no-check ink (t/All [[a]] [a -> a]))
(def ink inc)



(comment

  (t/cf (t/fn [x :- (t/IFn [t/Int -> t/Int])] :- (t/IFn [t/Int -> t/Int]) (incthen (incthen x))))
  
  (t/cf (comp2 incthen s->i))


(reduce ((comp parse2 double2) +) 0 ["1" "2"])
)

(t/ann ^:no-check  mapping (t/All [a b r] [[a -> b] -> (Transducer b a)]))
(defn mapping [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
         (rf result (f input)))
      ([result input & inputs]
         (rf result (apply f input inputs))))))


(defn -main []
  #_(println  (reduce (double2 plus) 0 [1 2 3]))
  #_(println  (reduce (double3 plus) 0 [1 2 3]))
  #_(println (reduce (double3-bad plus) 0 [1 2 3]))



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

