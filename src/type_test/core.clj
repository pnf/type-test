(ns type-test.core
  (:use clojure.walk clojure.pprint)
  (:require [clojure.core.typed :as t]))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
