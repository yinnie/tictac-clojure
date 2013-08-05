(ns tictac-clojure.core-test
  (:require [clojure.test :refer :all]
            [tictac-clojure.core :refer :all]))

(deftest test-full 
    (is (= false (full? [1 2 1 0 2 1 2 1 0]))))

