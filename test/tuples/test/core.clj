(ns tuples.test.core
  (:use [tuples.core :only [generate-tuples tuple?]])
  (:use [clojure.test])
  (:require [tuples.core :as tuples]))

(generate-tuples)

(deftest t-tuples
  (doseq [elements (take-while seq (iterate pop (vec (range 0 10))))
          :let [tup (apply tuple elements)]
          element elements]
    (are [x y] (= x y)
         element ((resolve (symbol "tuples" (str "get" element))) tup)
         [element element] (.entryAt tup element))
    (if (zero? (count tup))
      (is (empty? tup))
      (is (not (empty? tup)))))
  (doseq [elements (take-while seq (iterate pop (vec (range 0 9))))
          :let [tup (apply tuple elements)]]
    (is (thrown? Exception ((resolve (symbol "tuples"
                                             (str "get" (count elements))))
                            tup))))
  (let [x {:a 1}
        tup (tuple 1)]
    (is (= {} (meta tup)))
    (is (= x (meta (with-meta tup x)))))
  (is (tuple? (tuple 1 2 3)))
  (is (every? true? (map tuple? (repeat 5 (tuple 1 2 3)))))
  (is (not (tuple? 10)))
  (is (.contains (tuple :a :b :c :d) 1))
  (is (not (.contains (tuple :a) 2)))
  (is (thrown? IllegalArgumentException ((tuple 1 2) 1 2)))
  (is (empty? (tuple)))
  (is (not (empty? (tuple 1 2))))
  (are [x y] (= x y)
       3           (.lastIndexOf (tuple 4 4 4 4) 4)
       [:a :b]     (tuple :a :b)
       []          (tuple)
       {:a 1 :b 2} (into {} [(tuple :a 1) (tuple :b 2)])
       1           (.get (tuple 4 5 3 1 2) 3)
       [1 2 :a 4]  (assoc (tuple 1 2 3 4) 2 :a)
       "[]"        (with-out-str (pr (tuple)))
       []          (pop (tuple 1))
       1           (peek (tuple 1))
       1           (get (tuple 1) 0)))
