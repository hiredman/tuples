(ns tuples.test.core
  (:use [tuples.core])
  (:use [clojure.test]))

(deftest t-tuples
  (doseq [elements (take-while seq (iterate pop (vec (range 0 10))))
          :let [tup (apply tuple elements)]
          element elements]
    (are [x y] (= x y)
         element ((resolve (symbol (str "get" element))) tup)
         [element element] (.entryAt tup element))
    (if (zero? (count tup))
      (is (empty? tup))
      (is (not (empty? tup)))))
  (doseq [elements (take-while seq (iterate pop (vec (range 0 9))))
          :let [tup (apply tuple elements)]]
    (is (thrown? Exception ((resolve (symbol (str "get" (count elements))))
                            tup))))
  (is (= [:a :b] (tuple :a :b)))
  (is (= 1 (.get (tuple 4 5 3 1 2) 3)))
  (let [x {:a 1}
        tup (tuple 1)]
    (is (= {} (meta tup)))
    (is (= x (meta (with-meta tup x)))))
  (is (tuple? (tuple 1 2 3)))
  (is (every? true? (map tuple? (repeat 5 (tuple 1 2 3)))))
  (is (not (tuple? 10)))
  (is (= 3 (.lastIndexOf (tuple 4 4 4 4) 4)))
  (is (.contains (tuple [:a :b :c :d]) 1))
  (is (not (.contains (tuple [:a :b]) 2))))
