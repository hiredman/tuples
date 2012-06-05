(ns tuples.test.bitset
  (require [tuples.bitset :as bs]
           [clojure.test :refer :all]))

(deftest t-bitset
  (let [bs (bs/bitset)]
    (is (= 64 (bs/capacity bs)))
    (is (= 1 (count bs)))
    (is (= 3 (count (bs/set bs 128))))
    (is (= (str (reduce #(bs/set %1 %2)
                        (bs/bitset)
                        (range 128)))
           "0xffffffffffffffff 0xffffffffffffffff"))
    (is (= true (bs/get (bs/set bs 1) 1)))
    (is (= false (bs/get bs 1)))))

(deftest t-get-set
  (let [bs (bs/bitset)]
    (dotimes [i 500]
      (is (= true (bs/get (bs/set bs i) i))))))

(deftest t-bytes
  (is (bs/get (bs/read-bitset (bs/to-bytes (bs/set (bs/bitset) 300))) 300)))
