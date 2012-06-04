(ns tuples.bitset
  (:refer-clojure :exclude [and get set or])
  (:require [clojure.core :as cc]))

(set! *unchecked-math* true)

(set! *warn-on-reflection* true)

(defprotocol BitSet
  (and [bs1 bs2])
  (andNot [bs1 bs2])
  (cardinality [bs])
  (get [bs idx])
  (subset [from-index to-index])
  (clear
    [bs idx]
    [bs from-index to-index])
  (set
    [bs index]
    [bs from-index to-index])
  (flip
    [bs index]
    [bs from-index to-index])
  (intersects [bs1 bs2])
  (or [bs1 bs2])
  (xor [bs1 bs2])
  (to-bytes [bs])
  (capacity [bs]))

(defmacro mask [idx & operation]
  `(case (long ~idx)
     ~@(for [i (range 64)
             x [i
                `(let [~'m ~(bit-shift-left 1 i)]
                   ~@operation)]]
         x)))

(defmacro shift [idx thing]
  `(case (long ~idx)
     ~@(for [i (range 64)
             x [i `(bit-shift-right ~thing ~i)]]
         x)))

(defn generic-bs-equals [bs1 bs2]
  (let [n (max (capacity bs1) (capacity bs2))]
    (loop [i 0]
      (if (= i n)
        true
        (if (= (get bs1 i)
               (get bs2 i))
          (recur (inc i))
          false)))))

(declare bitset)

(defmacro declare-bit-sets [n]
  `(declare ~@(for [n (range 1 (eval n))]
                (symbol (str "bitset" n)))))

(def bit-set-limit 10)

(declare-bit-sets bit-set-limit)

(defmacro make-long-bitset [n]
  (let [fields (repeatedly n gensym)
        this-class (symbol (str "LongBitSet" n))
        padded-fields (vec (concat fields
                                   (repeat (- bit-set-limit n) 0)))]
    `(do
       (deftype ~this-class ~(vec (for [name fields]
                                    (with-meta name {:tag 'long})))
         BitSet
         (set [~'bs ~'idx]
           (cond
             ~@(for [i (range n bit-set-limit)
                     item [`(< ~'idx ~(* i 64))
                           `((fn []
                               (case (long (quot ~'idx 64))
                                 ~@(for [quo (range bit-set-limit)
                                         item [quo
                                               `(~@(if (= i n)
                                                     ['new this-class]
                                                     [(symbol (str "bitset" i))])
                                                 ~@(take i (assoc padded-fields
                                                             quo `(mask (mod ~'idx 64)
                                                                        (bit-or ~(cc/get padded-fields (dec i) 0)
                                                                                ~'m)))))]]
                                     item))))]]
                 item)
             :else (throw (UnsupportedOperationException.
                           (str ~'bs " " ~'idx)))))
         (get [~'bs ~'idx]
           (cond
             ~@(for [i (range n bit-set-limit)
                     item [`(< ~'idx ~(* i 64))
                           `((fn []
                               (case (long (quot ~'idx 64))
                                 ~@(for [quo (range bit-set-limit)
                                         item [quo
                                               `(let [m# (mask (mod ~'idx 64)
                                                               (bit-and ~(cc/get padded-fields (dec i)) ~'m))]
                                                  (not (zero? (shift (mod ~'idx 64) m#))))]]
                                     item))))]]
                 item)
             :else false))
         (capacity [_#] ~(* n 64))
         (and [~'bs1 ~'bs2]
           (if (instance? ~this-class ~'bs2)
             (new ~this-class ~@(for [field fields]
                                  `(bit-and ~field (. ~(with-meta 'bs2 {:tag this-class}) ~field))))
             (let [m# (max (count ~'bs1)
                           (count ~'bs2))]
               (case m#
                 ~@(for [c (range 1 10)
                         item [c `(~(symbol (str "bitset" c))
                                   ~@(for [fnn (range c)]
                                       `(bit-and (nth ~'bs1 ~c 0)
                                                 (nth ~'bs2 ~c 0))))]]
                     item)))))
         clojure.lang.Indexed
         (nth [t# i#]
           (.nth t# i# nil))
         (nth [_ i# not-found#]
           (case i#
             ~@(for [[idx v] (map-indexed vector fields)
                     item [idx v]]
                 item)
             not-found#))
         clojure.lang.Counted
         (count [_]
           ~n)
         Object
         (toString [_]
           (.trim (format ~(apply str (repeat n "0x%s "))
                          ~@(for [f fields]
                              `(Long/toHexString ~f)))))
         (equals [~'bs1 ~'bs2]
           (boolean (generic-bs-equals ~'bs1 ~'bs2))))
       ~(let [args (repeatedly n gensym)]
          `(defn ~(symbol (str "bitset" n)) [~@args]
             (if (cc/and (zero? ~(last args))
                      (> ~n 1))
               (~(symbol (str "bitset"
                              (if (= 1 n)
                                1
                                (dec n)))) ~@(butlast args))
               (new ~this-class ~@args)))))))

(defmacro make-bit-sets [n]
  `(do
     ~@(for [n (range 1 (eval n))]
         `(make-long-bitset ~n))))

(make-bit-sets bit-set-limit)

(defn bitset
  ([]
     (bitset 0))
  ([l]
     (bitset1 l)))
