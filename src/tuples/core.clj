(ns tuples.core
  (:import (clojure.lang IPersistentVector
                         Associative
                         Sequential
                         IPersistentStack
                         Reversible
                         Indexed
                         IFn
                         Counted
                         Util
                         IObj
                         IMeta
                         IPersistentCollection
                         ILookup
                         Seqable
                         IMapEntry
                         RT)
           (java.util RandomAccess List Collection
                      Map$Entry)
           (java.io Serializable)))



(defprotocol TupleAccess
  (get0 [t])
  (get1 [t])
  (get2 [t])
  (get3 [t])
  (get4 [t])
  (get5 [t])
  (get6 [t])
  (get7 [t])
  (get8 [t])
  (get9 [t]))

(defprotocol ITuple
  (tuple? [obj]))

(extend-type Object
  ITuple
  (tuple? [obj]
    (extend-type (class obj)
      ITuple
      (tuple? [obj] false))
    false))

(defmacro tuple-for [n & {:keys [class-name map-entry-class]}]
  (let [class-name (symbol (or class-name (format "Tuple%s" n)))
        fields (into {} (for [i (range n)]
                          [i (symbol (format "e%s" (inc i)))]))]
    `(do
       (deftype ~class-name [~@(map second (sort-by first fields)) md#]
         ~@(when (= n 2)
             `[IMapEntry
               (key [v#]
                    ~(get fields 0))
               (val [v#]
                    ~(get fields 1))
               Map$Entry
               (getKey [v#]
                       ~(get fields 0))
               (getValue [v#]
                         ~(get fields 1))
               (setValue [v# _#]
                         (throw (UnsupportedOperationException.)))])
         ITuple
         (tuple? [obj] true)
         TupleAccess
         ~@(for [i (range 10)]
             `(~(symbol (str "get" i)) [~'t]
               ~(if (>= i n)
                  `(throw (UnsupportedOperationException.))
                  (get fields i))))
         Associative
         (containsKey [v# key#]
           (if (and (Util/isInteger key#)
                    (> ~(inc n) key# ~(dec n)))
             true
             false))
         (entryAt [v# key#]
           (if (Util/isInteger key#)
             (let [key# (int key#)]
               (case key#
                     ~@(for [[idx fn] fields
                             itm [idx (if map-entry-class
                                        `(new ~(symbol map-entry-class)
                                              ~idx ~fn {})
                                        (first {idx fn}))]]
                         itm)
                     nil))
             nil))
         (assoc [v# key# val#]
           (if (Util/isInteger key#)
             (.assocN v# key# val#)
             (throw (IllegalArgumentException. "Key must be an integer"))))
         Sequential ; Marker
         IPersistentStack
         (peek [v#]
           ~(second (last (sort-by first fields))))
         (pop [~'v]
           ~(cond
             (zero? n)
             `(throw (UnsupportedOperationException.))
             (= 1 n)
             `(apply tuple nil)
             :else
             `(with-meta
                (apply tuple (list ~@(butlast
                                      (map second
                                           (sort-by first fields)))))
                (meta ~'v))))
         Reversible
         (rseq [v#]
           (throw (UnsupportedOperationException.)))
         Indexed
         (nth [v# i#]
           (.nth v# i# nil))
         (nth [v# i# not-found#]
           (if (Util/isInteger i#)
             (let [i# (int i#)]
               (case i#
                     ~@(for [[idx n] fields
                             itm [idx n]]
                         itm)
                     not-found#))
             not-found#))
         Counted
         (count [v#] ~n)
         IPersistentVector
         (length [v#] ~n)
         (assocN [v# i# ~'val]
           (let [i# (int i#)]
             (case i#
                   ~@(for [[idx n] fields
                           itm [idx `(new ~class-name
                                          ~@(for [[idx2 fn]
                                                  (sort-by first fields)]
                                              (if (= idx idx2)
                                                'val
                                                fn))
                                          {})]]
                       itm)
                   ~(inc n) (.cons v# ~'val)
                   (throw (IllegalArgumentException.
                           (str "Key " i# " is out of range for a "
                                ~n "-tuple"))))))
         (cons [v# o#]
           (throw (UnsupportedOperationException.)))
         IFn
         (invoke [v# arg1#]
           (let [arg1# (int arg1#)]
             (if (Util/isInteger arg1#)
               (case arg1#
                     ~@(for [[idx n] fields
                             itm [idx n]]
                         itm)
                     nil)
               (throw (IllegalArgumentException. "Key must be integer")))))
         IObj
         (withMeta [v# m#]
           (new ~class-name ~@(map second (sort-by first fields)) m#))
         IMeta
         (meta [v#] (if md# md# {}))
         IPersistentCollection
         (empty [v#] [])
         (equiv [v1# v2#]
           (if (instance? Sequential v2#)
             (if (= (count v2#) ~n)
               (zero? (.compareTo v1# v2#))
               false)
             false))
         Seqable
         (seq [v#]
           (list ~@(map second (sort-by first fields))))
         ILookup
         (valAt [v# key#]
           (.nth v# key# nil))
         (valAt [v# key# not-found#]
           (.nth v# key# not-found#))
         ;;JAVA
         RandomAccess ; Marker
         List
         (lastIndexOf [v# ~'obj]
           (cond
            ~@(for [i (range (dec n) 0 -1)
                    itm [`(= ~(get fields i) ~'obj) i]]
                itm)
            :else -1))
         (subList [v# fidx# tidx#]
           (throw (UnsupportedOperationException.)))
         (set [v# idx# o#]
           (throw (UnsupportedOperationException.)))
         (listIterator [v#]
           (throw (UnsupportedOperationException.)))
         (listIterator [v# idx#]
           (throw (UnsupportedOperationException.)))
         (add [v# obj# idx#]
           (throw (UnsupportedOperationException.)))
         (get [v# idx#]
           (.nth v# idx#))
         (indexOf [v# ~'obj]
           (cond
            ~@(for [i (range n)
                    itm [`(= ~(get fields i) ~'obj) i]]
                itm)
            :else -1))
         Collection
         (isEmpty [v#] ~(zero? n))
         (contains [v# k#]
           (and (Util/isInteger k#)
                (> ~(inc n) (long k#))
                (> (long k#) -1)))
         (size [v#]
           ~n)
         (toArray [v#]
           (RT/seqToArray (.seq v#)))
         (toArray [v# obj#]
           (throw (UnsupportedOperationException.)))
         (addAll [v# collection#]
           (throw (UnsupportedOperationException.)))
         (iterator [v#]
           (.iterator (.seq v#)))
          (removeAll [v# c#]
            (throw (UnsupportedOperationException.)))
          (retainAll [v# c#]
            (throw (UnsupportedOperationException.)))
         (add [v# obj#]
           (throw (UnsupportedOperationException.))) 
         (clear [v#]
           (throw (UnsupportedOperationException.)))
         Comparable
         (compareTo [v1# v2#]
           (let [^IPersistentVector v2# v2#]
             (cond
              (> (count v2#) ~n)
              -1
              (< (count v2#) ~n)
              1
              :else
              (loop [i# 0]
                (if (> ~n i#)
                  (let [c# (Util/compare (.nth v1# i#) (.nth v2# i#))]
                    (if (zero? c#)
                      (recur (inc i#))
                      c#))
                  0)))))
         Serializable ; Marker
         )
       (defmethod tuple ~n [~@(map second(sort-by first fields))]
         (new ~class-name ~@(map second(sort-by first fields)) {})))))

(defmacro generate-tuples []
  (let [ns (ns-name *ns*)]
    `(do
       (defmulti
         ^{:inline (fn [& ~'args]
                     (let [class-name# (symbol (format "%s.Tuple%s"
                                                       '~ns
                                                       (count ~'args)))]
                       `(new ~class-name# ~@~'args nil)))}
         tuple (comp count list))
       ;; Screw it, using Tuple2 for a mapentry was just too much of a
       ;; pain, kept getting slimed
       (tuple-for 2 :class-name "TMapEntry")
       ~@(for [i (range 11)]
           `(tuple-for ~i :map-entry-class "TMapEntry")))))

(generate-tuples)
