(ns com.wsscode.misc.coll
  (:require
    [clojure.set :as set])
  #?(:clj
     (:import
       (clojure.lang
         MapEntry))))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [fx (f x)]
            (if (contains? @seen fx)
              result
              (do (vswap! seen conj fx)
                  (rf result x)))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [fx (f x)]
                         (if (contains? seen fx)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen fx)))))))
                   xs seen)))]
     (step coll #{}))))

(defn dedupe-by
  "Returns a lazy sequence removing consecutive duplicates in coll when passed to a function f.
  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([f]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [prior @pv
                fx    (f x)]
            (vreset! pv fx)
            (if (= prior fx)
              result
              (rf result x))))))))
  ([f coll] (sequence (dedupe-by f) coll)))

(defn index-by
  "Like group by, but will keep only the last result."
  [f coll]
  (reduce
    (fn [m x]
      (assoc m (f x) x))
    {}
    coll))

(defn find-first
  "Return the first element in coll that returns true for f."
  [f coll]
  (->> coll
       (filter f)
       first))

(def sconj (fnil conj #{}))
(def vconj (fnil conj []))

(defn conj-at-index
  "Add element to a vector at some specific index. Only works with vectors!"
  [v idx x]
  (let [before (subvec v 0 idx)
        after  (subvec v idx (count v))]
    (into (with-meta [] (meta v)) (concat before [x] after))))

(defn index-of
  "Find the index of element x in coll. Return nil if element is not found."
  [coll x]
  (reduce
    (fn [_ [i x']]
      (if (= x x')
        (reduced i)))
    nil
    (map-indexed vector coll)))

(defn queue
  "Return a blank immutable queue or create one from coll."
  ([] #?(:clj  clojure.lang.PersistentQueue/EMPTY
         :cljs cljs.core/PersistentQueue.EMPTY))
  ([coll]
   (into (queue) coll)))

(defn make-map-entry
  "CLJC helper to create MapEntry."
  [k v]
  #?(:clj
     (MapEntry. k v)

     :cljs
     (MapEntry. k v nil)))

(defn map-keys
  "Map over the given hash-map keys.

  Example:
    (map-keys #(str/replace (name %) \"_\" \"-\") {\"foo_bar\" 1}) => {\"foo-bar\" 1}
  "
  [f m]
  (into (with-meta {} (meta m)) (map (fn [x] (make-map-entry (f (key x)) (val x)))) m))

(defn map-vals
  "Map over the given hash-map vals.

  Example:
    (map-vals inc {:a 1 :b 2})
  "
  [f m]
  (into (with-meta {} (meta m)) (map (fn [x] (make-map-entry (key x) (f (val x))))) m))

(defn filter-keys
  [f m]
  (into (with-meta {} (meta m)) (filter (comp f key)) m))

(defn filter-vals
  [f m]
  (into (with-meta {} (meta m)) (filter (comp f val)) m))

(defn remove-keys
  [f m]
  (into (with-meta {} (meta m)) (remove (comp f key)) m))

(defn remove-vals
  [f m]
  (into (with-meta {} (meta m)) (remove (comp f val)) m))

(defn keys-set
  "Return the map keys, as a set. This also checks if the entry is a map, otherwise
  returns nil (instead of throw)."
  [m]
  (if (map? m) (into #{} (keys m))))

(defn merge-grow
  "Additive merging.

  When merging maps, it does a deep merge.
  When merging sets, makes a union of them.

  When value of the right side is nil, the left side will be kept.

  For the rest works as standard merge."
  ([] {})
  ([a] a)
  ([a b]
   (cond
     (and (set? a) (set? b))
     (set/union a b)

     (and (map? a) (map? b))
     (merge-with merge-grow a b)

     (nil? b) a

     :else
     b)))

(defn merge-defaults
  "Like merge, but only add keys that are not present in the original map."
  [m defaults]
  (reduce-kv
    (fn [m k v]
      (if (contains? m k)
        m
        (assoc m k v)))
    m
    defaults))

(def ^:dynamic *deep-merge-handlers* {})

;; (s/def ::merge-self #{::merge-replace ::merge-into})

(defn value-merger [x]
  (some-> x meta ::merge-with))

(defn keep-current "Merge util to keep the value from the left side." [a _] a)
(defn keep-new "Merge util to keep the value from the right side." [_ b] b)

(defn deep-merge
  "Recursively merges maps together. If all the maps supplied have nested maps
  under the same keys, these nested maps are merged.

  A custom merge strategy may be configured for specific keys, to do so you need to
  bind the value of *deep-merge-handlers*, the keys are the keywords and the values
  are functions to handle the merge of that key.

      (binding [*deep-merge-handlers* {:foo +}]
        (deep-merge {:foo 1} {:foo 2}))
      ; => {:foo 3}

  Another way to control the merge of values is to set some metadata to control the
  merge process (notice it only applies for types that support meta like maps and vectors,
  but not for keywords or numbers for example).

      (deep-merge {:list [1 2 3]} ^{::merge-with into} {:list [:a :b]})
      ; => {:list [1 2 3 :a :b]}

  The meta may go in the left or side of the merge, the right side has higher priority.

  Note that meta mergers have higher priority than merge handlers.

  Here is the merger pick order for clarity:

  1. Value merger from meta at new value
  2. Value merger from meta at current value
  3. Merge key handler
  4. Deep merge (in case both values are maps)
  5. Keep the new value (as in `clojure.core/merge`)

  Forked from Medley library."
  {:arglists '([& maps])}
  ([])
  ([a] a)
  ([a b]
   (when (or a b)
     (letfn [(merge-entry [m e]
               (let [k  (key e)
                     v' (val e)]
                 (if (contains? m k)
                   (let [v      (get m k)
                         merger (or (value-merger v')
                                    (value-merger v)
                                    (get *deep-merge-handlers* k)
                                    (if (and (map? v) (map? v'))
                                      deep-merge)
                                    keep-new)]
                     (assoc m k (merger (get m k) v')))
                   (assoc m k v'))))]
       (reduce merge-entry (or a {}) (seq b)))))
  ([a b & more]
   (reduce deep-merge (or a {}) (cons b more))))

(defn assoc-if
  "Like assoc, but noop if v is falsy."
  ([m k v]
   (if v
     (assoc m k v)
     m))
  ([m k v & kvs]
   (let [ret (assoc-if m k v)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defn update-contained
  "Update some key when that key is present in the map."
  ([m k f]
   (if (contains? m k)
     (update m k f)
     m))
  ([m k f a1]
   (if (contains? m k)
     (update m k f a1)
     m))
  ([m k f a1 a2]
   (if (contains? m k)
     (update m k f a1 a2)
     m))
  ([m k f a1 a2 a3]
   (if (contains? m k)
     (update m k f a1 a2 a3)
     m))
  ([m k f a1 a2 a3 & args]
   (if (contains? m k)
     (apply update m k f a1 a2 a3 args)
     m)))

(defn update-if
  "Update some key if that key is present in the map and value is truthy."
  ([m k f]
   (if (get m k)
     (update m k f)
     m))
  ([m k f a1]
   (if (get m k)
     (update m k f a1)
     m))
  ([m k f a1 a2]
   (if (get m k)
     (update m k f a1 a2)
     m))
  ([m k f a1 a2 a3]
   (if (get m k)
     (update m k f a1 a2 a3)
     m))
  ([m k f a1 a2 a3 & args]
   (if (get m k)
     (apply update m k f a1 a2 a3 args)
     m)))

(defn native-map? [x]
  #?(:clj  (or (instance? clojure.lang.PersistentArrayMap x)
               (instance? clojure.lang.PersistentHashMap x))
     :cljs (or (instance? cljs.core/PersistentArrayMap x)
               (instance? cljs.core/PersistentHashMap x))))

(defn restore-order
  "Sorts output list to match input list order.

      (coll/restore-order
        [{:id 1} {:id 2} {:id 3}]
        :id
        [{:id 4 :x \"a\"}
         {:id 1 :x \"b\"}
         {:id 3 :x \"c\"}
         {:id 2 :x \"d\"}])

      => [{:id 1 :x \"b\"}
          {:id 2 :x \"d\"}
          {:id 3 :x \"c\"}]

      (coll/restore-order
        [{:id 1, :id2 1} {:id 2, :id2 0} {:id 3, :id2 1}]
        #(select-keys % [:id :id2])
        [{:id 4 :id2 0 :x \"a\"}
         {:id 1 :id2 1 :x \"b\"}
         {:id 3 :id2 1 :x \"c\"}
         {:id 2 :id2 0 :x \"d\"}])

      => [{:id 1 :id2 1 :x \"b\"}
          {:id 2 :id2 0 :x \"d\"}
          {:id 3 :id2 1 :x \"c\"}]

   Note it will also remove items that don't match anything in the original items
   list.

   In case the items contains a matching key more than once, the last one will be taken."
  ([inputs key items]
   (restore-order inputs key items (if (ident? key) #(hash-map key (get % key)) key)))
  ([inputs key items default-fn]
   (let [index (index-by key items)]
     (into []
           (map (fn [input]
                  (or (get index (key input))
                      (default-fn input))))
           inputs))))

(defn restore-order2
  "Same functionality as restore-order, but fixes the arguments order to make
  it easier to thread with a default-fn.

  Also, it returns nil for not found items instead of a map with the key."
  ([inputs key items] (restore-order inputs key items (constantly nil)))
  ([inputs key default-fn items] (restore-order inputs key items default-fn)))

(defn iterator
  "CLJC utility to get an iterator from the collection."
  [coll]
  #?(:clj
     (if (instance? java.lang.Iterable coll)
       (.iterator ^java.lang.Iterable coll)
       (.iterator (seq coll)))

     :cljs
     (iter coll)))

(defn iterate-while
  "Like iterate, but stops when it sees a `nil` value."
  [f x]
  (take-while some? (iterate f x)))

(defn coll-append-at-head?
  "Return true if column add items at head with conj."
  [s]
  (not (or (vector? s) (set? s))))

(defn collection?
  "Returns true for sequential collections and sets, false for maps."
  [x]
  (or (sequential? x)
      (set? x)))

(defn vector-compare
  "Compare two vectors, this expects the vectors to be ordered."
  [[value1 & rest1] [value2 & rest2]]
  (let [result (compare value1 value2)]
    (cond
      (not (zero? result)) result
      (nil? value1) 0
      :else (recur rest1 rest2))))
