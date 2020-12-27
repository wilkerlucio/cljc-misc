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

(def sconj (fnil conj #{}))
(def vconj (fnil conj []))

(defn conj-at-index
  "Add element to a vector at some specific index. Only works with vectors!"
  [v idx x]
  (let [before (subvec v 0 idx)
        after  (subvec v idx (count v))]
    (into [] (concat before [x] after))))

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
  (into {} (map (fn [x] (make-map-entry (f (key x)) (val x)))) m))

(defn map-vals
  "Map over the given hash-map vals.

  Example:
    (map-vals inc {:a 1 :b 2})
  "
  [f m]
  (into {} (map (fn [x] (make-map-entry (key x) (f (val x))))) m))

(defn filter-keys
  [f m]
  (into {} (filter (comp f key)) m))

(defn filter-vals
  [f m]
  (into {} (filter (comp f val)) m))

(defn remove-keys
  [f m]
  (into {} (remove (comp f key)) m))

(defn remove-vals
  [f m]
  (into {} (remove (comp f val)) m))

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
  "Update some key that key is present in the map and value is truthy."
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
  "Sorts output list to match input list order."
  ([inputs key items]
   (restore-order inputs key items #(hash-map key (get % key))))
  ([inputs key items default-fn]
   (let [index (index-by key items)]
     (into []
           (map (fn [input]
                  (or (get index (get input key))
                      (default-fn input))))
           inputs))))
