(ns com.wsscode.misc.refs
  #?(:clj
     (:import
       (clojure.lang
         IDeref
         Atom
         Volatile))))

(defn kw-identical?
  "Clojure/Clojurescript efficient keyword comparison."
  [kw-a kw-b]
  #?(:clj  (identical? kw-a kw-b)
     :cljs (keyword-identical? kw-a kw-b)))

(defn atom?
  "Check if x is an atom."
  [x]
  #?(:clj  (instance? IDeref x)
     :cljs (satisfies? IDeref x)))

(defprotocol GenericBox
  (-greset! [this value])
  (-gswap!
    [this f]
    [this f x]
    [this f x y]
    [this f x y more]))

(extend-protocol GenericBox
  Atom
  (-greset! [this value] (reset! this value))
  (-gswap!
    ([this f] (swap! this f))
    ([this f x] (swap! this f x))
    ([this f x y] (swap! this f x y))
    ([this f x y more] (apply swap! this f x y more)))

  Volatile
  (-greset! [this value] (vreset! this value))
  (-gswap!
    ([this f] (vswap! this f))
    ([this f x] (vswap! this f x))
    ([this f x y] (vswap! this f x y))
    ([this f x y more] (vreset! this (apply f @this x y more)))))

(defn greset!
  [this val] (-greset! this val))

(defn gswap!
  ([this f] (-gswap! this f))
  ([this f x] (-gswap! this f x))
  ([this f x y] (-gswap! this f x y))
  ([this f x y & more] (-gswap! this f x y more)))
