(ns com.wsscode.misc.refs
  #?(:clj
     (:import
       (clojure.lang
         IDeref))))

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
