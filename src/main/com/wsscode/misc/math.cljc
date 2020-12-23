(ns com.wsscode.misc.math)

(defn floor [x]
  #?(:clj  (int (Math/floor ^double x))
     :cljs (js/Math.floor x)))

(defn round [x]
  #?(:clj  (Math/round ^double x)
     :cljs (js/Math.round x)))

(defn ceil [x]
  #?(:clj  (int (Math/ceil ^double x))
     :cljs (js/Math.ceil x)))

(defn divmod [n d]
  [(floor (/ n d)) (mod n d)])
