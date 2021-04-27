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

(defn parse-long [s]
  #?(:clj (Long/parseLong s)
     :cljs (js/parseInt s)))

(defn parse-double [s]
  #?(:clj (Double/parseDouble s)
     :cljs (js/parseFloat s)))

