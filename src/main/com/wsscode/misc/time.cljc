(ns com.wsscode.misc.time)

(defn now-ms []
  #?(:clj  (/ (double (System/nanoTime)) 1000000.0)
     :cljs (system-time)))

(defn sleep-ms
  "Clojure and Clojurescript solution to hold execution for a given time."
  [ms]
  #?(:clj  (Thread/sleep ms)
     :cljs (let [now         (system-time)
                 continue-at (+ now ms)]
             (loop []
               (if (< (system-time) continue-at)
                 (recur))))))
