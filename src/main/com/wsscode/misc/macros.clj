(ns com.wsscode.misc.macros)

(defn full-symbol
  "Helper to expand symbol name during macro compilation. When symbol is qualified
  it is returned as is, otherwise fallback-ns is set as the namespace.

  Example usage:

      (defmacro defsomething [id options]
        (let [fqsym (full-symbol id (str *ns*)))]
          `(def ~id (assoc options ::id '~fqsym))))"
  [sym fallback-ns]
  (if (namespace sym)
    sym
    (symbol fallback-ns (name sym))))
