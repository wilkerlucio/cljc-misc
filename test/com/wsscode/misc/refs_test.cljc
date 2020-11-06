(ns com.wsscode.misc.refs-test
  (:require
    [clojure.test :refer [deftest is are run-tests testing]]
    [com.wsscode.misc.refs :refer [atom?] :as refs]))

(deftest kw-identical?-test
  (is (not (refs/kw-identical? :foo :bar)))
  (is (not (refs/kw-identical? :foo "foo")))
  (is (refs/kw-identical? :foo :foo))
  (is (refs/kw-identical? :foo (keyword "foo"))))

(deftest atom?-test
  (is (true? (atom? (atom "x"))))
  (is (false? (atom? "x"))))
