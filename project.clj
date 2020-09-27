(defproject com.wsscode/cljc-misc "0.0.1"
  :description "Collection of general utils for Clojure and Clojurescript."
  :url "https://github.com/wilkerlucio/cljc-misc"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}

  :source-paths ["src/main"]

  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.764" :scope "provided"]]

  :jar-exclusions [#"resources/.*" #"node-modules/.+"]

  :deploy-repositories [["clojars" {:url   "https://clojars.org/repo/"
                                    :creds :gpg :checksum :ignore}]
                        ["releases" :clojars]
                        ["snapshots" :clojars]])
