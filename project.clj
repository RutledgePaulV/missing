(defproject org.clojars.rutledgepaulv/missing "0.1.32"

  :description
  "A utility library for Clojure of functions and macros that are frequently missed and recreated."

  :url
  "https://github.com/rutledgepaulv/missing"

  :license
  {:name "MIT License" :url "http://opensource.org/licenses/MIT" :year 2020 :key "mit"}

  :scm
  {:name "git" :url "https://github.com/rutledgepaulv/missing"}

  :pom-addition
  [:developers
   [:developer
    [:name "Paul Rutledge"]
    [:url "https://github.com/rutledgepaulv"]
    [:email "rutledgepaulv@gmail.com"]
    [:timezone "-5"]]]

  :deploy-repositories
  [["releases" :clojars]
   ["snapshots" :clojars]]

  :plugins
  [[lein-cloverage "1.1.2"]]

  :dependencies
  [[org.clojure/clojure "1.10.1"]])
