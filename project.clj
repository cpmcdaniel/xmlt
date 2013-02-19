(require '[clojure.java.shell :refer [sh]])

(defproject jarohen/xmlt (.trim (:out (sh "git" "describe" "--tags" "--abbrev=0")))
  :description "An XML transformation library for Clojure"
  :url "https://github.com/james-henderson/xmlt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]

  :scm {:name "git"
        :tag ~(.trim (:out (sh "git" "rev-parse" "HEAD")))
        :url "https://github.com/james-henderson/xmlt"})
