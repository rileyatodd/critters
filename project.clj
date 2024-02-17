(defproject critters "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.1.8"]]
  :hooks [leiningen.cljsbuild]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.11.132"]
                 [quil "4.3.1323"]
                 [cljsjs/p5 "1.7.0-0"]
                 [org.apache.commons/commons-math3 "3.3"]
                 [genartlib "0.1.22"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :cljsbuild
  {:builds [{:source-paths ["src"] ; The path to the top-level ClojureScript source directory:
                                        ; The standard ClojureScript compiler options:
                                        ; (See the ClojureScript compiler documentation for details.)
             :compiler {:output-to "public/js/lein-main.js"  ; default: target/cljsbuild-main.js
                        :optimizations :whitespace
                        :pretty-print true}}]})
