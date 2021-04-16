(defproject nubank/clj-github-mock "0.1.0-SNAPSHOT"
  :description "An emulator of the github api"
  :url "https://github.com/nubank/clj-github-mock"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
 :plugins [[lein-cljfmt "0.6.4" :exclusions [org.clojure/clojure]]
            [lein-cloverage "1.0.13" :exclusions [org.clojure/clojure]]
            [lein-vanity "0.2.0" :exclusions [org.clojure/clojure]]
            [lein-kibit "0.1.7" :exclusions [org.clojure/clojure]]
            [lein-nsorg "0.3.0" :exclusions [org.clojure/clojure]]
            [s3-wagon-private "1.3.2" :exclusions [commons-logging org.apache.httpcomponents/httpclient]]
            [lein-ancient "0.6.14" :exclusions [commons-logging com.fasterxml.jackson.core/jackson-databind com.fasterxml.jackson.core/jackson-core]]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.eclipse.jgit/org.eclipse.jgit "5.9.0.202009080501-r"]
                 [metosin/reitit-ring "0.5.10"]
                 [base64-clj "0.1.1"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-mock "0.4.0"]
                 [datascript "1.0.7"]]
  :profiles {:dev {:dependencies [[clj-http "3.12.1"]
                                  [clj-http-fake "1.0.3"]
                                  [org.clojure/test.check "1.1.0"]
                                  [nubank/matcher-combinators "3.1.4"]
                                  [metosin/malli "0.3.0"]
                                  [lambdaisland/regal "0.0.89"]
                                  [juji/editscript "0.5.7"]]}}
  :aliases {"coverage" ["cloverage" "-s" "coverage"]
            "lint"     ["do" ["cljfmt" "check"] ["nsorg"]]
            "lint-fix" ["do" ["cljfmt" "fix"] ["nsorg" "--replace"]]
            "loc"      ["vanity"]})
