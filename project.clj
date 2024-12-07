(defproject dev.nubank/clj-github-mock "0.4.0"
  :description "An emulator of the github api"
  :url "https://github.com/nubank/clj-github-mock"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :repositories [["publish" {:url "https://clojars.org/repo"
                             :username :env/clojars_username
                             :password :env/clojars_passwd
                             :sign-releases false}]]

  :plugins [[lein-cljfmt "0.9.2"]
            [lein-cloverage "1.2.4"]
            [lein-vanity "0.2.0"]
            [lein-nsorg "0.3.0"]]

  :dependencies [[org.clojure/clojure "1.12.0"]
                 ; NOTE: can't upgrade to 7.X because it doesn't support JDK 11 anymore
                 [org.eclipse.jgit/org.eclipse.jgit "6.10.0.202406032230-r"]
                 [metosin/reitit-ring "0.7.2"]
                 [ring/ring-json "0.5.1"]
                 [ring/ring-mock "0.4.0"]
                 [datascript "1.7.3"]]

  :profiles {:dev {:plugins [[lein-project-version "0.1.0"]]
                   :dependencies [[org.clojure/test.check "1.1.1"]
                                  [nubank/matcher-combinators "3.9.1"]
                                  [metosin/malli "0.16.4"]
                                  [lambdaisland/regal "0.1.175"]
                                  [juji/editscript "0.6.4"]
                                  [reifyhealth/specmonstah "2.1.0"]
                                  [medley "1.4.0"]]}}

  :aliases {"coverage" ["cloverage" "-s" "coverage"]
            "lint"     ["do" ["cljfmt" "check"] ["nsorg"]]
            "lint-fix" ["do" ["cljfmt" "fix"] ["nsorg" "--replace"]]
            "loc"      ["vanity"]})
