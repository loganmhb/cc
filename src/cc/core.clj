(ns cc.core
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def parse (insta/parser "src/cc/grammar.abnf"))

(compile (parse "def myfn(a, b, c) a+b; extern puts(s);"))

(compile (parse "def myfn(a) 1+1*1;"))
[:module [:statement [:def] [:name "myfn"] [:arglist [:name "a"]] [:expr [:binaryop [:expr [:double "1"]] [:operator "+"] [:expr [:binaryop [:expr [:double "1"]] [:operator "*"] [:expr [:double "1"]]]]]]]]

;; Need a way to convert tree -> SSA, e.g.
;; (a + b) + c -> { %1 = add a, b; %2 = add %1, c; ret 2 }

(defn compile [ast]
  (match ast
    [:module & stmts] (clojure.string/join "\n" (map compile stmts))

    [:statement [:def] [:name def-name] arglist body]
    (str "define double @" def-name (compile arglist) "{" (compile body) "}")

    [:expr & contents] :???
    [:statement [:extern] [:name extern-name] arglist]
    (str "declare double @" extern-name (compile arglist) "{}")

    [:arglist & names] (str "(" (str/join ", " (map compile names)) ")")
    [:name n] (str "double %" n)))

(defn -main
  "I don't do a whole lot."
  [in out]
  (println "Compiling" in)
  (emit (parse (slurp in)) out))
