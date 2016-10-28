(ns cc.core
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.zip :as zip]))

(def parse (insta/parser "src/cc/grammar.abnf"))

;; Need a way to convert tree -> SSA, e.g.
;; (a + b) + c -> { %1 = add a, b; %2 = add %1, c; ret 2 }
;; SSA form: [[:assign :%1 [:op :add] [:int \"1\"] [:i32 \"2\"]]
;;            [:assign :%2 [:op :add] [:int \"2\"] [:ref :%2]]]
;; Converting a single node of the tree into SSA involves:
;; - converting all of the node's children into SSA (children must be
;;   converted before parents to prevent use before definition)
;; - saving references to converted nodes and recursively subsituting
;;   them into the expression tree


(defn directly-representable? [node]
  (match node
    [:binaryop
     [(:or :int :ref) x]
     op
     [(:or :int :ref) y]]
    true
    ;; Even expressions such as [:expr [:int \"1\"]] are not directly
    ;; representable; they're too low-level, as we need to generate statements.
    :else false))


(defn replace-node-with-ref [loc ref]
  (zip/replace loc [:ref ref]))

(defn ssaify [node ref]
  (match node
    [:binaryop x op y]
    [:assign [:ref ref] [:instr op x y]]
    ;; only binary ops supported
    :else (throw (Exception. "Can't ssaify a non-binaryop node!"))))

(defn operation-element? [node]
  (match node
    [:int _] true
    [:operator _] true
    [:ref _] true
    :else false))

(defn expr->ssa  
  "Convert an expression tree into linearized SSA form (a vector of assignments)."
  [ast]
  (loop [ast (zip/vector-zip ast)
         ssa [:ssa]
         last-ref nil]
    (if (nil? ast) ; base case
      (conj ssa [:ret [:ref last-ref]])
      (let [node (zip/node ast)]
        (cond
          (directly-representable? node) ; contains only refs and literals, no child exprs
          (let [ref (gensym)
                replaced (replace-node-with-ref ast ref)]
            (recur (or (zip/right replaced) (zip/up replaced))
                   (conj ssa (ssaify node ref))
                   ref))

          (operation-element? node) ; ref, op or literal -- doesn't need conversion
          (recur (or (zip/right ast) (zip/up ast)) ssa last-ref)
          
          :else ; start converting children, left first
          (recur (zip/next ast) ssa last-ref))))))

#_(expr->ssa [:binaryop
              [:int "1"]
              [:operator "+"]
              [:binaryop
               [:int "1"]
               [:operator "+"]
               [:int "1"]]])

(def llvm-for-op
  {"+" "add"})

(defn compile [ast]
  (match ast
    [:module & stmts] (clojure.string/join "\n" (map compile stmts))

    [:statement [:def] [:name def-name] arglist body]
    (str "define i32 @" def-name (compile arglist) "{" (compile body) "}")

    [:statement [:extern] [:name extern-name] arglist]
    (str "declare i32 @" extern-name (compile arglist) "{}")

    [:arglist & names] (str "(" (str/join ", " (map compile names)) ")")
    [:name n] (str "i32 %" n)
    [:binaryop x op y] (compile (expr->ssa ast))
    [:ssa & instrs] (str/join "\n" (map compile instrs))

    [:instr [:operator op] & operands]
    (str (llvm-for-op op) " i32 " (str/join ", " (map compile operands)))

    [:int x] (str x)
    [:ref r] (str "%" r)
    [:ret r] (str "ret i32 " (compile r))
    [:assign r instr] (str (compile r) " = " (compile instr))))

(defn emit [ast out]
  (spit out (compile ast)))

(defn -main
  "I don't do a whole lot."
  [in out]
  (println "Compiling" in)
  (emit (parse (slurp in)) out))

(comment
  (-> (parse "def myfn(a, b) 1 + 1 + 2;") (emit "test.ll")))
