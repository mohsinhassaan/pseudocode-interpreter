(ns pseudocode-interpreter.evaluator
  (:require [clojure.core.match :refer [match]]))

(def default-state {})

(defn evaluate
  ([code] (evaluate default-state code))
  ([state code]
   (if (:error state)
     (throw (str (:error state) " near " (-> code meta :clj-antlr/position)))
     (match code
            ;; Program Start
            ([:prog & lines] :seq) (reduce evaluate state lines)
            ;; Line
            ([:line ([:statement stmt] :seq)] :seq) (evaluate state stmt)
            ;; STATEMENTS TODO pass, ifstmt, casestmt, forstmt, whilestmt,
            ;; repeatstmt, funcstmt, procstmt, callstmt, inputstmt, printstmt,
            ;; vardecl, arraydecl, letstmt, expression
            ;; Constant Declaration
            ([:constdecl
              "CONSTANT"
              ([:varname sym] :seq)
              "="
              exp] :seq) (if (state sym)
                         (assoc state
                                :error "Constant cannot be redefined")
                         (assoc state
                                sym (:result (evaluate state exp))))

            ;; EXPRESSIONS TODO relationalExpression
            ;; Base Expression
            ([:expression exp] :seq) (evaluate state exp)
            ;; Funcs TODO funcCall, (expression)
            ([:func exp] :seq) (evaluate state exp)
            ;; Literals
            ([:literal
              ([t l] :seq)] :seq) (->> (case t
                                         :realLiteral (Double/parseDouble l)
                                         :intLiteral (Integer/parseInt l)
                                         :boolLiteral (Boolean/parseBoolean l)
                                         :charLiteral (second l)
                                         :strLiteral ())
                                       (assoc state :result))
            ;; Get var value
            ([:var ([:varname sym] :seq)] :seq) (->>
                                          (if (nil? (state sym))
                                            (assoc state
                                                   :error
                                                   (str sym " is undefined"))
                                            (state sym))
                                          (assoc state :result))
            "<EOF>" state))))
