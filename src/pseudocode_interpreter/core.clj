(ns pseudocode-interpreter.core
  (:require [pseudocode-interpreter.parser :refer [parse]]
            [pseudocode-interpreter.evaluator :refer [execute-prog]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn log-and-return
  [x]
  (pprint x)
  x)

(defn execute-string
  [s]
  (-> s
      parse
      log-and-return
      execute-prog))

(defn execute-code
  [f]
  (execute-string (slurp f)))

(defn -main
  "Interpret code from file"
  [& args]
  (if-let [file (first args)]
    (execute-code file)
    (execute-string (slurp *in*))))

