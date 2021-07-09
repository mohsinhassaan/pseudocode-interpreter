(ns pseudocode-interpreter.parser
  (:require [clj-antlr.core :as antlr])
  (:gen-class))

(def parse (antlr/parser "grammars/pseudocode.g4" {:throw? true}))

#_(parse "INPUT x
        OUTPUT \"X \", x")
