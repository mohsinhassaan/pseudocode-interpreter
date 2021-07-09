(ns pseudocode-interpreter.parser
  (:require [clj-antlr.core :as antlr])
  (:gen-class))

<<<<<<< HEAD
(def parse (antlr/parser "grammars/pseudocode.g4" {:throw? true}))
=======
(def parse (antlr/parser "grammars/pseudocode.g4" {:throw? false}))

#_(parse "INPUT x
        OUTPUT \"X \", x")

>>>>>>> main
