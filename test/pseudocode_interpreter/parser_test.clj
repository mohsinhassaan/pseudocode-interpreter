(ns pseudocode-interpreter.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [pseudocode-interpreter.parser :refer [parse]]))

(deftest int-literal-test
  (testing "Integer Literal"
    (testing "Positive Integer (implicit)"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:intLiteral "1"))))))
                     "<EOF>")
             (parse "1")))
      (testing "Positive Integer (explicit)"
        (is (= '(:prog (:line (:statement
                               (:expression (:func
                                             (:literal (:intLiteral "+1"))))))
                       "<EOF>")
               (parse "+1"))))
      (testing "Negative Integer"
        (is (= '(:prog (:line (:statement
                               (:expression (:func
                                             (:literal (:intLiteral "-1"))))))
                       "<EOF>")
               (parse "-1")))))))

(deftest real-literal-test
  (testing "Real Literal"
    (testing "Positive Real (implicit)"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:realLiteral "1.0"))))))
                     "<EOF>")
             (parse "1.0"))))
    (testing "Positive Real (explicit)"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:realLiteral "+3.1"))))))
                     "<EOF>")
             (parse "+3.1"))))
    (testing "Negative Real"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:realLiteral "-23.71"))))))
                     "<EOF>")
             (parse "-23.71"))))))

(deftest char-literal-test
  (testing "Character Literal"
    (testing "Single Character"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:charLiteral "'a'"))))))
                     "<EOF>")
             (parse "'a'"))))
    (testing "Escape Character"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:charLiteral "'\\n'"))))))
                     "<EOF>")
             (parse "'\\n'"))))))

(deftest string-literal-test
  (testing "String Literal"
    (is (= '(:prog (:line (:statement
                           (:expression (:func
                                         (:literal (:strLiteral "\"This is a string. \\n $123\""))))))
                   "<EOF>")
           (parse "\"This is a string. \\n $123\"")))))

(deftest bool-literal-test
  (testing "Boolean Literal"
    (testing "TRUE"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:boolLiteral "TRUE"))))))
                     "<EOF>")
             (parse "TRUE"))))
    (testing "FALSE"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:literal (:boolLiteral "FALSE"))))))
                     "<EOF>")
             (parse "FALSE"))))))

(deftest varname-test
  (testing "Variable Name"
    (testing "Lowercase"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:var (:varname "abc"))))))
                     "<EOF>")
             (parse "abc"))))
    (testing "Uppercase"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:var (:varname "ABC"))))))
                     "<EOF>")
             (parse "ABC"))))
    (testing "Letters + Numbers"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:var (:varname "abc123"))))))
                     "<EOF>")
             (parse "abc123"))))
    (testing "snake_case"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:var (:varname "abc_xyz"))))))
                     "<EOF>")
             (parse "abc_xyz"))))))

(deftest fn-call-test
  (testing "Function Calls"
    (testing "No Args"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:funcCall (:varname "f") "(" ")")))))
                     "<EOF>")
             (parse "f()"))))
    (testing "Single Arg"
      (is (= '(:prog (:line (:statement
                             (:expression (:func
                                           (:funcCall (:varname "f") "(" (:exprlist (:expression (:func (:var (:varname "x"))))) ")")))))

                     "<EOF>")
             (parse "f(x)"))))
    (testing "Multiple Args"
      (is (= '(:prog
               (:line
                (:statement
                 (:expression
                  (:func
                   (:funcCall
                    (:varname "f")
                    "("
                    (:exprlist (:expression (:func (:var (:varname "x")))) "," (:expression (:func (:var (:varname "y")))))
                    ")")))))
               "<EOF>")
             (parse "f(x, y)"))))))

(deftest vardecl-test
  (testing "Variable Declaration"
    (is (= '(:prog
             (:line (:statement
                     (:vardecl "DECLARE" (:varname "x") ":" (:type "INTEGER"))))
             "<EOF>")
           (parse "DECLARE x : INTEGER")))))

(deftest constdecl-test
  (testing "Constant Declaration"
<<<<<<< HEAD
    (is (= (parse "CONSTANT PI = 3.14")
           '(:prog
             (:line (:statement (:constdecl "CONSTANT" (:varname "PI") "=" (:expression (:func (:literal (:realLiteral "3.14")))))))
             "<EOF>")))))
=======
    (is (= '(:prog
             (:line (:statement
                     (:constdecl "CONSTANT"
                                 (:varname "PI")
                                 "="
                                 (:literal (:realLiteral "3.14")))))
             "<EOF>")
           (parse "CONSTANT PI = 3.14")))))
>>>>>>> main

(deftest letstmt-test
  (testing "Variable Assignment"
    (testing "Without LET"
      (is (= '(:prog
               (:line
                (:statement
                 (:letstmt (:variableassignment (:var (:varname "x")) "<-" (:expression (:func (:literal (:intLiteral "11"))))))))
               "<EOF>")
             (parse "x <- 11"))))
    (testing "With LET"
      (is (= '(:prog
               (:line
                (:statement
                 (:letstmt
                  "LET"
                  (:variableassignment (:var (:varname "x")) "<-" (:expression (:func (:literal (:intLiteral "10"))))))))
               "<EOF>")
             (parse "LET x <- 10"))))
    (testing "With Array Indexing"
      (is (= '(:prog
               (:line
                (:statement
                 (:letstmt
                  (:variableassignment
                   (:var (:arrayaccess (:varname "arr") "[" (:expression (:func (:literal (:intLiteral "1")))) "]"))
                   "<-"
                   (:expression (:func (:literal (:intLiteral "10"))))))))
               "<EOF>")
             (parse "arr[1] <- 10"))))))

(deftest arraydecl-test
  (testing "Array Declaration"
    (testing "1D"
      (is (= '(:prog
               (:line
                (:statement
                 (:arraydecl
                  "DECLARE"
                  (:varname "arr")
                  ":"
                  (:oneDimArray
                   "ARRAY"
                   "["
                   (:range (:intLiteral "1") ":" (:intLiteral "5"))
                   "]"
                   "OF"
                   (:type "STRING")))))
               "<EOF>")
             (parse "DECLARE arr : ARRAY[1:5] OF STRING"))))
    (testing "2D"
      (is (= '(:prog
               (:line
                (:statement
                 (:arraydecl
                  "DECLARE"
                  (:varname "arr")
                  ":"
                  (:twoDimArray
                   "ARRAY"
                   "["
                   (:range (:intLiteral "0") ":" (:intLiteral "10"))
                   ","
                   (:range (:intLiteral "0") ":" (:intLiteral "10"))
                   "]"
                   "OF"
                   (:type "INTEGER")))))
               "<EOF>")
             (parse "DECLARE arr : ARRAY[0:10,0:10] OF INTEGER"))))))

(deftest output-test
  (testing "OUTPUT statement"
    (testing "Single arg"
      (is (= '(:prog
               (:line (:statement (:printstmt "OUTPUT" (:printlist (:expression (:func (:var (:varname "x"))))))))
               "<EOF>")
             (parse "OUTPUT x"))))
    (testing "Multiple args"
      (is (= '(:prog
               (:line
                (:statement
                 (:printstmt
                  "OUTPUT"
                  (:printlist
                   (:expression (:func (:literal (:strLiteral "\"abc\""))))
                   ","
                   (:expression (:func (:literal (:strLiteral "\"xyz\""))))))))
               "<EOF>")
             (parse "OUTPUT \"abc\", \"xyz\""))))))

(deftest input-test
  (testing "INPUT statement"
    (is (= '(:prog (:line (:statement (:inputstmt "INPUT" (:varname "x")))) "<EOF>")
           (parse "INPUT x")))))

(deftest call-test
  (testing "CALL statement"
    (is (= '(:prog (:line (:statement (:callstmt "CALL" (:varname "x")))) "<EOF>")
           (parse "CALL x")))))

(deftest pass-test
  (testing "PASS statement"
    (is (= '(:prog (:line (:statement (:pass "PASS"))) "<EOF>")
           (parse "PASS")))))

(deftest if-test
  (testing "IF statement"
    (testing "Without ELSE"
      (is (= '(:prog
               (:line
                (:statement
                 (:ifstmt
                  "IF"
                  (:expression
                   (:relationalExpression
                    (:addingExpression (:multiplyingExpression (:signExpression (:func (:var (:varname "x"))))))
                    "="
                    (:addingExpression (:multiplyingExpression (:signExpression (:func (:literal (:intLiteral "1"))))))))
                  (:thenstmt "THEN"
                             (:statement (:pass "PASS")))
                  (:endifstmt "ENDIF"))))
               "<EOF>")
             (parse "IF x = 1 THEN
                        PASS
                     ENDIF"))))
    (testing "With ELSE"
      (is (= '(:prog
               (:line
                (:statement
                 (:ifstmt
                  "IF"
                  (:expression
                   (:relationalExpression
                    (:addingExpression (:multiplyingExpression (:signExpression (:func (:var (:varname "x"))))))
                    "="
                    (:addingExpression (:multiplyingExpression (:signExpression (:func (:literal (:intLiteral "1"))))))))
                  (:thenstmt  "THEN"
                              (:statement (:pass "PASS")))
                  (:elsestmt "ELSE"
                             (:statement (:pass "PASS")))
                  (:endifstmt "ENDIF"))))
               "<EOF>")
             (parse "IF x = 1 THEN
                        PASS
                       ELSE
                        PASS
                     ENDIF"))))))

(deftest for-test
  (testing "FOR loop"
    (testing "Without STEP"
      (is (= '(:prog
               (:line
                (:statement
                 (:forstmt
                  "FOR"
                  (:variableassignment (:var (:varname "i")) "<-" (:expression (:func (:literal (:intLiteral "1")))))
                  (:to "TO" (:expression (:func (:literal (:intLiteral "10")))))
                  (:statement (:pass "PASS"))
                  (:nextstmt "NEXT" (:varlist (:var (:varname "i")))))))
               "<EOF>")
             (parse "FOR i <- 1 TO 10
                        PASS
                     NEXT i"))))
    (testing "With STEP"
      (is (= '(:prog
               (:line
                (:statement
                 (:forstmt
                  "FOR"
                  (:variableassignment (:var (:varname "i")) "<-" (:expression (:func (:literal (:intLiteral "1")))))
                  (:to "TO" (:expression (:func (:literal (:intLiteral "10")))))
                  (:step "STEP" (:expression (:func (:literal (:intLiteral "2")))))
                  (:statement (:pass "PASS"))
                  (:nextstmt "NEXT" (:varlist (:var (:varname "i")))))))
               "<EOF>")
             (parse "FOR i <- 1 TO 10 STEP 2
                        PASS
                     NEXT i"))))))

(deftest while-test
  (testing "WHILE loop"
    (is (= '(:prog
             (:line
              (:statement
               (:whilestmt
                "WHILE"
                (:expression
                 (:relationalExpression
                  (:addingExpression (:multiplyingExpression (:signExpression (:func (:var (:varname "x"))))))
                  "="
                  (:addingExpression (:multiplyingExpression (:signExpression (:func (:literal (:intLiteral "1"))))))))
                (:statement (:pass "PASS"))
                (:endwhilestmt "ENDWHILE"))))
             "<EOF>")
           (parse "WHILE x = 1
                      PASS
                   ENDWHILE")))))

(deftest repeat-test
  (testing "REPEAT loop"
    (is (= '(:prog
             (:line
              (:statement
               (:repeatstmt
                "REPEAT"
                (:statement (:pass "PASS"))
                (:untilstmt
                 "UNTIL"
                 (:expression
                  (:relationalExpression
                   (:addingExpression (:multiplyingExpression (:signExpression (:func (:var (:varname "x"))))))
                   "="
                   (:addingExpression (:multiplyingExpression (:signExpression (:func (:literal (:intLiteral "1"))))))))))))
             "<EOF>")
           (parse "REPEAT
                      PASS
                   UNTIL x = 1")))))

(deftest fn-test
  (testing "Function Definition"
    (testing "Without args"
      (is (= '(:prog
               (:line
                (:statement
                 (:funcstmt
                  "FUNCTION"
                  (:varname "f")
                  "("
                  ")"
                  "RETURNS"
                  (:type "INTEGER")
                  (:returnstmt "RETURN" (:expression (:func (:literal (:intLiteral "1")))))
                  (:endfuncstmt "ENDFUNCTION"))))
               "<EOF>")
             (parse "FUNCTION f() RETURNS INTEGER
                        RETURN 1
                     ENDFUNCTION"))))
    (testing "With args"
      (is (= '(:prog
               (:line
                (:statement
                 (:funcstmt
                  "FUNCTION"
                  (:varname "f")
                  "("
                  (:arglist (:var (:varname "x")) ":" (:type "INTEGER") "," (:var (:varname "y")) ":" (:type "CHAR"))
                  ")"
                  "RETURNS"
                  (:type "INTEGER")
                  (:returnstmt "RETURN" (:expression (:func (:literal (:intLiteral "1")))))
                  (:endfuncstmt "ENDFUNCTION"))))
               "<EOF>")
             (parse "FUNCTION f(x:INTEGER, y:CHAR) RETURNS INTEGER
                        RETURN 1
                     ENDFUNCTION"))))))

(deftest proc-test
  (testing "Procedure Definition"
    (testing "Without args"
      (is (= '(:prog
               (:line
                (:statement
                 (:procstmt "PROCEDURE" (:varname "proc") "(" ")" (:statement (:pass "PASS")) (:endprocstmt "ENDPROCEDURE"))))
               "<EOF>")
             (parse "PROCEDURE proc()
                        PASS
                     ENDPROCEDURE"))))
    (testing "With args"
      (is (= '(:prog
               (:line
                (:statement
                 (:procstmt
                  "PROCEDURE"
                  (:varname "proc")
                  "("
                  (:arglist (:var (:varname "x")) ":" (:type "INTEGER") "," (:var (:varname "y")) ":" (:type "CHAR"))
                  ")"
                  (:statement (:pass "PASS"))
                  (:endprocstmt "ENDPROCEDURE"))))
               "<EOF>")
             (parse "PROCEDURE proc(x:INTEGER, y:CHAR)
                        PASS
                     ENDPROCEDURE"))))))

(deftest case-test
  (testing "CASE statement"
    (testing "Simple clause"
      (is (= '(:prog
               (:line
                (:statement
                 (:casestmt
                  "CASE"
                  "OF"
                  (:var (:varname "x"))
                  (:caseclause (:expression (:func (:literal (:intLiteral "1")))) ":" (:statement (:pass "PASS")))
                  (:endcasestmt "ENDCASE"))))
               "<EOF>")
             (parse "CASE OF x
                        1 : PASS
                     ENDCASE"))))
    (testing "Range clause"
      (is (= '(:prog
               (:line
                (:statement
                 (:casestmt
                  "CASE"
                  "OF"
                  (:var (:varname "x"))
                  (:rangeClause
                   (:expression (:func (:literal (:intLiteral "1"))))
                   "TO"
                   (:literal (:intLiteral "10"))
                   ":"
                   (:statement (:pass "PASS")))
                  (:endcasestmt "ENDCASE"))))
               "<EOF>")
             (parse "CASE OF x
                        1 TO 10 : PASS
                     ENDCASE"))))
    (testing "Otherwise clause"
      (is (= '(:prog
               (:line
                (:statement
                 (:casestmt
                  "CASE"
                  "OF"
                  (:var (:varname "x"))
                  (:caseclause (:expression (:func (:literal (:intLiteral "1")))) ":" (:statement (:pass "PASS")))
                  (:otherwiseClause "OTHERWISE" ":" (:statement (:pass "PASS")))
                  (:endcasestmt "ENDCASE"))))
               "<EOF>")
             (parse "CASE OF x
                        1 : PASS
                        OTHERWISE : PASS
                     ENDCASE"))))
    (testing "Simple, Range and Otherwise"
      (is (= '(:prog
               (:line
                (:statement
                 (:casestmt
                  "CASE"
                  "OF"
                  (:var (:varname "x"))
                  (:caseclause (:expression (:func (:literal (:intLiteral "1")))) ":" (:statement (:pass "PASS")))
                  (:rangeClause
                   (:expression (:func (:literal (:intLiteral "2"))))
                   "TO"
                   (:literal (:intLiteral "10"))
                   ":"
                   (:statement (:pass "PASS")))
                  (:otherwiseClause "OTHERWISE" ":" (:statement (:pass "PASS")))
                  (:endcasestmt "ENDCASE"))))
<<<<<<< HEAD
               "<EOF>"))))))
=======
               "<EOF>")
             (parse "CASE OF x
                        1 : PASS
                        2 TO 10 : PASS
                        OTHERWISE : PASS
                     ENDCASE"))))))
>>>>>>> main
