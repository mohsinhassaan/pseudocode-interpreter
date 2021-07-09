(ns pseudocode-interpreter.evaluator
  (:require [clojure.core.match :refer [match]]))

(def default-state {})

<<<<<<< HEAD
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
=======
(defn- real-str? [val]
  (re-matches #"[0-9]+\.[0-9]+" val))

(defn- integer-str? [val]
  (re-matches #"[0-9]+" val))

(defn- bool-str? [val]
  (#{"TRUE" "FALSE"} val))

(defn- char-str? [val]
  (re-matches #"." val))

(defn- char-lit->char [lit]
  (case (str lit)
    "'\\n'" \newline
    "'\\r'" \return
    "'\\t'" \tab
    "'\\f'" \formfeed
    (-> (re-matches #"'(.)'" lit)
        second
        first)))

(defn- string-lit->string [lit]
  (second (re-matches #"\"([^\n\r]*)\"" lit)))

(defn- char-str->char [val]
  (case (str val)
    "\\n" \newline
    "\\r" \return
    "\\t" \tab
    "\\f" \formfeed
    (-> (re-matches #"(.)" val)
        second
        first)))

(defn- val-type [val]
  (cond
    (integer-str? val) :integer
    (real-str? val) :real
    (bool-str? val) :boolean
    (char-str? val) :char
    :else :string))

(defn- parse-val [datatype val]
  (case datatype
    :integer (Integer/parseInt val)
    :real (Double/parseDouble val)
    :boolean (Boolean/parseBoolean val)
    :char (char-str->char val)
    :string val))

(defn- constant? [var]
  (= :constant (:type var)))

(defn- datatype [val]
  (cond
    (string? val) :string
    (integer? val) :integer
    (boolean val) :boolean
    (float? val) :real
    (char? val) :char))

;; TODO refactor everything to return state and value (nil if no value)

(defmulti execute
  "Executes parsed code and returns program state"
  (fn [_ form]
    (first form)))

(defmulti evaluate
  "Evaluates expression and returns value"
  (fn [_ form]
    (first form)))

;; STATE-RETURNING

(defn execute-prog [[_ & prog]]
  (let [state default-state
        lines (filter #(= :line (first %)) (drop-last prog))]
    (reduce execute state lines)))

(defmethod execute :line [state [_ statement]]
  (execute state statement))

(defmethod execute :statement [state [_ sub-statement]]
  (execute state sub-statement))

(defmethod execute :letstmt [state letstmt]
  (execute state (last letstmt)))

(defmethod execute :variableassignment [state [_ var _ exp]]
  (let [[_ [var-type varname]] var
        {:keys [datatype
                val]} (evaluate state exp)
        var-data {:val val
                  :type :var
                  :datatype datatype}]
    (case var-type
      :varname (if (constant? (state varname))
                 (throw (Error. (str varname " is a defined constant.")))
                 (assoc state varname var-data)))))

(defmethod execute :printstmt [state [_ _ [_ & printlist]]]
  (println (apply str
                  (map (comp :val
                             (partial evaluate state))
                       (remove string? printlist))))
  state)

(defmethod execute :inputstmt [state [_ _ {varname :varname}]]
  (let [old-val (get state varname)]
    (if (constant? old-val)
      (throw (Error. (str varname " is a defined constant.")))
      (let [input (read-line)
            datatype (val-type input)
            parsed-val (parse-val datatype input)
            var-data {:val parsed-val
                      :type :var
                      :datatype datatype}]
        (assoc state varname var-data)))))



(defmethod execute :expression [state [_ child]]
  (evaluate state child)
  state)

;; VAL-RETURNING

(defmethod evaluate :expression [state [_ child]]
  (evaluate state child))

(defmethod evaluate :signExpression [state [_ & children]]
  (if (= 1 (count children))
    (evaluate state (first children))
    (let [var (evaluate state (second children))
          x (:val var)
          val (case (first children)
                "-" (* -1 x)
                "+" x
                "NOT" (not x))]
      {:type :value
       :datatype (datatype val)
       :val val})))

(defmethod evaluate :multiplyingExpression [state [_ & children]]
  (println children)
  {:type :value
   :datatype (datatype val)
   :val (reduce (fn [acc [op a]]
                  (let [x (:val (evaluate state a))]
                    (case op
                      "*" (*' acc x)
                      "/" (/ acc x))))
                (:val (evaluate state (first children)))
                (partition-all 2 (rest children)))})

(defmethod evaluate :addingExpression [state [_ & children]]
  #_(if (= 1 (count children))
      (evaluate state (first children)))
  {:type :value
   :datatype (datatype val)
   :val (reduce (fn [acc [op a]]
                  (let [x (:val (evaluate state a))]
                    (case op
                      "+" (+' acc x)
                      "-" (-' acc x))))
                (:val (evaluate state (first children)))
                (partition-all 2 (rest children)))})

(defmethod evaluate :relationalExpression [state [_ & children]]
  (if (= 1 (count children))
    (evaluate state (first children))
    (let [[a sign b] children
          x (:val (evaluate state a))
          y (:val (evaluate state b))
          val (case sign
                "=" (= x y)
                "<" (< x y)
                "<=" (<= x y)
                ">" (> x y)
                ">=" (>= x y)
                "<>" (not= x y))]
      {:type :value
       :datatype :boolean
       :val val})))

(defmethod evaluate :func [state [_ & children]]
  (if (= 1 (count children))
    (evaluate state (first children))
    (let [exp (second children)]
      (evaluate state exp))))

(defmethod evaluate :literal [state [_ child]]
  (evaluate state child))

(defmethod evaluate :var [state [_ child]]
  (let [[vartype x] child]
    (cond
      (= :varname vartype) (state x))))

(defmethod evaluate :intLiteral [_ [_ val]]
  {:type :value
   :datatype :integer
   :val (Integer/parseInt val)})

(defmethod evaluate :realLiteral [_ [_ val]]
  {:type :value
   :datatype :real
   :val (Double/parseDouble val)})

(defmethod evaluate :strLiteral [_ [_ val]]
  {:type :value
   :datatype :string
   :val (string-lit->string val)})

(defmethod evaluate :charLiteral [_ [_ val]]
  {:type :value
   :datatype :char
   :val (char-lit->char val)})

(defmethod evaluate :boolLiteral [_ [_ val]]
  {:type :value
   :datatype :boolean
   :val (Boolean/parseBoolean val)})
>>>>>>> main
