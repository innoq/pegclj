;    Copyright (C) 2012 innoQ Deutschland GmbH
;
;    Licensed under the Apache License, Version 2.0 (the "License")
;    you may not use this file except in compliance with the License. 
;    You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
;
;    Unless required by applicable law or agreed to in writing, software distributed 
;    under the License is distributed on an "AS IS" BASIS, 
;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
;    See the License for the specific language governing permissions and limitations 
;    under the License.


(ns #^{:author "Burkhard Neppert"
       :company "InnoQ Deutschland GmbH"}
  com.innoq.peg.comp
  (:require [com.innoq.peg :as peg])
  (:use     [com.innoq.nostalgia]
            [com.innoq.doc]))
(nsdoc! 
 "The functions in this namespace allow to write grammar in a style"
 "similar to EBNF, except that prefix notation is used everywhere."
 "The following combinators are provide:"
 " %  : sequence of rules"
 " .  : sequence of rules but result is put in '(' ...  ')'"
 " |  : choice of rules"
 " +  : one or more matches of the rule"
 " *  : zero or more matches of the rule"
 " l  : matches elememts in '(' ...  ')' , l as list"
 " v  : matches elememts in '[' ... ']' , v as in vector "
"  !  : directly calls other parser function"
"  ->  : apply function to parser result and append to AST"
"  =>  : apply function to parser result and splice to AST (function must return a seq)"
"  -  : consume parser but drop from AST" 
 " <keyword> : rule invocation"
 ""
 "If you do not like the memnoics, modify seqn?, choice? and other predicates."
 "You can extend the set of combinators by extending the cases in expd-rule."
 ""
 "Example grammars: " 
 " EBNF                       #    PEG"
 " S ::= <A> | <B>            #   (S (| :A :B))"
 " A ::= x o y | x <A> y      #   (A (| (% x o y) (% x :A y)))"
 " B ::= x o y y | x <B> y y  #   (B (| (% x o y y ) (% x :B y y)))"
)


(defn symbol-from-keyword [x]
  (symbol (.substring (.toString x) 1)))

(doc! symbol-from-keyword 
      "Create symbol from keyword with equivalent name."
      "Required for rule invocation in grammars."
)

(def rule-id
     (memoize ;; Gleiche Regel => gleiches Symbol
      (fn [x] (cond 
               (and (seq? x) (keyword? (car x))) (symbol-from-keyword (car x))
               (keyword? x)  (symbol-from-keyword x)
               :else  (gensym)))))
(doc! rule-id 
      "Create symbols for rules. "
      "Some rules in the grammar will be created by the expansion process."
      "This function creates the id for reference by other rules")


(defn children  "The 'children' of a rule or '()"
  [rule] (if (seq? rule) (cdr rule) '()))

(comment Classifier functions)
(defn rule-call? [rule] (keyword? rule))
(defn literal? [rule] (or (symbol? rule) 
                          (number? rule) 
                          (string? rule)
                          (char? rule)))
(defn seqn?    [rule] (and (seq? rule)      (= '% (car rule))))
(defn seqn*?   [rule] (and (seq? rule)      (= '. (car rule))))
(defn choice? [rule] (and (seq? rule)       (= '| (car rule))))
(defn choice:max? [rule] (and (seq? rule)   (= '|| (car rule))))
(defn repeat*? [rule] (and (seq? rule)      (= '* (car rule))))
(defn repeat+? [rule] (and (seq? rule)      (= '+ (car rule))))
(defn optional? [rule] (and (seq? rule)     (= '? (car rule))))
(defn predicate? [rule] (and (seq? rule)    (= 'p (car rule))))
(defn drop? [rule] (and (seq? rule)         (= '- (car rule))))
(defn trafo? [rule] (and (seq? rule)        (= '-> (car rule))))
(defn trafo:splice? [rule] (and (seq? rule) (= '=> (car rule))))
(defn fn-call? [rule] (and (seq? rule)      (= '! (car rule))))
(defn list-group? [rule] (and (seq? rule)   (= 'l (car rule))))
(defn vect-group? [rule] (and (seq? rule)   (= 'v (car rule))))

(defn lit-rule [rule]
  (let [arg `state#]
    (list (rule-id rule) [arg] 
          (list 'com.innoq.peg/literal arg (list 'quote rule)))))
(doc! lit-rule
      "Create a list that can be used as function definition"
      "for a \"literal\" rule in expd-rule.")


(defn- rule-expander:one [op]
  (fn [name rule]
    (let [arg `state#]
      (list name  [arg]
            (list op (list 'quote name) arg 
                  (rule-id (car (children rule))))))))
(doc! rule-expander:one 
      "Create a function that creates a function 'f'"
      "that creates a letfn fragment with 'op' as combinator")

(defn- rule-expander:list [op]
  (fn [name rule]
    (let [arg `state#]
      (list name  [arg]
            (list op (list 'quote name) arg 
                  (cons 'list (map rule-id (children rule))))))))


(def seqn-rule (rule-expander:list 'com.innoq.peg/seqn)) 
(def seqn*-rule (rule-expander:list 'com.innoq.peg/seqn*))
(def choice-rule (rule-expander:list 'com.innoq.peg/choice))
(def choice:max-rule (rule-expander:list 'com.innoq.peg/choice:max))
(def optional-rule    (rule-expander:one 'com.innoq.peg/optional))
(def drop-rule       (rule-expander:one 'com.innoq.peg/drop))
(def repeat-rule*    (rule-expander:one 'com.innoq.peg/repeat-*))
(def repeat-rule+    (rule-expander:one 'com.innoq.peg/repeat-+))
(def list-group-rule (rule-expander:one 'com.innoq.peg/list-group))
(def vect-group-rule (rule-expander:one 'com.innoq.peg/vector-group))
(defn predicate-rule [name rule] 
  (let [arg `state#]
    (list name [arg]
          (list 'com.innoq.peg/pred name arg (cadr rule)))))
(defn fn-call-rule [name rule]
  (let [arg `state#]
    (list name [arg] 
          (list (cadr rule) arg))))
(defn trafo-rule 
  ([name rule]
     (let [arg `state#]
       (list name [arg]
             (list 'com.innoq.peg/trafo name arg (rule-id (cddr rule)) (cadr rule)))))
  ([name rule subrule-id]
     (let [arg `state#]
       (list name [arg]
             (list 'com.innoq.peg/trafo name arg subrule-id (cadr rule))))))
(doc! trafo-rule 
      "[name rule subrule-id]" 
      "If matching rules are defined inside the trafo expr the id of the new rule"
      "must be referenced inside the transformation."
      "E.g. (-> (fn [_] ...) (% a b c)) needs the id of the expansion of (% a b c)"
)
(defn trafo:splice-rule 
  ([name rule]
     (let [arg `state#]
       (list name [arg]
             (list 'com.innoq.peg/trafo:splice name arg (rule-id (cddr rule)) (cadr rule)))))
  ([name rule subrule-id]
     (let [arg `state#]
       (list name [arg]
             (list 'com.innoq.peg/trafo:splice name arg subrule-id (cadr rule))))))


(def expd-rule
     (letfn 
      [(add-unique [lst new-elems] 
                   (concat lst 
                           (filter (fn [e] 
                                     (and e 
                                          (not (some (fn [_] (= (car e) (car _))) 
                                                     lst))))
                                   new-elems)))
       (append-expand [new-rule subrules]
                      (reduce add-unique                 
                              (list new-rule)
                              (doall (map (fn [chld] 
                                            (expand (rule-id chld) chld))
                                          subrules))))
       (expand [name rule]
                 (cond 
                  (seqn? rule) 
                  (append-expand
                   (seqn-rule name rule) 
                   (children rule))

                  (seqn*? rule) 
                  (append-expand
                   (seqn*-rule name rule) 
                   (children rule))

                  (choice? rule)
                  (append-expand
                   (choice-rule name rule) 
                   (children rule))

                  (choice:max? rule)
                  (append-expand 
                   (choice:max-rule name rule) 
                   (children rule))

                  (optional? rule)
                  (append-expand
                   (optional-rule name rule) 
                   (children rule))

                  (repeat*? rule)
                  (append-expand
                   (repeat-rule* name rule) 
                   (children rule))

                  (repeat+? rule)
                  (append-expand
                   (repeat-rule+ name rule) 
                   (children rule))

                  (list-group? rule) 
                  (append-expand 
                   (list-group-rule name rule)
                   (children rule))

                  (vect-group? rule) 
                  (append-expand 
                   (vect-group-rule name rule)
                   (children rule))

                  (drop? rule) 
                  (append-expand 
                   (drop-rule name rule)
                   (children rule))

                  (predicate? rule) (list (predicate-rule name rule))

                  (fn-call? rule) (list (fn-call-rule name rule))                  

                  (literal? rule)   (list (lit-rule rule))

                  (rule-call? rule) (list false) 

                  (trafo? rule)    
                  (let [rules* (cddr rule)]
                    (if (keyword? (car rules*)) 
                      (append-expand (trafo-rule name rule) rules*)
                      (let [id* (rule-id (car rules*))]
                        (cons  (trafo-rule name rule id*)
                               (expand id* (car rules*))))))

                  (trafo:splice? rule) 
                  (let [rules* (cddr rule)]
                    (if (keyword? (car rules*)) 
                      (append-expand (trafo:splice-rule name rule) rules*)
                      (let [id* (rule-id (car rules*))]
                        (cons  (trafo:splice-rule name rule id*)
                               (expand id* (car rules*))))))

                  :else ;;rule
                  (throw (Exception. (.toString (list 'PEGCOMP:SYNTAX_ERROR  rule))))
                  ))]
      expand))
(doc! expd-rule
      "Does the rule expansion.")

(defn expand-rule 
  ([rule] (expd-rule (rule-id rule) rule))
  ([name rule] (expd-rule name rule)))
(doc! expd-rule "Wrapper function for expd-rule.")

(defn expand-named-rule [named-rule]
  (expand-rule (car named-rule) (cadr named-rule)))

; Hier ist noch ein TODO. Literale aus
; unterschiedlichen Top-Level Regeln erscheinen
; mehrfach. "mapcat" in Mengenoperation
; ändern.

(defmacro grammar [& named-rules]
  (list 'letfn (vec 
                (mapcat expand-named-rule `~named-rules)) ;; Hier im mapcat das TODO
        (caar `~named-rules)))
(doc! grammar 
      "[& named-rules] creates a parser function from the list of named rules.")


(defmacro def-grammar [name & named-rules]
  (list 'def `~name  `(grammar ~@named-rules)))
(doc! def-grammar 
      "[name & named-rules] defines a parser function from the list of named rules.")


(defmacro grammar* [& named-rules]
  (list 'letfn (vec (mapcat ;(TODO)
                     expand-named-rule `~named-rules))
         (conj  (mapcat 
                 (fn [x#] (list (list 'keyword (list 'quote (car x#)))  (car x#)))
                 `~named-rules) 'hash-map)))
(doc! grammar* 
  "Expands named rules and returns a hashmap with (keyword rule-symbol) as key"
  "and rule fun as value. May be useful for debugging")

(defmacro def-grammar* [name & named-rules]
  (list 'def `~name `(grammar* ~@named-rules)))
(doc! def-grammar* 
  "Defines name as  a hashmap with (keyword rule-symbol) as key"
  "and rule fun as value. May be useful for debugging")

(defmacro grammar-code [& named-rules]
  `(list 'letfn (vec 
                 (mapcat expand-named-rule '~named-rules)) ;; Hier im mapcat das TODO
         (caar '~named-rules)))
(doc! grammar-code
      "Return the code of the grammar expansions of named rules as list."
      "The result does not look too pretty and is mostly for debugging.")
      

(defmacro parse [grammar & tokens]
  `(let [state# (peg/make-state ~@tokens)]
     (peg/ast (~grammar state#))))
(doc! parse "Take grammar, apply it to the tokens and return the parse result.")

(defmacro parse.remainder [grammar & tokens]
  `(let [state# (peg/make-state ~@tokens)
         parsed# (~grammar state#) ]
     {:match (peg/ast parsed#) :rest (drop (peg/position parsed#) (peg/text parsed#))}))
(doc! parse.remainder
      "Take grammar, apply it to the tokens and return the parse result"
      "as hashmap {:match <ast> :rest <unmatched>}")


