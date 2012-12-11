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
       :company "InnoQ Deutschland GmbH" }
  com.innoq.peg
  (:use [com.innoq.nostalgia]
        [com.innoq.doc]))
  
(nsdoc! "Recursive descend parsing."
        "Combinators for parsers."
        "Usual limitations apply : no left recursive grammars.")
(defstruct peg-state 
  :text       
  :position   
  :memoizer   
  :stack      
  :ast)       
(doc! peg-state 
      "Struct peg-state holds the information about the parser input and result."
      "A parser functions is any function that takes a peg-state as input and "
      "create a new as result."
      ":text        : the input to parse. Must implement indexed element access."
      ":position    : the read position in the input."
      ":memoizer    : memory for intermediate parser results."
      ":stack       : rule call stack. Used for detection of recursion, not much more."
      ":ast         : the result produced so far." 
)

(defn text     [state] (state :text))
(defn position [state] (state :position))
(defn memoizer [state] (state :memoizer))
(defn stack    [state] (state :stack))
(defn ast      [state] (state :ast))
(defn size     [state] (count (text state))) ; ; (*)

(defn make-memoizer [] (atom {}))

(defmacro make-state [& args]
  `(struct peg-state 
           (apply vector  '~args)
           0        ;; position
           (make-memoizer) 
           '()      ;; stack
           '()))

(doc! make-state "Create an initial parsesr state from arguments.")

(defn print-state [state]
  (println (text state) " memoizer:" (hash  @(memoizer state)) "with"
           (count (keys  @(memoizer state))) "entries")
  (println (position state) (ast state)))
(doc! print-state 
      "The state is a recursive structure, better not print it directly."
      "Use this function for output")

(defn failed?  
  "True iff the state is in error"
  [state] (= :error (first (ast state))))

(defn eoi?
  "True iff all input consumed"
  ([state] (>= (position state) (size state))))

(defn finished? 
  "True if all input consumed without error"
  [state]
  (and (eoi? state) (not (failed? state))))

(defn terminated?
  "True iff we cannot proceed, either because we failed or there is no input left"
  [state] 
  (or (failed? state) (eoi? state)))



(defn next-token [state]  ; (*)
  (if (terminated? state)
    :error 
    (nth (text state) 
         (position state))))
(doc! next-token 
     "Return the next token to be processed."
     "Iff there is nothing left, return :error ."
)

(defn- append [l & es] (concat l es))

(defn consume-token
  ([state]
     (consume-token state 1))
  ([state n-tokens-consumed]
     (assoc state 
       :position (+ n-tokens-consumed 
                    (position state)) 
       :ast (append (ast state) 
                    (next-token state))))
  ([state result n-tokens-consumed]
     (assoc state
       :position (+ n-tokens-consumed 
                    (position state)) 
       :ast (if (= result :peg/ignore) 
              (ast state)
              (append (ast state) result)))))
(doc! consume-token
      "[state] : return a new state where the first token from input is appended to the "
      "          result and the read position is advanced by 1."
      "[state ntoken] :  return a new state where the first ntokens tokens from input are"
      "          appended to the  result and read position is advanced by ntokens."
      "[state result ntoken] : return a new state with read position advanced by ntoken"
      "          and the element \"result\" appended to the ast."
      "          !! If the element \"result\" is the keyword :peg/ignore then the resulting"
      "           AST will be unchanged but the input position advanced by ntokens.")

(defn consume-token:splice
  ([state result n-tokens-consumed]
     (assoc state
       :position (+ n-tokens-consumed 
                    (position state)) 
       :ast (if (= result :peg/ignore) 
              (ast state)
              (concat (ast state) result)))))
(doc! consume-token:splice
      "Used if the result is a seq where the elements should be appended and not the list."
      "Example: (ast state) is (a b c) and parse result is (d e f)."
      "     then consume-token will produce (a b c (d e f))) "
      "     and consume-token:splice will produce (a b c d e f) .")


(defmacro memoize-if! [state rule-key expr]
  `(let [key# (list (position ~state) ~rule-key)
         value# ((deref (memoizer ~state)) key#)]
     (if value#
       value# 
       (let [newstate# ~expr]
         (swap! (memoizer ~state) 
                assoc key# newstate#)
         newstate#))))

(doc! memoize-if!  
      "If \"state\" does not contain a result for \"rule-key\" "
      "evaluate \"expr\" and associate the resulte in state's memoizer.")

(defn recursion? [state parser]
  (some #(= % (list (state :position) parser))
        (stack state)))
(doc! recursion? 
      "Return true iff state's class stack alreadey called parser at current position."
)

(defn enter-parser [state parser]
  (assoc state :stack (conj (state :stack) 
                            (list (state :position) parser))))
(doc! enter-parser "Prepare new state for call of parser.")

(defn exit-parser [state]
  (assoc state :stack (cdr (state :stack))))
(doc! exit-parser "Return state with last parser call from state's stack removed." )


;; Now a list of parser functions.

(defn fail      
  ([state] (fail state fail))
  ([state parser] (assoc state :ast 
                         (list :error parser 
                               :at (position state)))))
(doc! fail
      "Applying this to a state createds an error state."
      "If parser is supplied as snd argument it will be used in the :error part.")

(defn literal [state sym] 
  (if (= sym (next-token state))
    (consume-token state)
    (fail state sym)))
(doc! literal 
      "[state sym] : if the toke in state's read position is = sym," 
      "              return a new state with token added to AST."
      "              Else return an error state.")


(defn drop
  ([state parser] (drop :drop state parser))
  ([name state parser]
     (let [state* (parser state)] 
       (if (failed? state*) 
         state*
         (assoc state* :ast (ast state))))))
(doc! drop 
      "applies parser to state and returns a state with read pos advanced "
      "to reflect tokens consumed by parser but with unmodified ast.")


(defn pred 
  ([state token-pred] (pred (list :pred token-pred) 
                            state token-pred))
  ([rule-name state token-pred]
     (cond (failed? state) state
           (eoi? state) (fail state rule-name)
           :else (if (token-pred (next-token state))
                   (consume-token state)
                   (fail state rule-name)))))
(doc! pred 
      "[rule-name state token-pred] Apply token-pred to first input token."
      "If the result is neither nil nor #f the token is append to the AST."
      "A result of token-pred of nil or #f creates a failed state."
      " rule-name is used for error information only." )


(defn seqn 
  ([state rules] (seqn (list :seqn rules) state rules))
  ([rule-name state rules]
     (memoize-if! state 
                  rule-name
                  (if (terminated? state)
                    state
                    (loop [rules* rules
                           s state]
                      (if (empty? rules*)
                        s
                        (let [s* ((first rules*) s)]
                          (if (failed? s*)
                            s*
                            (recur (next rules*) s*)))))))))
(doc! seqn 
      "[rule-name state rules] rules must be a list of parser functions."
      "This parser combinator applies all parsers from rules the result of" 
      "the previous rule until the first parser from rule fails or all "
      "parsers are applied. E.g. (seqn s0 [f g h]) => (h (g (f s0)))"
      ""
      "As usual, rule-name is used for error information only." )

(defn seqn* 
  ([state rules] (seqn* (list :seqn* rules) state rules))
  ([rule-name state rules]
    (memoize-if! state 
                 rule-name
                 (cond 
                  (failed? state) state
                  (eoi? state)    (assoc state :ast (list (ast state)))
                  :else (loop [rules* rules
                               s state]
                          (if (empty? rules*)
                            (assoc s :ast (list (ast s)))
                            (let [s* ((first rules*) s)]
                              (if (failed? s*)
                                s*
                                (recur (next rules*) s*)))))))))
(doc! seqn* 
      "[rule-name state rules] rules must be a list of parser functions."
      "This parser combinator applies all parsers from rules to the state"
      "until the first parser from rule fails or all parsers are applied."
      "The result of all parsers is applied to the AST wrapped in a list."
      ""
      "As usual, rule-name is used for error information only." )


(defn choice 
  ([state rules] (choice (list :choice rules) state rules))
  ([rule-name state rules]
     (memoize-if! state
                  rule-name
                  (if (terminated? state)
                    state
                    (loop  [rules* rules]
                      ;; (println (position state)(car rules*))
                      (if (empty? rules*)
                        (fail state rule-name)
                        (let [r* (car rules*)]
                          (if (recursion? state r*) ;; Rekursion ?
                            (do ;; ja, parser überspringen
                              (println "Recursion at" (position state) r*)
                              (recur (cdr rules*)))    
                            (let [s* (exit-parser (r* (enter-parser state r*)))]
                              (if (failed? s*)
                                (recur (cdr rules*))
                                s*))))))))))
(doc! choice 
      "[rule-name state rules] rules must be a sequence of parser functions."
      "Choice applies the parser fn from rules in order until the first parser"
      "succeeds. This result will be returned or a failed state if no parser succeeds."
      ""
      "As usual, rule-name is used for error information only." )

(defn parse-max [state-a state-b]
  (if (> (position state-b) (position state-a))
    state-b 
    state-a))

(defn choice:max 
  ([state rules] (choice:max (list 'choice:max rules) 
                             state rules))
  ([rulename state rules] 
     ;; (println "---- choice:max REKURSION CHECK NOT IMPLEMENTED  ------")
     (cond (failed? state) state
           (eoi? state) (fail state rulename)
           :else (loop [pending state
                        parsed ((car rules) state)
                        rules (cdr rules)]
                   (if (empty? rules) 
                     (parse-max parsed pending)
                     (recur (parse-max parsed pending)
                            ((car rules) state)
                            (cdr rules)))))))

(doc! choice:max
      "[rule-name state rules] like choice except that not the first"
      "non-fail result is returned but that of the parser which advances"
      "the read position the most."
      ""
      "As usual, rule-name is used for error information only." )

(defn trafo 
  ([state parser sem-fn] (trafo (list :trafo parser sem-fn)
                                state parser sem-fn))
  ([rule-name state parser sem-fn]
     (let [result (parser state)]
       (if (failed? result)
         (fail state rule-name)
         (let [n-tokens-consumed (- (position result) 
                                    (position state))
               new-results (take-last (- (count (ast result))
                                         (count (ast state)))
                                      (ast result))]
           (consume-token state
                          (sem-fn new-results)
                          n-tokens-consumed))))))
(doc! trafo 
      "[rule-name state parser sem-fn] transformation of parser result."
      "This function applies parser to  state. If the parser succeeds"
      "then sem-fn is applied to the  parsed tokens and the application"
      "result added to the new AST (instead of the parsed tokens)."
      ""
      "As usual, rule-name is used for error information only." )


(defn trafo:splice 
  ([state parser sem-fn] (trafo:splice (list :trafo:splice parser sem-fn)
                                       state parser sem-fn))
  ([rule-name state parser sem-fn]
     (let [result (parser state)]
       (if (failed? result)
         (fail state rule-name)
         (let [n-tokens-consumed (- (position result) 
                                    (position state))
               new-results (take-last (- (count (ast result))
                                         (count (ast state)))
                                      (ast result))]
           (consume-token:splice state
                                 (sem-fn new-results)
                                 n-tokens-consumed))))))
(defn optional 
  ([state f] (optional (list :optional f) state f))
  ([rulename state f]
     (let [state* (f state)]
       (if (failed? state*)
         state
         state*))))
(doc! optional
      "[rulename state f] applies f to state and return the result on success."
      "if f fails return the original state."
      ""
      "As usual, rule-name is used for error information only." )


(defn repeat-n 
  ([state f n] (repeat-n (list :repeat-n n f) state f n))
  ([rulename state f n]
     (if (or (failed? state) 
             (<= n 0))
       state
       (loop [s (f state)
              i (- n 1)]
         (if (= 0 i)
           s
           (recur (f s) (- i 1)))))))
(doc! repeat-n 
      "[rulename state f n] repeatedly applies f n times  to state."
      "E.g. (repeat-n s0 f 3) => (f (f (f s0)))" 
      ""
      "As usual, rule-name is used for error information only." )



(defn repeat-* 
  ([state f] (repeat-* (list :repeat-* f) state f))
  ([rulename state f]
     (if (failed? state) 
       state
       (loop [s  state
              s* (f state)]
         (cond (failed? s*) s
               (eoi? s*)    s*
               :else        (recur s* (f s*)))))))
(doc! repeat-* 
      "[rulename state f]  apply f as many times as possible."
      "The last state of succesful rule application is returned."
      ""
      "As usual, rule-name is used for error information only." )

(defn repeat-+ 
  ([state f] (repeat-+ (list :repeat-+ f) state f))
  ([rulename state f]
     (if (failed? state) 
       state
       (loop [s  (f state)
              s* (f s)]
         (cond (failed? s*) s
               (eoi? s*)    s*
               :else        (recur s* (f s*)))))))
(doc! repeat-+ 
      "[rulename state f]  apply f as many times as possible."
      "The last state of succesful rule application is returned."
      "f must succeed at least once or this application will return"
      "a failed state."
      ""
      "As usual, rule-name is used for error information only." ) 

(defn list-group 
  ([state f] (list-group (list :list-group f) state f))
  ([rulename state f]
     (let [grp (next-token state)]
       (if (seq? grp)
         (let [res (f (struct peg-state grp 0 (make-memoizer) '() '()))]
           (if (eoi? res)
             (consume-token state (ast res) 1)
             (fail state rulename)))
         (fail state rulename)))))
(doc! list-group 
      "[rule-name state f] matches f against the elements inside of a list."
      ""
      "As usual, rule-name is used for error information only." ) 


(defn vector-group 
  ([state f] (vector-group (list :vector-group f) state f))
  ([rulename state f]
     (let [grp (next-token state)]
       (if (vector? grp)
         (let [res (f (struct peg-state grp 0 (make-memoizer) '() '()))]
           (if (eoi? res)
             (consume-token state (ast res) 1)
             (fail state rulename)))
         (fail state rulename)))))
(doc! vector-group 
      "[rule-name state f] matches f against the elements inside of a vector."
      ""
      "As usual, rule-name is used for error information only." ) 

(defn epsilon "Parser succeeds whithout consumption."
  ([state] state))

(defn numeric "This parser recognizes any numeric clojure token"
  [state]
   (if (number? (next-token state))
     (consume-token state)
     (fail state :numeric)))
