# Naive parser generator for clojure

This package implements a simple parser generator for clojure. 
It uses parser combinators with a recursive decend strategy.
Left recursive grammars are not supported,
The functions in this namespace allow to write grammar in a style
similar to EBNF, except that prefix notation is used everywhere.
The following combinators are provide:
+ %  : sequence of rules
+ .  : sequence of rules but result is put in '(' ...  ')'
+ |  : choice of rules
+ +  : one or more matches of the rule
+ *  : zero or more matches of the rule
+ l  : matches elememts in '(' ...  ')' , l as list
+ v  : matches elememts in '[' ... ']' , v as in vector 
+ !  : directly calls other parser function
+ ->  : apply function to parser result and append to AST
+ =>  : apply function to parser result and splice to AST (function must return a seq)
+ -  : consume parser but drop from AST
+ <keyword> : rule invocation"

Example grammar: 
        EBNF                       #    PEG
                                   #   (def-grammar AB                            
        S ::= <A> | <B>            #     (S (| :A :B))
        A ::= x o y | x <A> y      #     (A (| (% x o y) (% x :A y)))
        B ::= x o y y | x <B> y y  #     (B (| (% x o y y ) (% x :B y y))))

Use:
      (parse AB x x x o y y y y)  -> (x x x o y y y) ;; The last y is not parsed. 
or       
      (parse.remainder AB x x x o y y y y)  -> 
                       {:match (x x x o y y y)  :rest (y) }