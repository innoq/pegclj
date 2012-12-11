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


(ns de.bne.peg.tests
  (:use [com.innoq.peg :as peg]
        [com.innoq.peg.comp]
        [com.innoq.peg.stringparse :as sp]
        [clojure.test]))


(defn error? [l] (= :error (first l)))
(defmacro p-test 
  ([grammar input]
     `(= (parse.remainder ~grammar ~@input) {:match '~input :rest '()}))
  ([grammar input parsed]
     `(= (parse.remainder ~grammar ~@input) {:match '~parsed :rest '()}))
  ([grammar input parsed remaining]
     `(= (parse.remainder ~grammar ~@input) {:match '~parsed :rest '~remaining}))
  )
(defmacro p-fail [grammar & input]
  `(error? (parse ~grammar ~@input)))

(deftest seqn-test
  (def-grammar gAB   (g (% a b)))
  (def-grammar gABC  (g (% a b c)))

  (is (p-test gAB (a b) (a b)))
  (is (p-test gAB (a b c) (a b) (c)))
  (is (p-test gABC (a b c)))
  (is (p-fail gABC a b))
  (is (p-fail gABC 1 b))
)

(deftest seqn*-test
  (def-grammar gAB-   (g (. a b)))
  (def-grammar gABC-  (g (. a b c)))

  (is (p-test gAB- (a b) ((a b))))
  (is (p-test gAB- (a b c) ((a b)) (c)))
  (is (p-test gABC- (a b c) ((a b c))))
  (is (p-fail gABC- a b))
  (is (p-fail gABC- 1 b))
)

(deftest rep-test
  (def-grammar gA+   (g (+ a)))
  (def-grammar gAB+  (g (+ (% a b))))
  (def-grammar gA+B+ (g (% (+ a) (+ b))))
  (def-grammar gA-+B+ (g (% (. (+ a)) (+ b))))
  (def-grammar gA+-B+ (g (% (+ (. a)) (+ b))))
  (is (p-test gA+ (a)))
  (is (p-test gA+ (a a)))
  (is (p-test gA+ (a a a)))
  (is (p-test gA+ (a b) (a) (b)))
  (is (p-test gA+ (a a b) (a a) (b)))
  (is (p-test gAB+ (a b c) (a b) (c)))
  (is (p-test gAB+ (a b a b c) (a b a b) (c)))
  (is (p-test gAB+ (a b a c) (a b) (a c)))
  (is (p-test gA+B+ (a b)))
  (is (p-test gA+B+ (a a b)))
  (is (p-test gA+B+ (a b b)))
  (is (p-test gA+B+ (a a b b)))
  (is (p-test gA+B+ (a a a b b)))
  (is (p-test gA+B+ (a b a c) (a b) (a c)))
  (is (p-test gA-+B+ (a b) ((a) b)))
  (is (p-test gA-+B+ (a a b)     ((a a) b)))
  (is (p-test gA-+B+ (a b b)     ((a) b b)))
  (is (p-test gA-+B+ (a a b b)   ((a a) b b)))
  (is (p-test gA-+B+ (a a a b b) ((a a a) b b)))
  (is (p-test gA-+B+ (a b a c)   ((a) b) (a c)))
  (is (p-test gA+-B+ (a b)       ((a) b)))
  (is (p-test gA+-B+ (a a b)     (((a) a) b))) 
  (is (p-test gA+-B+ (a b b)     ((a) b b)))
  (is (p-test gA+-B+ (a a b b)   (((a) a) b b)))
  (is (p-test gA+-B+ (a a a b b) ((((a) a) a) b b)))
  (is (p-test gA+-B+ (a b a c)   ((a) b) (a c)))
  (is (p-fail gA+ ))
  (is (p-fail gA+ b))
  (is (p-fail gAB+ a))
  (is (p-fail gA+B+ a))
  (is (p-fail gA+B+ b))
  (is (p-fail gA+B+ a a))
  (is (p-fail gA+B+ b b))
)


(deftest pred-test
  (def-grammar gOdd  (g (p odd?)))
  (def-grammar gOdd+ (g (+ (p odd?))))
  (is (p-test gOdd (1)))
  (is (p-test gOdd (1 2) (1) (2)))
  (is (p-test gOdd+ (1 3 5 7)))
  (is (p-test gOdd+ (1 3 5 7 a) (1 3 5 7) (a)))
  (is (p-fail gOdd ))
  (is (p-fail gOdd a b 1))
  (is (p-fail gOdd+ a b 1))
)


(deftest drop-test
  (def-grammar gOdd-    (g (- (+ (p #(and (number? %) (odd? %)))))))
  (def-grammar gEvenOdd- (g (+ (| (- (p odd?)) 
                                  (! (fn [_] (peg/pred _ even?)))))))
  (is (p-test gOdd- (1) ()))
  (is (p-test gOdd- (1 2) () (2)))
  (is (p-test gOdd- (1 3 5 7) ()))
  (is (p-test gOdd- (1 3 5 7 a) () (a)))
  (is (p-test gEvenOdd- (1 2 3 4 5 6 7) (2 4 6)))
)

(deftest trafo:splice-test 
  ;; (def-grammar gSplicSym (g (=> (fn [_] 'x) (% a b c)))) fn must return a seq for trafo:splice 
  (def-grammar gTrafo (g (% (-> (fn [_] (reverse _)) (+ (| a b c))) 
                            d)))
  (def-grammar gSplice (g (% (=> (fn [_] (reverse _)) (+ (| a b c)))
                             d)))
  (is (p-test gSplice (c b a d) (a b c d)))
  (is (p-test gTrafo (c b a d) ((a b c) d))) ;; Result in ()
)

(deftest fncall-test
  ;; Shows how different grammars and literal functions
  ;; can be combined
  ;; 
  (def-grammar* gXYZ 
    (X (% x)) 
    (Y (% y))
    (Z (% z)))
  (def-grammar gzyx-Q
    (g (% (! (gXYZ :Z))
          (! (gXYZ :Y))
          (! (gXYZ :X))
          (- (+ (! (fn [_] (literal _ 'Q))))))))
  (is (p-test gzyx-Q (z y x Q) (z y x)))
  (is (p-test gzyx-Q (z y x Q x) (z y x) (x)))
)

