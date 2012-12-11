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


(ns com.innoq.nostalgia)

(def  car first)
(def  cdr rest)

(defn caar   [l] (ffirst l))
(defn cadr   [l] (first (rest l)))
(defn cdar   [l] (rest (first l)))
(defn cddr   [l] (rest (rest l)))

(defn caaar  [l] (first (ffirst l)))
(defn caadr  [l] (ffirst (rest l)))
(defn cadar  [l] (first (rest (first l))))
(defn cdaar  [l] (rest (ffirst l)))
(defn caddr  [l] (first (rest (rest l))))
(defn cdadr  [l] (rest (first (rest l))))
(defn cddar  [l] (rest (rest (first l))))
(defn cdddr  [l] (rest (rest (rest l))))

(defn caaaar [l] (ffirst (ffirst l)))
(defn cddddr [l] (rest (rest (rest (rest l)))))

(defn nth-cdr [l n]
  (if (or (> 1 n) (empty? l))
    l
    (recur (cdr l) (dec n))))
