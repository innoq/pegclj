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

(ns com.innoq.doc)

; The author of this (Burkhard Neppert) is not so happy with the documentation syntax.
; But that's a matter of taste. 

(defmacro doc! [sym & text] 
  `(alter-meta! (var ~sym) 
                (fn [m#] (assoc m# :doc (apply str  (interpose \newline '~text))))))

(doc! doc! "Create :doc meta info for a symbol .")

(defmacro nsdoc! [& text] 
  `(alter-meta! *ns* 
                (fn [m#] (assoc m# :doc (apply str  (interpose \newline '~text))))))

(doc! nsdoc! "Create :doc meta for the current name space.")

(defmacro add-doc! [sym & text] 
  `(alter-meta! (var ~sym) 
                (fn [m#] (assoc m# :doc (apply str  (cons (m# :doc) (cons \newline (interpose \newline '~text))))))))
(add-doc!  doc!
     "Use add-doc! to append comments to end of existiting comments")

