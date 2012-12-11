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


(ns com.innoq.peg.stringparse
  (:use [com.innoq.peg :as peg]
        [com.innoq.peg.comp :as comp]
        [com.innoq.doc]))

(nsdoc!
 "Stuff that makes parsing strings with peg easier.")


(defn in-range? [chr lower upper] 
  (and (>= (int chr) lower) (<= (int chr) upper)))

(defn in-ranges? [chr & lower-upper] 
  (some (fn [lu] (in-range? chr (lu 0) (lu 1))) lower-upper))


(defn alpha? [c] (or (in-range? c 0x41 0x5A) (in-range? c 0x61 0x7A)))
(doc! alpha? 
      "True if c is in the range "
      "%x41-5A or  %x61-7A")

(defn digit? [c] (in-range? c 0x30 0x39))
(doc! digit? "True iff c represents a digit.")

(defn hexdig? [c] (or (digit? c) (#{\a \b \c \d \e \f \A \B \C \D \E \F} c)))
(doc! hexdig? 
      "True iff c represents a hex digit."
      "Case insensitive.")

(defn pct-enc [s] (peg/seqn "pct" s 
                            [#(peg/literal % \%) 
                            #(peg/pred :hexdigit % hexdig?) 
                            #(peg/pred :hexdigit  % hexdig?)]))
(doc! pct-enc 
      "Parser function for a pct encoded char.")

(defn- make-state:chars [text] 
  (struct peg-state (vec (seq text)) 0 (make-memoizer) '() '()))


(defn parse-string [grammar text]
  (ast (grammar (make-state:chars text))))
(doc! parse-string 
      "Apply grammar to the text that is converted to a sequence of characters.")






