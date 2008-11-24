;; utility functions for Clojure
;; Roland Sadowski [szabla gmail com] http://www.haltingproblem.net

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software. 

(ns rosado.utils
  (:require [rosado.io :as io])
  (:import (java.io InputStreamReader BufferedReader ))
  (:import (java.util Collections Arrays)))

(defn read-all
  "Returns a string read from input stream."
  [in-stream]
  (let [rdr (BufferedReader. (InputStreamReader. in-stream))
		sb (StringBuilder. )]
	(loop [c (.read rdr)]
	  (if (not= c -1)
		(let []
		  (.append sb (char c))
		  (recur (.read rdr)))
		(.toString sb)))))

(def #^{:private true} p-info)
(def #^{:private true} *p*)

(defn- remember-pi
  "Utility fn for mutating a var in run-process."
  [kw val]
  (set! p-info (assoc p-info kw val)))

(defn run-command [#^String cmd-str]
  (let [params (java.util.ArrayList.)
		pb (ProcessBuilder. (Arrays/asList (.split cmd-str " ")))]
	(.redirectErrorStream pb true)
	(try
	 (binding [p-info {} *p* (.start pb)]
	   (remember-pi :in-stream (.getInputStream *p*))
	   (.waitFor *p*)
	   (remember-pi :output (read-all (p-info :in-stream)))
	   (.destroy *p*)
	   (if (not= 0 (.exitValue *p*))
		 (let []
		   (println "Error executing command: " cmd-str)
		   (print (:output p-info))
		   :fail)
		 (let []
		   (print (:output p-info))
		   :ok)))
	 (catch java.io.IOException ex :fail))))

