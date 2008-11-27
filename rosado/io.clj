;; simple io abstraction layer for clojure
;; Roland Sadowski [szabla gmail com] http://www.haltingproblem.net

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software. 

(ns rosado.io
  (:import (java.io File FileReader FileWriter BufferedReader BufferedWriter
					FileInputStream FileOutputStream FileOutputStream
					BufferedInputStream BufferedOutputStream)))

(defn reader 
  "Returns a reader or input stream (buffered).
  Optional keywords: :unbuff [:ub] - for unbuffered input. 
  :bytes - for an InputStream instead of a Reader."
  [file-name & modes]
  (cond
   (nil? modes) (BufferedReader. (FileReader. file-name))
   (some #{:bytes} modes) (if (some #{:unbuff :ub} modes)
							(FileInputStream. file-name)
							(BufferedInputStream. (FileInputStream. file-name)))
   (some #{:unbuff :ub} modes) (FileReader. file-name)))

(defn read-lines 
  "Returns a vector of lines from reader.
 The lines don't include line termination characters"
  [#^java.io.BufferedReader rdr]
  (loop [lines [] line (.readLine rdr)]
	(if (nil? line)
	  lines
	  (recur (conj lines line) (.readLine rdr)))))

(defn writer
  "Returns a writer or output stream (buffered).
  Optional keywords: :unbuff [:ub] - for unbuffered output. 
  :bytes - for an OutputStream instead of a Writer."
  [file-name & modes]
  (cond
   (nil? modes) (BufferedWriter. (FileWriter. file-name))
   (some #{:bytes} modes) (if (some #{:unbuff :ub} modes)
							(FileOutputStream. file-name)
							(BufferedOutputStream. (FileOutputStream. file-name)))
   (some #{:unbuff :ub} modes) (FileWriter. file-name)))

(defn write-lines 
  "Writes a seq of lines to a writer. Appends a newline to ecah line."
  [#^java.io.BufferedWriter a-writer lines]
  (loop [lns lines]
	(when lns
	  (.write a-writer #^String (first lns))
	  (.newLine a-writer)
	  (recur (rest lns)))))

;; compares mtime of files
;; no errors/exceptions are thrown
(defmulti #^{:doc "Returns true if file (first arg.) is newer than the
  other (mtime comparison). Accepts strings or java.io.File's"}
  newer? (fn [file other] (class file)))

(defmethod newer? java.io.File
  [#^File file #^File other]
  (> (.lastModified file) (.lastModified other)))

(defmethod newer? java.lang.String
  [#^String file #^String other]
  (> (.lastModified (File. file)) (.lastModified (File. other))))

(defmulti #^{:doc "Returns true if argument file/directory exists.
  Accepts strings or java.io.File's"}
  exists? class)
(defmethod exists? java.io.File [#^File f] (.exists f))
(defmethod exists? String [#^String fname] (.exists (File. fname)))

(defmulti #^{:doc "Returns true if argument is a directory."}
  directory? class)
(defmethod directory? java.io.File [#^File f] (.isDirectory f))
(defmethod directory? String [#^String fname] (.isDirectory (File. fname)))

(defmulti #^{:doc "Lists contents of a directory. Returns nil if arg is a file."}
  list-dir class)
(defmethod list-dir java.io.File [#^File f] (.listFiles f))
(defmethod list-dir String [#^String fname] (.listFiles (File. fname)))

(defmulti #^{:doc "Deletes a file or directory (with its contents)."}
  delete class)

(defn- delete-dir [#^java.io.File dir-name]
  (doseq [f (list-dir dir-name)]
	  (cond (not (directory? f)) (delete f)
			:else (delete-dir f)))
  (.delete dir-name))

(defmethod delete java.lang.String [#^java.lang.String fname]
  (delete (File. fname)))

(defmethod delete java.io.File [#^java.io.File fname]
  (if (exists? fname)
	(if (directory? fname)
	  (delete-dir fname)
	  (.delete fname))))

(defn mkdir
  "Creates directories, including necessary parent dirs."
  [& dirs]
  (doseq [#^String dir dirs]
	  (-> (File. dir) .mkdirs)))