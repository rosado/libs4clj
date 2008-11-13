;; simple processing wrapper for Clojure
;; Roland Sadowski [szabla gmail com]
;; This code is in the public domain

(ns rosado.math.vector2d
  (:import (java.lang Math)))

(defn norm [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn normalize [[x y]]
  (let [nm (norm [x y])]
	[(/ x nm) (/ y nm)]))

(defn sub
  "Returns the difference of two 2d vectors"
  [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn add
  "Returns the sum of two 2d vectors"
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn mul
  [[x y] s]
  [(* x s) (* y s)])

(defn div [[x y] s]
  (mul [x y] (/ 1 s)))

(defn dot
  [[x y] [u v]]
  (+ (* x u) (* y v)))
