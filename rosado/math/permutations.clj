;; generating permutations etc
;; Roland Sadowski [szabla gmail com]

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software. 

(ns rosado.math.permutations)

(def *order*) ;; order of permutation

(defn factoradic
 "Computes the factoradic of integer k."
 ([k] (factoradic k *order*))
 ([k ord]
	(loop [j 1 f nil t k]
	  (if (<= j ord)
		(recur (inc j) (cons (rem t j)  f) (int (/ t j)))
		f))))

(defn fact->perm
  "Converts a factoradic to permutation of numbers [0, n-1], where
  n=(count fct)."
  [fct]
  (loop [rfct (rest (reverse (map inc fct))) p '(1)]
	(if rfct
	  (recur (rest rfct)
			 (cons (first rfct)
				   (map #(if (<= (first rfct) %1) (inc %1) %1) p)))
	  (map dec p))))

(defn int->perm
  "Converts an integer to a permutation of numbers [0, order-1]"
  ([k] (fact->perm (factoradic k)))
  ([k ord] (fact->perm (factoradic k ord))))

(defn seek-left [v]
  (let [iter (range (- (count v) 2) -1 -1)]
	  (map #(if (< (v %1) (v (inc %1)))
			  %1
			  false)
		   iter)))

(defn seek-right [v left-pos]
  (if left-pos
	  (let [iter (range (dec (count v)) -1 -1) left (v left-pos)] 
		(map #(if (< left (v %1))
				%1
				false)
			 iter))))

(defn swap-at [v pos-a pos-b]
  "Swaps elements at positions pos-a and pos-b in vector v."
  (let [val-a (nth v pos-a) val-b (nth v pos-b)] pos-a
	(assoc (assoc v pos-a val-b) pos-b val-a)))

(defn order-tail [perm l r]
  "Order elements in range [l,r] in permutation."
  (loop [i l j r s perm]
	(if (< i j)
	  (recur (inc i) (dec j) (swap-at s i j))
	  s)))

(defn successor [p]
  "Returns a successor of a permutation p (or nil). p must be a vector."
  (let [l (first (for [a (seek-left p) :when a] a))
		r (first (for [a (seek-right p l) :when a] a))]
	(if (or (not l) (and (= l 0) (> (p l) (p r))))
	  nil
	  (order-tail (swap-at p l r) (inc l) (dec (count p))))))

(defn permute [col perm]
  "Permutates given collection according to perm."
  (let [len (count col), blank-vec (vec (take len (repeat 0)))]
	(loop [cnt (range len), v blank-vec, p perm]
	  (if cnt
		(recur (rest cnt) (assoc v (first cnt) (get col (first p))) (rest p))
		v))))

(defn inverse [perm]
  "Returns the inverse of a permutation."
  (let [len (count perm) np (vec (take len (repeat 0)))]
	(loop [i (range len) v np p perm]
	  (if i
		(recur (rest i) (assoc v (first p) (first i)) (rest p))
		v))))

;; sample
;; (binding [*order* 3]
;;   (doseq [i (range 6)]
;; 	 (prn (fact->perm (factoradic i)))))

;; (binding [*order* 3]
;;   (let [col [:a :b :c]]
;; 	(permute col (int->perm 3))))

