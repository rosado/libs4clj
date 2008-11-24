;; roland sadowski [szabla gmail com] http://www.haltingproblem.net

;; Copyright (c) 2008 Roland Sadowski. All rights reserved.  The use and
;; distribution terms for this software are covered by the Common
;; Public License 1.0 (http://www.opensource.org/licenses/cpl1.0.php)
;; which can be found in the file CPL.TXT at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software. 

(ns rosado.math.graph
  (:use clojure.set))

;; :type can be : #{:directed :undirected}
(defstruct graph-info :type)
(defstruct vertex :meta :out)
(defstruct edge-info :dest :weight)

(defmacro get-type [g] `((first ~g) :type))

(defn directed? [g]
  "Returns true if graph is a directed graph."
  (= (get-type g) :directed))

(defn make-vertex
  [m out]
  (struct vertex m out))

(defn make-graph
  "Creates an 'empty' graph."
  ([num-verts]
	 (make-graph num-verts (struct graph-info :directed)))
  ([num-verts g-info]
	 (let [g (vec (take (inc num-verts)(repeat nil)))]
	   (assoc g 0 g-info))))

(defn alter-vertex
  "Set the vertex v of graph g to vertex new-v."
  [g v new-v]
  (assoc g v new-v))

(defn add-edge
  "Add edge from verex v1 to v2 to graph g.
  Both vertices must already be in g."
  [g [v1 v2]]
  (let [v (g v1) 
		vert (if v v (empty g))
		new-g (alter-vertex g v1 (make-vertex (vert :meta)
											  (conj (vert :out) v2)))]
	(cond (new-g v2) new-g
		  :else (alter-vertex new-g v2 (make-vertex nil nil)))))

(defn add-vertex
  "Adds a vector to graph g. Returns the graph on success,
  nil on failure (when given vertex is already in g)."
  ([g k v]
	 (if (g k)
	   nil
	   (alter-vertex g k v)))
  ([g k]
	 (if (g k)
	   nil
	   (alter-vertex g k (make-vertex nil nil)))))

(defn adjacent-to
  ([v]
	 (if v
	   (v :out)))
  ([g v]
	 (if-let [vert (g v)]
	   (vert :out))))

(defn degree
  "Returns the out-degree of a vertex."
  [vert]
  (when vert
	(count (adjacent-to vert))))

(defn tag?
  "Returns the data associated with vertex with given key (nil
  otherwise)"
  ([g v key]
	 (if-let [vert (g v)]
	   (tag? vert key)))
  ([v key]
	 (if-let [m (v :meta)]
	   (m key))))

(defn tag-vertex
  "Associates given data with a vertex with given key."
  ([g v key val]
	 (tag-vertex g v {key val}))
  ([g v mta]
	 (if-let [vert (g v)]
	   (alter-vertex g v (tag-vertex vert mta))
	   g))
  ([v mta]
	 (make-vertex (merge (v :meta) mta)
				  (v :out))))

(defn alter-adj-list-of
  "Returns the graph with the adjacency list of vertex v changed to list."
  [g v lst]
  (alter-vertex g v (make-vertex ((g v) :meta) lst)))

(defn delete-edge
  ([g [a b]]
	 (alter-adj-list-of g
						a
						(filter #(not= b %)
								(adjacent-to g a)))))

(defn delete-vertex
  "Returns the graph with vertex v removed."
  [g v]
  (let [new-g    (alter-vertex g v nil)
		vertices (for [i (range 1 (count new-g)) :when (not= (new-g i) nil)] i)
		rm-fn    (fn [vert] (not= v vert))]
	(loop [verts vertices graph new-g]
	  (if verts
		(recur (rest verts)
			   (alter-adj-list-of graph
								  (first verts)
								  (filter rm-fn
										  (adjacent-to new-g
													   (first verts)))))
		graph))))

(defn- flatten-pairs [coll]
  (mapcat identity coll))

(defn pairs->graph
  "Produces a graph from a list of ordered pairs [u v],
  where u, v -- vertices."
  [pairs]
	 (let [vert-indices (into #{} (flatten-pairs pairs))
		   empty-graph  (make-graph (last (sort vert-indices)))
		   graph (loop [g empty-graph indices vert-indices]
				   (if indices
					 (recur (add-vertex g (first indices)) (rest indices))
					 g))]
	   (reduce add-edge (cons graph pairs))))

(defn get-valid-indices
  "Returns a seq of indices for which vertices are set in given
  graph."
  [g]
  (filter #(not= nil %) (range 1 (count g))))

(defn eulerian?
  "Returns true if graph is Eulerian, false otherwise."
  [g vi ui]
  (let [dv (degree (g vi))
		du (degree (g ui))
		gsize (count g)]
	 (if (not= 0 (rem (+ dv du) 2))
	   false
	   (let [other-verts (filter #(and %
									   (not= ui %)
									   (not= vi %))
								 (range 1 gsize))]
		 (loop [verts other-verts]
		   (if verts
			 (if (not= 0 (rem (degree (g (first verts))) 2))
			   false
			   (recur (rest verts)))
			 true))))))

;; (defn euler-path
;;   "Finds Euler path in a graph. Assumes the path exists."
;;   [g vi]
;;   (let [epath (fn [gr v st]
;; 				(loop [stack st vert v graph gr]
;; 				  (let [adj (adjacent-to graph vert) w (first adj)]
;; 					(if adj
;; 					  (recur (cons w stack)
;; 							 w
;; 							 (-> graph
;; 								 (delete-edge [vert w])
;; 								 (delete-edge [w vert])))
;; 					  [graph stack vert]))))]
;; 	(loop [pvert vi
;; 		   [graph stack vert] (epath g vi '())
;; 		   path []]
;; 	  (cond (and (= pvert vert) (not (empty? stack)))
;; 				(recur (first stack)
;; 					   (epath graph (first stack) (rest stack))
;; 					   (conj path (first stack)))
;; 			:else path))))

(defn discovered? 
  ([g vi]
	 (tag? g vi :pre))
  ([vert]
	 (tag? vert :pre)))

(defstruct dfs-args :graph :pre :post)

;; customizable depth first search

(def #^{:private true} *dfs-internal*)
(def #^{:private true} *m*)								; holds the arg-map in main loop in internal-dfs
(def #^{:private true} *wi*) 								; index of 
(def #^{:private true} *vi*)
(def #^{:private true} *v*)								; index of current vert. in main loop in internal-dfs
(def *verts*) 							; holds vertices adjacent to current verts in internal dfs

(def #^{:private true} *mark-pre*)
(def #^{:private true} *mark-pre-fn*)
(def #^{:private true} *mark-post*)
(def #^{:private true} *mark-post-fn*)
(def #^{:private true} *increment-pre*)
(def #^{:private true} *increment-pre-fn*)
(def #^{:private true} *increment-post*)
(def #^{:private true} *increment-post-fn*)
(def #^{:private true} *increment-component*)
(def #^{:private true} *increment-component-fn*)
(def #^{:private true} *mark-component*)
(def #^{:private true} *mark-component-fn*)

(def #^{:private true} *tree-eg?*)
(def #^{:private true} *back-eg?*)
(def #^{:private true} *down-eg?*)
;; no cross-eg

(def #^{:private true} *tree-eg?-fn*)
(def #^{:private true} *back-eg?-fn*)
(def #^{:private true} *down-eg?-fn*)

;; for symbols
(def #^{:private true} *tree-eg-hook*)
(def #^{:private true} *back-eg-hook*)
(def #^{:private true} *down-eg-hook*)
(def #^{:private true} *cross-eg-hook*)

;; for data (code)
(def #^{:private true} *tree-eg-hook-fn*)
(def #^{:private true} *back-eg-hook-fn*)
(def #^{:private true} *down-eg-hook-fn*)
(def #^{:private true} *cross-eg-hook-fn*)

(defn- make-fn-map [bds]
  (reduce #(assoc %1 (first %2) (second %2)) {} bds))

(defn- verify-fn-map [fn-map]
  (let [required #{:tree-edge? :increment-pre :mark-pre-visited}]
   (if (= (intersection (set (keys fn-map)) required) required)
	 fn-map
	 (throw (Exception. "Required functions not present.")))))

(defn- make-let-pair
  "Returns syntax quoted pair if b is not nil."
  [[a b]]
  (if b
	`(~a ~b)
	nil))

(defn- component-key-val-pair [m]
  (if *increment-component-fn*
	`(assoc ~m :components 0)
	m))

(defn- mark-component [arg-map v]
  (if *mark-component-fn*
	`(assoc ~arg-map :graph (~*mark-component* (:graph ~arg-map) ~v (:components ~arg-map)))
	arg-map))

(defn- increment-and-mark-component [arg-map v]
  (if *increment-component-fn*
	`(let [cnt# (~*increment-component* (:components ~arg-map))
		   ~arg-map (merge ~arg-map {:components cnt#})]
	   ~(mark-component arg-map v))
	arg-map))

(defn insert-recur-form
  ([]
	 `(recur ~*m* (rest ~*verts*)))
  ([hook-symbol]
	 `(recur (~hook-symbol ~*m* [~*wi* ~*v*]) (rest ~*verts*)))
  ([hook-symbol wrapper]
	 `(recur (~wrapper (~hook-symbol ~*m* [~*wi* ~*v*])
							  [~*wi* ~*v*])
			 (rest ~*verts*))))

(defn- make-cond-pair-helper [t? hook-symb hook-fn]
  (when hook-fn
	`((~t? (~*m* :graph) ~*wi* ~*v*) (recur (~hook-symb ~*m* [~*wi* ~*v*])
											(rest *verts*)))))

(defmulti make-cond-pair identity :default)

(defmethod make-cond-pair :tree-edge [kw]
  (if *tree-eg-hook-fn*
	`((~*tree-eg?* (~*m* :graph) ~*wi* ~*v*)
	  ~(insert-recur-form *tree-eg-hook* *dfs-internal*))
	`((~*tree-eg?* (~*m* :graph) ~*wi* ~*v*) ~(insert-recur-form *dfs-internal*))))

(defmethod make-cond-pair :cross-edge [kw]
  (if *cross-eg-hook-fn*
	`(:else ~(insert-recur-form *cross-eg-hook*))
	`(:else ~(insert-recur-form))))

(defmethod make-cond-pair :down-edge [kw]
  (make-cond-pair-helper *down-eg?* *down-eg-hook* *down-eg-hook-fn*))

(defmethod make-cond-pair :back-edge [kw]
  (make-cond-pair-helper *back-eg?* *back-eg-hook* *back-eg-hook-fn*))

(defn- increment-and-mark-post [arg-map]
  (if *mark-post-fn*
	`(let [post-c# (~*increment-post* (~arg-map :post))
		   mg# (~*mark-post* (~arg-map :graph) ~*wi* post-c#)]
	   (merge ~arg-map {:post post-c# :graph mg#}))
	arg-map))

(defn make-dfs-internal []
  (let [arg-map (gensym "arg-map__")]
	  `(fn ~*dfs-internal* [~arg-map [~*vi* ~*wi*]]
		 (let [pre-c# (~*increment-pre* (~arg-map :pre))
			   graph# (~*mark-pre* (~arg-map :graph) ~*wi* pre-c#)
			   ~arg-map (assoc ~arg-map :graph graph#)
			   ~arg-map ~(mark-component arg-map *wi*)]
		   (loop [~*m* (assoc ~arg-map :pre pre-c#)
				  ~*verts* (adjacent-to graph# ~*wi*)]
			 (if-let [~*v* (first ~*verts*)]
				 (cond ~@(doall (mapcat make-cond-pair [:tree-edge :back-edge :down-edge :cross-edge])))
			   ~(increment-and-mark-post *m*)))))))

(defn- insert-fn-definitions []
  (mapcat make-let-pair (seq [[*tree-eg?* *tree-eg?-fn*]
							  [*back-eg?* *back-eg?-fn*]
							  [*down-eg?* *down-eg?-fn*]
							  ;; hooks
							  [*tree-eg-hook* *tree-eg-hook-fn*]
							  [*back-eg-hook* *back-eg-hook-fn*]
							  [*down-eg-hook* *down-eg-hook-fn*]
							  [*cross-eg-hook* *cross-eg-hook-fn*]
							  ;; marking visited vertices
							  [*mark-pre* *mark-pre-fn*]
							  [*mark-post* *mark-post-fn*]
							  [*increment-pre* *increment-pre-fn*]
							  [*increment-post* *increment-post-fn*]
							  [*increment-component* *increment-component-fn*]
							  [*mark-component* *mark-component-fn*]])))

(defn- make-dfs-main [dfs-internal]
  (let [m (gensym "m__") g (gensym "g__") vs (gensym "vs__") v (gensym "v(main)__")]
	`(let [~@(insert-fn-definitions)
		   dfs-internal# ~dfs-internal]
	   (fn [~g vi#]
		 (let [verts# (cons vi# (remove (fn [i#] (= vi# i#))
										(get-valid-indices ~g)))]
		   (loop [~vs verts#
				  {graph# :graph
				   pre-c# :pre
				   post-c# :post :as ~m} ~(component-key-val-pair {:graph g :pre 0 :post 0})]
			 (if-let [~v (first ~vs)]
			   (cond
				(~*tree-eg?* (~m :graph) (first ~vs) (first ~vs)) 
				(recur (rest ~vs)
					   (dfs-internal# ~(increment-and-mark-component m v) 
									  [(first ~vs) (first ~vs)]))
				:else (recur (rest ~vs) ~m))
			   (with-meta (~m :graph) {:components (:components ~m)}))))))))

(defmacro make-dfs
  "Creates a custom Depth First Search function with provided hooks
  and predicates. Some of those hooks are required.

  Required hooks:
  :tree-edge? (graph u-index v-index -> boolean)
  :increment-pre (counter -> counter)
  :mark-pre-visited (graph vert-index data -> graph)
  
  Other possible hooks:
  :tree-edge-hook
  :cross-edge-hook
  :down-edge-hook
  :mark-post-visited (graph vert-index data -> graph)
  :back-edge?
  :down-edge?
  :increment-post (counter -> counter)
  :increment-component (counter -> counter)
  :mark-component (graph vert-index counter -> graph)

  (notice, that there's no :cross-edge? hook)

  Hooks ending with '-edge?' alaways should obey the conctract:
  
     graph u-index v-index --> boolean

  Hooks ending with '-hook' should satisfy following contract:
     
     arg-map [u v] --> arg-map

  where arg-map is a struct with keys :graph, :pre, :post.  

  The default counters are integers stored in the arg-map. The
  increment pre/post is performed before marking the
  vertices (therefore the default counters start with 1).
  
  Example:

  (make-dfs
   (:mark-pre-visited #(tag-vertex %1 %2 :pre %3))
   (:tree-edge? #(not (discovered? %1 %3)))
   (:increment-pre #(inc %1)))
  "
  [& bodies]
  (let [hooks-map (-> bodies make-fn-map verify-fn-map)]
	(binding [*tree-eg?* (gensym "tree-eg?__")
			  *back-eg?* (gensym "back-eg?__")
			  *down-eg?* (gensym "down-eg?__")
			  *tree-eg?-fn* (:tree-edge? hooks-map)
			  *back-eg?-fn* (:back-edge? hooks-map)
			  *down-eg?-fn* (:down-edge? hooks-map)
			  *tree-eg-hook* (gensym "tree-eg-hook__")
			  *back-eg-hook* (gensym "back-eg-hook__")
			  *down-eg-hook* (gensym "down-eg-hook__")
			  *cross-eg-hook* (gensym "cross-eg-hook__")
			  *tree-eg-hook-fn* (:tree-edge-hook hooks-map)
			  *back-eg-hook-fn* (:back-edge-hook hooks-map)
			  *down-eg-hook-fn* (:down-edge-hook hooks-map)
			  *cross-eg-hook-fn* (:cross-edge-hook hooks-map)
			  ;; marking/incrementing counters
			  *mark-pre* (gensym "mark-per__")
			  *mark-pre-fn* (:mark-pre-visited hooks-map)
			  *mark-post* (gensym "mark-post__")
			  *mark-post-fn* (:mark-post-visited hooks-map)
			  *increment-pre* (gensym "increment-pre__")
			  *increment-pre-fn* (:increment-pre hooks-map)
			  *increment-post* (gensym "increment-post__")
			  *increment-post-fn* (:increment-post hooks-map)
			  *increment-component* (gensym "increment-comp__")
			  *increment-component-fn* (:increment-component hooks-map)
			  *mark-component* (gensym "mark-component__")
			  *mark-component-fn* (:mark-component hooks-map)
			  ;; other stuff
			  *m* (gensym "m__")
			  *vi* (gensym "vi__")
			  *wi* (gensym "wi__")
			  *v* (gensym "vvvv__")
			  *verts* (gensym "verts__")
			  *dfs-internal* (gensym "dfs-internal__")]
	  (make-dfs-main (make-dfs-internal)))))

;; end custom dfs

(defn acyclic?
  [g]
  false)

;; utility functions

(defn- adj-list-of->str
  [vert]
  (if-let [adj (adjacent-to vert)]
	(map #(format "%2d" %) adj)))

(defn print-graph
  [gr]
  (let []
	(doseq [i (range 1 (count gr))]
	  (when (gr i)
		(print (format "%3d: " i))
		(doseq [adj (adj-list-of->str (gr i))]
		  (print adj))
		(println)))))

(defn to-mathematica-format [pairs]
  (let [format-pair (fn [[x y]] (format "{%d, %d}" x y))
		str-pairs (butlast (interleave (map format-pair pairs)
									   (repeat ", ")))
		sb (StringBuilder.)]
	(.append sb "{")
	(doseq [s str-pairs] (.append sb s))
	(.append sb "}")
	(.toString sb)))

;; TODO:
;; transitive closure: matrix form, adj. list form