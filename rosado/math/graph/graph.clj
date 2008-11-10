;; roland sadowski [szabla gmail com]

(ns rosado.math.graph
  (:use clojure.set))

;; :type can be : #{:directed :undirected}
(defstruct graph-info :type)
(defstruct vertex :meta :out)
(defstruct edge-info :dest :weight)

(defmacro get-type [g] `((first ~g) :type))

(defn directed? [g]
  (= (get-type g) :directed))

(defn make-vertex
  [m out]
  (struct vertex m out))

(defn make-graph
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
	 (if-let vert (g v)
	   (vert :out))))

(defn degree
  [vert]
  (when vert
	(count (adjacent-to vert))))

(defn tag?
  ([g v key]
	 (if-let vert (g v)
	   (tag? vert key)))
  ([v key]
	 (if-let m (v :meta)
	   (m key))))

(defn tag-vertex
  ([g v key val]
	 (tag-vertex g v {key val}))
  ([g v mta]
	 (if-let vert (g v)
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

(defn euler-path
  "Finds Euler path in a graph. Assumes the path exists."
  [g vi]
  (let [epath (fn [gr v st]
				(loop [stack st vert v graph gr]
				  (let [adj (adjacent-to graph vert) w (first adj)]
					(if adj
					  (recur (cons w stack)
							 w
							 (-> graph
								 (delete-edge [vert w])
								 (delete-edge [w vert])))
					  [graph stack vert]))))]
	(loop [pvert vi
		   [graph stack vert] (epath g vi '())
		   path []]
	  (cond (and (= pvert vert) (not (empty? stack)))
				(recur (first stack)
					   (epath graph (first stack) (rest stack))
					   (conj path (first stack)))
			:else path))))

(defn discovered? 
  ([g vi]
	 (tag? g vi :pre))
  ([vert]
	 (tag? vert :pre)))

(defstruct dfs-args :graph :pre :post)

(defmulti dfsearch (fn [args [a b]] (get-type  (args :graph))))

(defmethod dfsearch :directed [args [vi wi]]
  (let [pre-c (inc (args :pre))
		graph (tag-vertex (args :graph) wi :pre pre-c)]
	(loop [m (-> args (assoc :graph graph) (assoc :pre pre-c))
		   verts (adjacent-to graph wi)]
	  (if-let v (first verts)
		(cond (not (discovered? (m :graph) v)) 
				  (recur (dfsearch m [wi v]) (rest verts))
			  :else (recur m (rest verts)))
		{:graph (tag-vertex (m :graph) wi :post (inc (m :post)))
		 :pre (m :pre)
		 :post (inc (m :post))}))))

(defn depth-first-search
  [g vi]
  (let [verts (cons vi (filter #(not= vi %)
							   (get-valid-indices g)))]
	(loop [verts verts
		   {graph :graph
			pre-c :pre
			post-c :post :as m} {:graph g :pre 0 :post 0}]
	  (if verts 
		(cond
		 (not (discovered? graph (first verts))) 
		 	(recur (rest verts)
				   (dfsearch m 
							 [(first verts) (first verts)]))
		 :else (recur (rest verts) m))
		graph))))

;; customizable depth first search

(defn- make-fn-map [bds]
  (reduce #(assoc %1 (first %2) (second %2)) {} bds))

(defn- verify-fn-map [fn-map]
  (let [required #{:tree-edge? :increment-pre :mark-pre-visited}]
   (if (= (intersection (set (keys fn-map)) required) required)
	 fn-map
	 (throw (Exception. "Required functions not present.")))))

(defn- insert-hook [hooks-map hook-kw arg-map v u verts]
  (if (hooks-map hook-kw)
	(let [hook-fn_ (hooks-map hook-kw)]
	  `(recur (~hook-fn_ ~arg-map [~v ~u]) (rest ~verts)))
	`(recur ~arg-map (rest ~verts))))

(defn- make-condition [hooks-map test-kw hook-kw arg-map v u verts]
  (when (and (hooks-map hook-kw) (hooks-map test-kw))
	(let [test-fn_ (hooks-map test-kw)
		  hook-fn_ (hooks-map hook-kw)]
	  `((~test-fn_ (~arg-map :graph) ~v ~u) (recur (~hook-fn_ ~arg-map [~v ~u])
												   (rest ~verts))))))

(defn- increment-and-mark-post [inc-post-fn mark-post-fn arg-map current-v]
  (if inc-post-fn
	(let [post-c (gensym "post-c_") mg (gensym "mg_")]
		`(let [~post-c (~inc-post-fn (~arg-map :post))
			   ~mg (~mark-post-fn (~arg-map :graph) ~current-v ~post-c)]
		   (merge ~arg-map {:post ~post-c :graph ~mg})))
	arg-map))

(defn- make-hook-call [required optional]
  (if optional
	`(fn [amap# [v# u#]]
	   (~required (~optional amap# [v# u#])
				  [v# u#]))
	required))

(defn make-internal-dfs [hooks-map]
  (let [h-map (gensym "hooks-map_")
		dfs-internal (gensym "dfs-internal_")
		dfs-self (gensym "dfs-self_")
		tree-edge-action (gensym "tree-edge-action_")
		arg-map (gensym "arg-map_")
		m (gensym "m_")
		post-c (gensym "post-c_")
		increment-post (gensym "increment-post_")
		mark-post (gensym "mark-post_")
		verts (gensym "verts_")
		[vi wi v] [(gensym "vi_") (gensym "wi_") (gensym "v_")]]
   `(let [~h-map ~hooks-map
		  mark-pre# (~h-map :mark-pre-visited)
		  increment-pre# (~h-map :increment-pre)
		  ~increment-post (~h-map :increment-post)
		  ~mark-post (~h-map :mark-post-visited)
		  ~tree-edge-action (~h-map :tree-edge-action)]
	  (fn ~dfs-internal [~arg-map [~vi ~wi]]
		(let [pre-c# (increment-pre# (~arg-map :pre))
			  graph# (mark-pre# (~arg-map :graph) ~wi pre-c#)
			  ~dfs-self ~(make-hook-call dfs-internal tree-edge-action)]
		  (loop [~m (-> ~arg-map (assoc :graph graph#) (assoc :pre pre-c#))
				 ~verts (adjacent-to graph# ~wi)]
			(if-let ~v (first ~verts)
			  (cond
			   ~@(make-condition (assoc hooks-map :self dfs-self)
								 :tree-edge?
								 :self
								 m
								 wi v
								 verts)
			   ~@(make-condition hooks-map
								 :back-edge?
								 :back-edge-action
								 m
								 wi v
								 verts)
			   ~@(make-condition hooks-map
								 :down-edge?
								 :down-edge-action
								 m
								 wi v
								 verts)
			   :else ~(insert-hook hooks-map
								   :cross-edge-action
								   m
								   wi v
								   verts))
			  ~(increment-and-mark-post increment-post mark-post m wi))))))))

(defn- make-dfs-main-fn [pre-visited?-fn dfs-internal]
  `(let [dfs-internal# ~dfs-internal
		 not-pre-visited?# ~pre-visited?-fn]
	 (fn [g# vi#]
	   (let [verts# (cons vi# (remove (fn [i#] (= vi# i#))
									  (get-valid-indices g#)))]
		 (loop [vs# verts#
				{graph# :graph
				 pre-c# :pre
				 post-c# :post :as m#} {:graph g# :pre 0 :post 0}]
		   (if vs# 
			 (cond
			  (not-pre-visited?# (m# :graph) (first vs#) (first vs#)) 
			  (recur (rest vs#)
					 (dfs-internal# m# 
									[(first vs#) (first vs#)]))
			  :else (recur (rest vs#) m#))
			 (m# :graph)))))))

(defmacro make-dfs
  "Creates a custom Depth First Search function with provided hooks
  and predicates. Some of those hooks are required.

  Required hooks:
  :tree-edge? (graph u-index v-index -> boolean)
  :increment-pre (counter -> counter)
  :mark-pre-visited (graph vert-index data -> graph)
  
  Other possible hooks:
  :tree-edge-action
  :cross-edge-action
  :down-edge-action
  :mark-post-visited (graph vert-index data -> graph)
  :back-edge?
  :down-edge?
  :increment-post (counter -> counter)

  (notice, that there's no :cross-edge? hook)

  Hooks ending with '-edge?' alaways should obey the conctract:
  
     graph u-index v-index --> boolean

  Hooks ending with '-action' should satisfy following contract:
     
     arg-map [u v] --> arg-map

  where arg-map is a struct with keys :graph, :pre, :post.  
  "

  [& bodies]
  (let [hooks-map (-> bodies make-fn-map verify-fn-map)
		dfs-internal-fn (make-internal-dfs hooks-map)
		pre-visited? (hooks-map :tree-edge?)]
	(make-dfs-main-fn pre-visited? dfs-internal-fn)))

;; end custom dfs

(defn acyclic?
  [g]
  false)

(defn topological-sort
  [g]
  nil)

;; utility functions

(defn- adj-list-of->str
  [vert]
  (if-let adj (adjacent-to vert)
	(map #(format "%2d" %) adj)))

(defn print-graph
  [gr]
  (let []
	(doseq i (range 1 (count gr))
	  (when (gr i)
		(print (format "%3d: " i))
		(doseq adj (adj-list-of->str (gr i))
		  (print adj))
		(println)))))

(defn to-mathematica-format [pairs]
  (let [format-pair (fn [[x y]] (format "{%d, %d}" x y))
		str-pairs (butlast (interleave (map format-pair pairs)
									   (repeat ", ")))
		sb (StringBuilder.)]
	(.append sb "{")
	(doseq s str-pairs (.append sb s))
	(.append sb "}")
	(.toString sb)))

;; TODO:
;; transitive closure: matrix form, adj. list form