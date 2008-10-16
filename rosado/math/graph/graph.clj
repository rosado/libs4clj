;; roland sadowski [szabla gmail com]

(ns rosado.math.graph)

;; :type can be : #{:directed :undirected}
(defstruct graph-info :type)
(defstruct vertex :meta :out)
(defstruct edge-info :dest :weight)

(derive clojure.lang.IPersistentMap ::compound)
(derive java.lang.Number ::simple)

(defmulti V class)
(defmulti E class)

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


(defn- seq-shift<<
  "(seq-shift<< '(1 2 3) 2) ==> (3 1)"
  [s n]
  (take (dec (count s))(drop n (cycle s))))

(defn- get-valid-indices
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

(defn acyclic?
  [g]
  false)

(defn topological-sort
  [g]
  nil)

(defn discovered? 
  ([g vi]
	 (tag? g vi :pre))
  ([vert]
	 (tag? vert :pre)))

(defmulti dfsearch (fn [g [a b] pre post] (get-type  g)))

(defmethod dfsearch :directed [graph [vi wi] pre-c post-c]
  (let [pre-c (inc pre-c)
		graph (tag-vertex graph wi :pre pre-c)]
	(loop [{g :graph pre-c :pre post-c :post :as m} {:graph graph :pre pre-c :post post-c}
		   verts (adjacent-to g wi)]
	  (if-let v (first verts)
		(cond (not (discovered? g v)) 
			  	(recur (dfsearch g [wi v] pre-c post-c) (rest verts))
			  :else (recur m (rest verts)))
		{:graph (tag-vertex g wi :post (inc post-c)) :pre pre-c :post (inc post-c)}))))

(defn depth-first-search
  [g vi]
  (let [verts (cons vi (filter #(not= vi %)
							   (get-valid-indices g)))]
	(loop [verts verts
		   {graph :graph pre-c :pre post-c :post :as m} {:graph g :pre 0 :post 0}]
	  (if verts 
		(cond
		 (not (discovered? graph
						   (first verts))) (recur (rest verts)
												  (dfsearch graph 
															[(first verts) (first verts)]
															pre-c
															post-c))
		 :else (recur (rest verts) m))
		graph))))


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

;; tests etc
(def g1 [[1 1]])
(def g2 [[1 2] [2 3]])
(def gx2 [[:a :b] [:b :c]])
(def g3 [[1 2] [2 3] [2 1]])
(def g4 [[1 2] [2 3] [3 1]])
(def g4b [[1 2] [2 3] [3 1] [2 1] [3 2] [1 3]])

(defn to-mathematica-format [pairs]
  (let [format-pair (fn [[x y]] (format "{%d, %d}" x y))
		str-pairs (butlast (interleave (map format-pair pairs)
									   (repeat ", ")))
		sb (StringBuilder.)]
	(.append sb "{")
	(doseq s str-pairs (.append sb s))
	(.append sb "}")
	(.toString sb)))

;; (to-mathematica-format g1)
;; (pairs->graph g3)
(def g (pairs->graph g4b))

;; multimethods
;; (defmulti tmulti (fn [x y] (= x :a)))
;; (defmethod tmulti true [x y] :OK)
;; (tmulti :a 1)



