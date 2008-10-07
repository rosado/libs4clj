;; roland sadowski [szabla gmail com]

(ns rosado.math.graph)

(defstruct vertex :meta :out)

(defn make-vertex
  [m out]
  (struct vertex m out))

(defn make-graph
  [g]
  (let [g g]))

(defn delete-edge
  [g e]
  nil)

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
  [g k v]
  (if (g k)
	nil
	(alter-vertex g k v)))

(defn adjacent-to
  ([v]
	 (if v
	   (v :out)))
  ([g v]
	 (if-let vert (g v)
	   (vert :out))))

(defn tag-vertex
  ([g v mta]
	 (if-let vert (g v)
	   (alter-vertex g v (tag-vertex vert mta))
	   g))
  ([v mta]
	 (when v
	   (make-vertex (first (merge (v :meta) mta))
					(v :out)))))

(defn alter-adj-list-of
  "Returns the graph with the adjacency list of vertex v changed to list."
  [g v lst]
  (alter-vertex g v (make-vertex ((g v) :meta) lst)))

(defn delete-vertex
  "Returns the graph with vertex v removed."
  [g v]
  (let [new-g    (dissoc g v)
		vertices (map #(nth % 0) new-g)
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

(defn pairs->graph
  "Produces a graph from a list of ordered pairs [u v],
  where u, v -- vertices."
  [pairs]
  (let [empty-graph {}]
	(reduce add-edge (cons empty-graph pairs))))

(defn acyclic?
  [g]
  false)

(defn topological-sort
  [g]
  nil)

(defn depth-first-search
  [g v])

;; tests etc
(def g1 [[1 1]])
(def g2 [[1 2] [2 3]])
(def gx2 [[:a :b] [:b :c]])
(def g3 [[1 2] [2 3] [2 1]])

(defn to-mathematica-format [pairs]
  (let [format-pair (fn [[x y]] (format "{%d, %d}" x y))
		str-pairs (butlast (interleave (map format-pair pairs)
									   (repeat ", ")))
		sb (StringBuilder.)]
	(.append sb "{")
	(doseq s str-pairs (.append sb s))
	(.append sb "}")
	(.toString sb)))

(to-mathematica-format g1)
(pairs->graph g3)
(def g (pairs->graph g3))