;; rosado.math.graph tests

(ns graph-tests
  (:use clojure.contrib.test-is))

(require '(rosado.math [graph :as g]))

(def a-graph (g/make-graph 3))
(def graph-w-2v (g/add-vertex (g/add-vertex (g/make-graph 2) 1) 2))

(deftest make-graph
  (is (not= nil (g/make-graph 3)))
  (is (= [{:type :directed} nil nil nil] (g/make-graph 3)))
  (is (not= nil (first (g/make-graph 3)))))

(deftest add-vertex
  (is (= ((g/add-vertex a-graph 1) 1) (g/make-vertex nil nil)))
  (is (nil? (-> a-graph (g/add-vertex 1) (g/add-vertex 1)))))


(deftest add-edge
  (is (= (g/add-edge graph-w-2v [1 2])
		 [{:type :directed} (g/make-vertex nil '(2)) (g/make-vertex nil nil)])))

(deftest adjacent-to
  (is (= (g/adjacent-to (g/add-edge graph-w-2v [1 2]) 1)
		 '(2))))

;; altering adj. list
(def g (-> (g/make-graph 3)
		   (g/add-vertex 1 (g/make-vertex nil nil))
		   (g/add-vertex 2 (g/make-vertex nil nil))
		   (g/add-vertex 3 (g/make-vertex nil nil))))
(def g-new (g/add-edge g [1 3]))

(deftest alter-adj-list-of
  (is (= (g/adjacent-to g 1)
		 (g/adjacent-to (g/alter-adj-list-of g-new 1 nil) 1))))

;; tagging a vertex
(deftest tag-vertex
  (is (= (g/tag-vertex g 1 {:label "test"})
		 (g/alter-vertex g 1 (g/make-vertex {:label "test"} nil))))
  (is (= (g/tag? (g/tag-vertex g 1 :x "tagged") 1 :x) "tagged")))

;; delete vertex
(deftest delete-vertex
  (is (= (g/delete-vertex graph-w-2v 1) [{:type :directed} nil (g/make-vertex nil nil)])))

;; ordered pairs to graph
(def g2 [[1 2] [2 3]])

(deftest pairs->graph
  (is (= (g/pairs->graph g2 )
		 [{:type :directed} {:meta nil, :out '(2)} {:meta nil, :out '(3)} {:meta nil, :out nil}])))

;; eulerian?

(def not-e (g/pairs->graph [[1 2] [2 3] [3 4]]))

(deftest eulerian?
  (is (= false (g/eulerian? not-e 1 1))))

;; SCRATCH 

(def my-dfs-extra
	 (g/make-dfs
	  (:mark-pre-visited #(g/tag-vertex %1 %2 :pre %3))
	  (:tree-edge? #(not (g/discovered? %1 %3)))
	  (:tree-edge-action (fn [amap [a b]]
						   (println "it's a tree edge!" [a b])
						   amap))
	  (:increment-pre #(inc %1))
	  (:mark-post-visited #(g/tag-vertex %1 %2 :post %3))
	  ;; extra
	  (:back-edge? #(not (g/tag? %1 %3 :post)))
	  (:back-edge-action (fn [amap [a b]]
						   (println "back edge!" [a b])
						   amap))
	  (:down-edge? (fn [g v u]
					(> (g/tag? g u :pre) (g/tag? g v :pre))))
	  (:down-edge-action (fn [amap [a b]]
						   (println "down edge!" [a b])
						   amap))
	  (:cross-edge-action (fn [amap [a b]]
							(println "cross edge!" [a b]) 
							amap))
	  (:increment-post #(inc %1))
	  (:mark-post-visited #(g/tag-vertex %1 %2 :post %3))
	  (:back-edge-terminate? (fn [verts]
							   (println "OMG! back edge, terminate")
							   false))
	  (:cross-edge-terminate? (fn [verts]
								(println "I WILL NOT TERMINATE on cross edge")
								false))))

;;(println (my-dfs-extra g3 1))

;; (def g1 [[1 1]])
;; (def g2 [[1 2] [2 3]])
;; (def gx2 [[:a :b] [:b :c]])
;; (def g3 [[1 2] [2 3] [2 1]])

(run-tests)
