;; testing dfs search macros & functions

(ns graph-dfs-tests
  (:use clojure.contrib.test-is))

(require '(rosado.math [graph :as g]))


(def g3b [[1 2] [2 3] [3 1] [2 1] [3 2] [1 3]])
(def g3 (g/pairs->graph g3b))

;; dfs with tree action
(def my-dfs
	 (g/make-dfs
	  (:mark-pre-visited #(g/tag-vertex %1 %2 :pre %3))
	  (:tree-edge? #(not (g/discovered? %1 %3)))
	  (:tree-edge-hook (fn [amap [a b]]  amap))
	  (:increment-pre #(inc %1))
	  (:mark-post-visited #(g/tag-vertex %1 %2 :post %3))
	  ;; extra
	  (:back-edge? #(not (g/tag? %1 %3 :post)))
	  (:cross-edge-hook (fn [amap [a b]] amap))
	  (:increment-post (fn [cnt] (inc cnt)))
	  (:mark-post-visited (fn [g v data]
							(g/tag-vertex g v :post data)))))

;; graph search started from vertex 1
(def desired-result
	 [{:type :directed}
	  {:meta {:post 3, :pre 1}, :out '(3 2)}
	  {:meta {:post 1, :pre 3}, :out '(1 3)}
	  {:meta {:post 2, :pre 2}, :out '(2 1)}])

(deftest custom-dfs
  (is (= desired-result (my-dfs g3 1))))

;; dfs w/o tree edge hook
(def my-dfs-no-tree-hook
	 (g/make-dfs
	  (:mark-pre-visited #(g/tag-vertex %1 %2 :pre %3))
	  (:tree-edge? #(not (g/discovered? %1 %3)))
	  (:increment-pre #(inc %1))))

(deftest dfs-without-tree-edge-hook
  (is (not= nil (my-dfs-no-tree-hook g3 1))))

;; counting components


(def two-comp-g (g/pairs->graph [[1 2] [3 4]]))
(def one-comp-g1 (g/pairs->graph [[1 2] [2 3]]))
(def one-comp-g2 (g/pairs->graph [[1 2]]))

(def my-dfs-component-count
	 (g/make-dfs
	  (:mark-pre-visited #(g/tag-vertex %1 %2 :pre %3))
	  (:tree-edge? #(not (g/discovered? %1 %3)))
	  (:increment-pre #(inc %1))
	  ;; component count related
	  (:increment-component (fn [cnt]
							  (inc cnt)))
	  (:mark-component (fn [g v cn]
						 ;(println "mark-component: " [g v cn])
						 (g/tag-vertex g v :compo cn)))))

(def result1 (my-dfs-component-count one-comp-g2 1))

(deftest counting-components
  (is (= (meta (my-dfs-component-count two-comp-g 1)) {:components 2}))
  (is (= (meta (my-dfs-component-count one-comp-g1 1)) {:components 1}))
  (is (= [(g/tag? result1 1 :compo) (g/tag? result1 2 :compo)] [1 1])))

(run-tests)