(ns missing.topology-test
  (:require [missing.topology :refer :all]
            [clojure.test :refer :all]
            [clojure.set :as sets])
  (:refer-clojure :exclude (empty complement)))

(deftest normalization-test
  (let [g {:a [:b :c] :b [:d]}]
    (is (= {:a #{:c :b}
            :b #{:d}
            :c #{}
            :d #{}}
           (normalize g)))))

(deftest consumers-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:b :c :d} (consumers g))))

(deftest producers-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:a :b} (producers g))))

(deftest sources-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:a} (sources g))))

(deftest sinks-test
  (let [g {:a [:b :c] :b [:d]}]
    (is #{:c :d} (sinks g))))

(deftest topological-sort-test
  (let [g {:a [:b :c] :b [:d]}]
    (is (= [:a :c :b :d] (topological-sort g)))))

(deftest topological-sort-with-grouping-test
  (let [g {:a [:b :c] :b [:d]}]
    (is (= [#{:a} #{:c :b} #{:d}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :c] :b [:d] :c [:d]}]
    (is (= [#{:a} #{:c :b} #{:d}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :m] :b [:c] :c [:d] :d [:e] :m [:e]}]
    (is (= [#{:a} #{:m :b} #{:c} #{:d} #{:e}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :c]
           :b [:d :e]
           :c [:e :d]
           :e [:f]
           :d [:f]}]
    (is (= [#{:a} #{:c :b} #{:e :d} #{:f}]
           (topological-sort-with-grouping g))))

  (let [g {:a [:b :c :l]
           :b [:d :e :m]
           :c [:e :d :m]
           :d [:f]
           :e [:f]
           :l [:d :e]}]
    (is (= [#{:a} #{:l :c :b} #{:m :e :d} #{:f}]
           (topological-sort-with-grouping g)))))

(deftest topological-sorts-return-nil-if-cyclical
  (let [g {:a [:b :c] :b [:a]}]
    (is (nil? (topological-sort g)))
    (is (nil? (topological-sort-with-grouping g)))))

(deftest inverse-preserves-nodes-with-no-edges
  (let [g {:a [:b :c] :d #{}}]
    (is (= {:c #{:a}, :b #{:a}, :d #{}, :a #{}} (inverse g)))))

(deftest shortest-paths-test
  (let [g {:a [:b :c] :c [:d] :d [:e] :b [:e]}]
    (is (= {[:b :e] {:distance 1, :path [:b :e]},
            [:c :d] {:distance 1, :path [:c :d]},
            [:c :e] {:distance 2, :path [:c :d :e]},
            [:e :e] {:distance 0, :path [:e]},
            [:a :d] {:distance 2, :path [:a :c :d]},
            [:d :e] {:distance 1, :path [:d :e]},
            [:a :a] {:distance 0, :path [:a]},
            [:a :b] {:distance 1, :path [:a :b]},
            [:d :d] {:distance 0, :path [:d]},
            [:b :b] {:distance 0, :path [:b]},
            [:a :c] {:distance 1, :path [:a :c]},
            [:a :e] {:distance 2, :path [:a :b :e]},
            [:c :c] {:distance 0, :path [:c]}}
           (shortest-paths g)))))

(deftest cycles-and-shortest-paths
  (let [g     {"application/pdf" #{"image/*"}
               "image/*"         #{"image/png" "image/gif" "image/jpeg"}
               "image/png"       #{"image/*"}
               "image/gif"       #{"image/*"}
               "image/jpeg"      #{"image/*"}
               "image/tiff"      #{"application/pdf"}}
        paths (shortest-paths g)]
    (are [source target] (contains? paths [source target])
      "image/tiff" "image/png"
      "image/tiff" "image/gif"
      "image/tiff" "image/jpeg"
      "image/tiff" "application/pdf"
      "image/png" "image/png"
      "image/png" "image/jpeg"
      "image/png" "image/gif"
      "image/png" "image/*"
      "image/gif" "image/gif"
      "image/gif" "image/jpeg"
      "image/gif" "image/png"
      "image/jpeg" "image/jpeg"
      "image/jpeg" "image/png"
      "image/jpeg" "image/gif"
      "image/jpeg" "image/*"
      "image/png" "image/*"
      "image/gif" "image/*")))

(deftest intersect?-test
  (is (intersect? {:a [:b]} {:b [:c]}))
  (is (not (intersect? {:a [:c]} {:b [:d]}))))

(deftest exclusive?-test
  (is (exclusive? {:a [:c]} {:b [:d]}))
  (is (not (exclusive? {:a [:b]} {:b [:c]}))))

(deftest supergraph?-test
  (is (supergraph? {:a [:b :c]} {:a [:b]}))
  (is (not (supergraph? {:a [:b]} {:a [:b :c]})))
  (is (supergraph? {:a [:b :c]} {:a [:b :c]})))

(deftest subgraph?-test
  (is (subgraph? {:a [:b]} {:a [:b :c]}))
  (is (not (subgraph? {:a [:b :c]} {:a [:b]})))
  (is (subgraph? {:a [:b]} {:a [:b]})))

(deftest complete?-test
  (is (complete? {:a [:b] :b [:a]}))
  (is (not (complete? {:a [:b] :c [:d]}))))

(deftest tree?-test
  (is (tree? {:a [:b]}))
  (is (not (tree? {:a [:b] :c [:d]}))))

(deftest select-test
  (is (= {:a #{:c :b}, :c #{:d}, :b #{}, :d #{}}
         (select {:a [:b :c] :c [:d]} :a)))
  (is (= {:c #{:d}, :d #{}}
         (select {:a [:b :c] :c [:d]} :c))))

(deftest connected?-test
  (is (connected? {:a [:b] :b [:a]}))
  (is (not (connected? {:a [:b] :b [:c]}))))

(deftest cyclical-test
  (is (cyclical? {:a [:b] :b [:a]}))
  (is (not (cyclical? {:a [:b]}))))

(deftest branches-test
  (is (= #{:a :c} (branches {:a [:b] :c [:d]}))))

(deftest degree-test
  (is (= 2 (degree {:a [:b :c]} :a))))

(deftest neighbors-test
  (is (= #{:e :a} (neighbors {:a [:b] :c [:d] :d [:e] :e [:b]} :b))))

(deftest bidirectional-test
  (is (= {:a #{:b}, :d #{:c}, :b #{:a}, :c #{:d}}
         (bidirectional {:a [:b] :c [:d]}))))

(deftest intersection-test
  (is (= {:b #{}} (intersection {:a [:b]} {:b [:c :d]}))))

(deftest empty-test
  (is (= {:a #{} :b #{}} (empty {:a [:b]}))))

(deftest expandg-test
  (is (= {1 #{3 2}, 2 #{4 5}, 3 #{4 5}, 4 #{}, 5 #{}}
         (expandg identity {[1] #{[2 3]} [2 3] #{[4 5]}}))))

(deftest filterg-test
  (is (= {2 #{4}, 4 #{}} (filterg even? {1 [2 3] 2 [4]}))))

(deftest removeg-test
  (is (= {1 #{3}, 3 #{}} (removeg even? {1 [2 3] 2 [4]}))))

(deftest mapg-test
  (is (= {2 #{4 3}, 5 #{7 6}, 7 #{}, 4 #{}, 6 #{}, 3 #{}}
         (mapg inc {1 #{2 3} 4 #{5 6}}))))

(deftest contractg-test
  (is (= {3 #{}, 5 #{3}} (contractg even? {2 #{3} 5 #{2}}))))

(deftest sink-test
  (is (not (sink? {:a [:b]} :a)))
  (is (sink? {:a [:b]} :b)))

(deftest source-test
  (is (not (source? {:a [:b]} :b)))
  (is (source? {:a [:b]} :a)))

(deftest difference-test
  (is (= {:a #{}} (difference {:a [:b]} {:b [:c]}))))

(deftest symmetric-difference-test
  (is (= {:a #{} :c #{}} (symmetric-difference {:a [:b]} {:b [:c]}))))

(deftest exterior-test
  (is (= #{:a :c} (exterior {:a [:b] :b [:c]}))))

(deftest interior-test
  (is (= #{:b} (interior {:a [:b] :b [:c]}))))

(deftest transitive-intersection-test
  (is (= {:a #{:b :c :d} :b #{} :c #{} :d #{}}
         (transitive-intersection
           {:a [:b :c] :e [:f]} {:a [:d]}))))

(deftest transitive-union-test
  (is (= {:e #{:f}, :g #{}, :c #{}, :b #{}, :f #{}, :a #{:g :c :b}}
         (transitive-union
           {:a [:b :c] :e [:f]} {:a [:g] :k [:h]}))))

(deftest transitive-difference-test
  (let [keep {:add [:+] :subtract [:-]}
        drop {:+                          [:number]
              :-                          [:number]
              :number                     [:referenced-by-referenced]
              :referenced-by-referenced   []
              :referenced-by-unreferenced []
              :unreferenced               [:referenced-by-unreferenced]}]
    (is (= {:unreferenced               #{:referenced-by-unreferenced},
            :referenced-by-unreferenced #{}}
           (transitive-difference drop keep)))))

(deftest bridges-test
  (let [g {:a #{:c :b},
           :b #{:e :c :f :a},
           :c #{:g :h :b :d :a},
           :d #{:e :c :f},
           :e #{:b :d},
           :f #{:g :b :d},
           :g #{:c :f},
           :h #{:c :j :i},
           :i #{:k :j :h},
           :j #{:l :h :i},
           :k #{:m :l :i},
           :l #{:k :j},
           :m #{:o :n :k},
           :n #{:o :m},
           :o #{:n :m}}]
    (is (= #{[:k :m] [:c :h] [:h :c] [:m :k]} (bridges g)))
    (doseq [bridge (bridges g)]
      (is (bridge? g bridge)))
    (doseq [edge (sets/difference (edges g) (bridges g))]
      (is (not (bridge? g edge))))))

(deftest self-looped-test
  (is (= {:c #{:c :d}, :b #{:b}, :d #{:d}, :a #{:b :a}}
         (self-looped {:a [:b] :c [:d]}))))

(deftest self-loops-test
  (is (= #{:c :b :d} (self-loops {:c #{:c :d}, :b #{:b}, :d #{:d}, :a #{:b}})))
  (is (= (nodes {:a [:b] :c [:d]}) (self-loops (self-looped {:a [:b] :c [:d]})))))