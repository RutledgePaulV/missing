(ns missing.core-test
  (:require [clojure.test :refer :all]
            [missing.core :refer :all])
  (:import (java.time Duration)))



(deftest filter-keys-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (filter-keys any? nil)))
    (is (= {1 2 3 4} (filter-keys odd? m)))))

(deftest filter-vals-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (filter-vals any? nil)))
    (is (= {6 5 8 7} (filter-vals odd? m)))))

(deftest map-keys-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (map-keys inc nil)))
    (is (= {2 2 4 4 7 5 9 7} (map-keys inc m)))))

(deftest map-vals-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (map-vals inc nil)))
    (is (= {1 3 3 5 6 6 8 8} (map-vals inc m)))))

(deftest reverse-map-test
  (let [m {1 2 3 4 6 5 8 7}]
    (is (= {} (reverse-map nil)))
    (is (= {2 1 4 3 5 6 7 8} (reverse-map m)))))

(deftest index-by-test
  (let [coll [{:id 1} {:id 2} {:id 3}]]
    (is (= {1 {:id 1} 2 {:id 2} 3 {:id 3}} (index-by :id coll)))))

(deftest contains-all?-test
  (let [coll #{1 2 3 4 5}]
    (is (contains-all? coll []))
    (is (contains-all? coll [1 2]))
    (is (contains-all? coll [1 2 3 4 5]))
    (is (not (contains-all? coll [1 6])))))

(deftest intersect?-test
  (is (intersect? #{1 2} #{2} #{2 3}))
  (is (not (intersect? #{1} #{2} #{3})))
  (is (not (intersect? #{1 2} #{2} #{3}))))

(deftest keepcat-test
  (let [coll [[1 2 3 nil nil 4] [nil 5 6]]]
    (is (= [1 2 3 4 5 6] (into [] (keepcat identity) coll)))
    (is (= [1 2 3 4 5 6] (vec (keepcat identity coll))))))

(deftest lift-by-test
  (let [f (fn [x] (* x x))]
    (is (= 9 ((lift-by (partial * 3) f) 1)))))

(deftest shared-keys-test
  (let [m1 {:one 1 :three 3} m2 {:two 2 :three 3} m3 {}]
    (is (= #{} (shared-keys m1 m2 m3)))
    (is (= #{:three} (shared-keys m1 m2)))))

(deftest shared-entries-test
  (let [m1 {:one 1 :three 3} m2 {:two 2 :three 4} m3 {}]
    (is (= [{} {} {}] (shared-entries m1 m2 m3)))
    (is (= [{:three 3} {:three 4}] (shared-entries m1 m2)))))

(deftest deep-merge-test
  (let [m1 {1 {:stuff 3 :thing 2} 2 4} m2 {1 {:things 4 :stuff 5}}]
    (is (= {1 {:thing 2 :stuff 5 :things 4} 2 4} (deep-merge m1 m2)))))

(deftest key=-test
  (is (key= :stuff :stuff))
  (is (key= "stuff" :stuff))
  (is (not (key= :badger :stuff)))
  (is (not (key= "badger" :stuff))))

(deftest nor-test
  (let [state (atom [])]
    (is (not (nor true (swap! state conj 1))))
    (is (= [] @state)))
  (let [state (atom [])]
    (is (not (nor (identity false) (swap! state conj 1))))
    (is (= [1] @state)))
  (let [state (atom true) state2 (atom true)]
    (is (nor (identity false) (reset! state false) (reset! state2 false)))
    (is (false? @state))
    (is (false? @state2))))

(deftest sort-by-value-test
  (let [m {1 2 4 5 3 4 0 5}]
    (is (= [[4 5] [0 5] [3 4] [1 2]]
           (vec (seq (sort-by-value m)))))))

(deftest dissoc-in-test
  (let [m {1 {:stuff 3 :thing 2} 2 4}]
    (is (= {1 {:thing 2} 2 4} (dissoc-in m [1 :stuff])))))

(deftest distinct-by-test
  (let [vs [{:stuff 5 :thing 2} {:thing 3 :stuff 7} {:thing 4 :stuff 5}]]
    (is (= vs (distinct-by :thing vs)))
    (is (= vs (into [] (distinct-by :thing) vs)))
    (is (= [(get vs 0) (get vs 1)] (distinct-by :stuff vs)))
    (is (= [(get vs 0) (get vs 1)] (into [] (distinct-by :stuff) vs)))))

(deftest merge-sort-test
  (let [v1 (range 0 10 2) v2 (range 1 10 2)]
    (is (= (range 0 10) (merge-sort [v1 v2])))))

(deftest do-force-test
  (let [state (atom [])]
    (is (nil?
          (doforce
            (do (swap! state conj 1) (throw (ex-info "" {})))
            (do (swap! state conj 2) (throw (ex-info "" {})))
            (do (swap! state conj 3) (throw (ex-info "" {})))
            (do (swap! state conj 4) (throw (ex-info "" {}))))))
    (is (= [1 2 3 4] @state))))

(deftest with-timeout-test
  (let [start (System/currentTimeMillis)
        [success result] (with-timeout 1000 (Thread/sleep 5000))
        stop  (System/currentTimeMillis)]
    (is (not success))
    (is (nil? result))
    (is (> 2000 (- stop start)))))

(deftest run-par!-test
  (let [task-durations [500 600 800]
        [millis _] (timing (run-par! #(Thread/sleep %) task-durations))]
    (is (> (/ (reduce + 0 task-durations) 2) millis))))

(deftest together-test
  (let [[millis [one two three]]
        (timing (together
                  (do (Thread/sleep 500) 1)
                  (do (Thread/sleep 600) 2)
                  (do (Thread/sleep 800) 3)))]
    (is (> (/ (reduce + 0 [500 600 800]) 2) millis))
    (is (= one 1))
    (is (= two 2))
    (is (= three 3))))

(deftest get-extension-test
  (is (nil? (get-extension "stuff")))
  (is (= ".pdf" (get-extension "stuff.txt.pdf")))
  (is (= "." (get-extension "stuff.")))
  (is (= ".stuff" (get-extension ".stuff")))
  (is (= ".txt" (get-extension ".stuff.txt"))))

(deftest get-filename-test
  (is (= "stuff" (get-filename "stuff")))
  (is (= "stuff.txt" (get-filename "stuff.txt.pdf")))
  (is (= "stuff" (get-filename "stuff.")))
  (is (= ".stuff" (get-filename ".stuff"))))

(deftest not-blank?-test
  (is (not-blank? "testing"))
  (is (not (not-blank? "")))
  (is (not (not-blank? nil)))
  (is (not (not-blank? "   \n\t \r\n"))))

(deftest human-readable-test
  (is (= "2 days, 22 hours, 50 seconds, 233 milliseconds"
         (human-readable (Duration/ofMillis 252050233)))))

(deftest powerset-test
  (is (= #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}} (subsets #{1 2 3}))))

(deftest group-by-labels-test
  (let [maps    [{:labels {:one 1 :two 2 :three 3 :four 5}}
                 {:labels {:one 2 :two 2 :three 4 :four 5}}]
        grouped (group-by-labels :labels maps)]
    (is (nil? (get grouped {:bananas true})))
    (is (= [(first maps)] (get grouped {:one 1})))
    (is (= maps (get grouped {})))
    (is (= maps (get grouped {:two 2})))
    (is (= maps (get grouped {:two 2 :four 5})))
    (is (= [(second maps)] (get grouped {:three 4})))))