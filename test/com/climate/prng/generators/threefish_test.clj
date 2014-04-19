(ns com.climate.prng.generators.threefish-test
  (:require
    [clojure.test :refer :all]
    [criterium.core :refer [quick-bench]]
    [com.climate.prng.core :as prng]
    [com.climate.prng.generators.threefish :as tf])
  (:import
    [com.climate.prng.generators.threefish ThreefishState]))

(deftest test-seed-state
  (testing "seed-state returns an object that satisfies PRNGState protocol"
    (are [seed] (satisfies? prng/PRNGState (tf/seed-state seed))
         (long-array [0x12345 0x23456 0x34567 0x45678])
         (long-array [-1 -1 -1 -1])
         (long-array [0 0 0 0 0 0 0 0])))
  (testing "seed-state is a pure function that creates a new object"
    (are [seed] (let [state-a (tf/seed-state seed)
                      state-b (tf/seed-state seed)]
                  (not (identical? state-a state-b))
                  (= state-a state-b)
                  (= (hash state-a) (hash state-b)))
         (long-array [0x12345 0x23456 0x34567 0x45678])
         (long-array [-1 -1 -1 -1])
         (long-array [0 0 0 0 0 0 0 0])))
  (testing "seed-state does not hold a reference to the seed argument"
    (let [seed-a (long-array [1 2 Long/MAX_VALUE -72])
          seed-b (into-array Long/TYPE (seq seed-a))
          state-a (tf/seed-state seed-a)
          state-b (tf/seed-state seed-b)]
      (is (= state-a state-b))
      ; mutating the provided seed does not change the state
      (aset-long seed-a 0 (inc (aget seed-a 0)))
      (is (= state-a state-b))
      (is (= (prng/->long state-a) (prng/->long state-b)))))
  (testing "seed-state throws an exception when seed argument is invalid"
    (are [seed] (thrown? IllegalArgumentException (tf/seed-state seed))
         nil
         (long-array 0) ; empty array is not allowed
         (long-array [0 1 2 3 4]) ; does not match available block size
         (int-array [0x12345 0x23456 0x34567 0x45678]))))

(deftest test-clone
  (testing "clone returns a new object that equals and hashes like the original"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          a (tf/seed-state seed)
          b (prng/clone a)
          c (prng/clone a)]
      (is (= a b c))
      (is (= (hash a) (hash b) (hash c)))
      (is (not (identical? a b)))
      (is (not (identical? a c)))
      (is (not (identical? b c)))
      ; mutating the copy does not affect the original
      (is (let [idx 0
                backup-value (.getInput a idx)]
            (.setInput b idx (inc backup-value))
            (= (.getInput a idx) backup-value)))))
  (testing "clone is thread-safe"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)
          as (doall (map agent (repeat 10000 state)))]
      (doseq [a as] (send-off a prng/clone))
      (apply await as)
      (is (every? #(= % state) (map deref as))))))

(deftest test-next-state
  (testing "next-state returns a new, distinct state"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)]
      (not= state (prng/next-state state))))
  (testing "next-state is a pure function"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)]
      (= (prng/next-state state) (prng/next-state state))))
  (testing "next-state is thread-safe"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)
          as (doall (map agent (repeat 10000 state)))]
      (doseq [a as] (send-off a prng/next-state))
      (apply await as)
      (is (= 1 (count (distinct (map deref as))))))))

(deftest test-jump-state
  (testing "jump-state jumps ahead by the requested number of iterations"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)
          nth-state (fn [state n]
                      (->> state
                        (iterate prng/next-state)
                        (drop n)
                        (first)))]
      (are [jump-size] (= (prng/jump-state state jump-size)
                          (nth-state state jump-size))
           0
           1
           32768
           100000)))
  (testing "jump-state jumps behind by the requested number of iterations"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)
          nth-state (fn [state n]
                      (->> state
                        (iterate prng/next-state)
                        (drop n)
                        (first)))]
      (are [jump-size] (= state
                          (-> state
                            (prng/jump-state jump-size)
                            (nth-state (- jump-size))))
           -1
           -32768
           -100000)))
  (testing "jump-state is commutative"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)]
      (is (= ; these two sequences of jump sizes are permutations
            (reduce #(prng/jump-state %1 %2) state [100 1000 1 10])
            (reduce #(prng/jump-state %1 %2) state [1 10 1000 100])))))
  (testing "jump-state is associative"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)]
      (is (= ; these two sequences of jump sizes sum to the same total
            (reduce #(prng/jump-state %1 %2) state [100 1000 1 10])
            (reduce #(prng/jump-state %1 %2) state [70 1041])))))
  (testing "jump-state throws exception when jump-size is not an integer"
    (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
          state (tf/seed-state seed)
          jump-size 100.0]
      (is (thrown?
            IllegalArgumentException
            (prng/jump-state state jump-size))))))

(deftest test-state->long
  (testing "->long returns a long"
    (let [n 1000
          valid? #(and
                    (integer? %)
                    (instance? Long %))]
      (are [initial-state] (->> initial-state
                             (iterate prng/next-state)
                             (map prng/->long)
                             (take n)
                             (every? valid?))
           (tf/seed-state (long-array [0x12345 0x23456 0x34567 0x45678]))
           (tf/seed-state (long-array [-1 -1 -1 -1]))
           (tf/seed-state (long-array [0 0 0 0 0 0 0 0])))))
  (testing "->long is a pure function"
    (are [state] (let [state-backup (prng/clone state)]
                   (and
                     (= (prng/->long state) (prng/->long state))
                     (= state state-backup)))
         (tf/seed-state (long-array [0x12345 0x23456 0x34567 0x45678]))
         (tf/seed-state (long-array [-1 -1 -1 -1]))
         (tf/seed-state (long-array [0 0 0 0 0 0 0 0])))))

(deftest test-state->double
  (testing "->double returns a double on the unit interval"
    (let [n 1000
          valid? #(and
                    (float? %)
                    (instance? Double %)
                    (> % 0.0)
                    (< % 1.0))]
      (are [initial-state] (->> initial-state
                             (iterate prng/next-state)
                             (map prng/->double)
                             (take n)
                             (every? valid?))
           (tf/seed-state (long-array [0x12345 0x23456 0x34567 0x45678]))
           (tf/seed-state (long-array [-1 -1 -1 -1]))
           (tf/seed-state (long-array [0 0 0 0 0 0 0 0])))))
  (testing "->double is a pure function"
    (are [state] (let [state-backup (prng/clone state)]
                   (and
                     (= (prng/->double state) (prng/->double state))
                     (= state state-backup)))
         (tf/seed-state (long-array [0x12345 0x23456 0x34567 0x45678]))
         (tf/seed-state (long-array [-1 -1 -1 -1]))
         (tf/seed-state (long-array [0 0 0 0 0 0 0 0])))))

(deftest ^:benchmark benchmark-next-state
  (with-test-out
    (testing "Benchmarking next-state function"
      (println (testing-contexts-str))
      (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
            state (tf/seed-state seed)]
        (quick-bench (prng/next-state state))))))

(deftest ^:benchmark benchmark-jump-state
  (with-test-out
    (testing "Benchmarking jump-state function"
      (println (testing-contexts-str))
      (let [seed (long-array [0x12345 0x23456 0x34567 0x45678])
            state (tf/seed-state seed)
            jump-size 1000000000] ; 1 billion!
        (quick-bench (prng/jump-state state jump-size))))))
