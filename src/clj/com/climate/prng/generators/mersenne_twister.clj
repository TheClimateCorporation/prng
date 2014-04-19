(ns com.climate.prng.generators.mersenne-twister
  "A Clojure implementation of the 64-bit Mersenne Twister MT19937.

  A reference implementation in C (distributed under a MIT-style
  license) and related documentation can be found at

      http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html

  This implementation uses the jump-ahead algorithm described in

      Haromoto H., Matsumoto M., Nishimura T., Panneton F., L'Ecuyer P.
      (2008) Efficient jump ahead for F2-linear random number
      generators. INFORMS Journal on Computing 20(3): 385-390.
      http://dx.doi.org/10.1287/ijoc.1070.0251
  "
  (:require
    [clojure.java.io :refer [file input-stream resource]]
    [lazymap.core :refer [delayed-sorted-map]]
    [com.climate.prng.core :as prng]
    [com.climate.prng.utils :refer [long->double]])
  (:import
    [java.io DataInputStream]
    [org.bouncycastle.pqc.math.linearalgebra GF2Polynomial]
    [com.climate.prng.generators.mersenne_twister
     BitShift
     MersenneTwisterState]))

(set! *warn-on-reflection* true)

; Constants

(def ^:const K
  "Size of the MT19937 state vector, in bits"
  19937)

(def ^:const N
  "Number of 64-bit words that are used to represent the MT19937 state
  vector"
  312)

(def ^:const ^:private M
  "A lag parameter for the MT19937 state transition function"
  156)

(def ^:const ^:private MAG01
  "A XOR paramter for the MT19937 state transition function"
  [0x0 -5403634167711393303]) ; 0xB5026F5AA96619E9

(def ^:const ^:private UM
  "Mask for the most significant 33 bits of the array of 64-bit words
  that represent the MT19937 state vector"
  -2147483648) ; 0xFFFFFFFF80000000

(def ^:const ^:private LM
  "Mask for the most significant 31 bits of the array of 64-bit words
  that represent the MT19937 state vector"
  2147483647) ; 0x7FFFFFFF

; Implementation

(defn- get-polynomial
  "Create a GF2Polynomial instance from an array of integers that are
  read from a resource, file, input stream, etc. The polynomial is
  assumed to have K=19937 coefficients, corresponding to the size of the
  MT19937 state vector."
  [x]
  (with-open [is (input-stream x)
              dis (DataInputStream. is)]
    (let [len (long (Math/ceil (/ (double K) Integer/SIZE)))
          ia (int-array len)]
      (dotimes [idx (alength ia)]
        (aset-int ia idx (.readInt dis)))
      (GF2Polynomial. K ia))))

(def ^:private minimal-polynomial
  "A Delay that returns a GF2Polynomial instance that represents the
  minimal polynomial of the MT19937 state transition function"
  (delay (get-polynomial (resource "minimal-polynomial"))))

(def ^:dynamic *remainder-polynomials*
  "A lazy sorted map that computes remainder polynomials

      $ t^J \\bmod{\\phi(t)} $

  for select values of the exponent J, where

      $ \\phi(t) $

  is the minimal polynomial of the MT19937 state transition function.
  The values in this lazy sorted map are GF2Polynomial instances.

  The `jump-state` function looks up the remainder polynomial for a
  given value of J to jump ahead by J steps.

  This var is dynamic to allow thread-local bindings."
  (let [prefix "remainder-polynomials"
        resources (-> (resource prefix)
                    (.toURI)
                    (file)
                    (.listFiles))
        jump-sizes (->> resources
                     (map #(Long/parseLong (.getName ^java.io.File %)))
                     (sort))
        delay-fn (fn [jump-size]
                   (delay
                     (get-polynomial
                       (resource (format "%s/%d" prefix jump-size)))))]
    (apply
      delayed-sorted-map
      (interleave jump-sizes (map delay-fn jump-sizes)))))

(defn- unsigned-bit-shift-right
  "Bitwise shift right, without sign-extension.

  Equivalent to the Java >>> operator."
  [x n]
  (. BitShift unsignedShiftRight x n))

(defn- temper
  "Transform bits to improve equidistribution. This is the standard
  tempering transform of MT19937."
  [^long x]
  (let [x (-> x
            (unsigned-bit-shift-right 29)
            (bit-and 6148914691236517205) ; 0x5555555555555555
            (bit-xor x))
        x (-> x
            (bit-shift-left 17)
            (bit-and 8202884508482404352) ; 0x71D67FFFEDA60000
            (bit-xor x))
        x (-> x
            (bit-shift-left 37)
            (bit-and -2270628950310912) ; 0xFFF7EEE000000000
            (bit-xor x))
        x (-> x
            (unsigned-bit-shift-right 43)
            (bit-xor x))]
    x))

(defn- next-state!
  "Given an initial state, mutate the state in place to advance by one
  step and return nil. This impure function is intended for private
  use."
  [^MersenneTwisterState state]
  (let [x (bit-or
            (bit-and (.get state 0) UM)
            (bit-and (.get state 1) LM))
        new-word (bit-xor
                   (.get state M)
                   (unsigned-bit-shift-right x 1)
                   (get MAG01 (bit-and x 1)))]
    (.set state 0 new-word))
  (.rotateLeft state))

(defn- jump-state!
  "Given an initial state and a jump size, mutate the state in place to
  jump ahead by the specified jump size and return nil. This impure
  function is intended for private use.

  This function can not jump \"backwards\". It will throw an exception
  if the jump size is negative."
  [^MersenneTwisterState state jump-size]
  (cond

    ; negative jump-size is not supported
    (neg? jump-size)
    (throw
      (IllegalArgumentException.
        "Negative jump size is not supported"))

    ; jump-size equals a precomputed jump size
    (contains? *remainder-polynomials* jump-size)
    (let [initial-state (.clone state) ; snapshot the current state
          ^GF2Polynomial poly (*remainder-polynomials* jump-size)]
      ; set the state to all zeros
      (dotimes [i (.size state)]
        (.set state i 0))
      (loop [i (dec K)]
        (next-state! state)
        (when (.testBit poly i)
          (.addToThis state initial-state))
        (if (zero? i)
          nil
          (recur (dec i)))))

    ; jump-size is smaller than the smallest precomputed jump size
    (let [precomputed-jump-sizes (keys *remainder-polynomials*)]
      (or
        (empty? precomputed-jump-sizes)
        (< jump-size (apply min precomputed-jump-sizes))))
    (dotimes [_ jump-size]
      (next-state! state))

    ; jump-size must be greater than the smallest precomputed jump size
    :else
    (let [best-jump-size (->> (keys *remainder-polynomials*)
                           (filter #(< % jump-size))
                           (apply max))]
      (do
        ; Jump as close as possible to the target state without
        ; overshooting
        (jump-state! state best-jump-size)
        ; Jump the remainder
        (jump-state! state (- jump-size best-jump-size))
        nil))))

(defn- seed-long*
  [^long seed]
  (let [^MersenneTwisterState state (MersenneTwisterState. (int N))]
    (.set state 0 seed)
    (doseq [i (range 1 N)
            :let [prev (.get state (dec i))]]
      ; The equivalent C code is
      ;
      ;     mt[mti] = (UINT64_C(6364136223846793005) * (mt[mti-1] ^ (mt[mti-1] >> 62)) + mti);
      ;
      ; mt is array of uint64_t
      (->> (bit-xor prev (unsigned-bit-shift-right prev 62))
        (unchecked-multiply 6364136223846793005)
        (unchecked-add i)
        (.set state i)))
    state))

(defn- seed-longs*
  [^longs seed]
  (when (zero? (alength seed))
    (throw
      (IllegalArgumentException. "seed array must be non-empty")))
  (let [^MersenneTwisterState state (seed-long* 19650218)
        n (alength seed)
        k-max (max N n)
        i (atom 1)
        j (atom 0)]
    ; TODO rewrite in Clojure style instead of this imperative nonsense
    (doseq [k (range k-max 0 -1)
            :let [prev (.get state (dec @i))]]
      (.set state @i (->> (bit-xor prev (unsigned-bit-shift-right prev 62))
                       (unchecked-multiply 3935559000370003845)
                       (bit-xor (.get state @i))
                       (unchecked-add (aget seed @j))
                       (unchecked-add @j)))
      (swap! i inc)
      (when (>= @i N)
        (.set state 0 (.get state (dec N)))
        (swap! i (constantly 1)))
      (swap! j inc)
      (when (>= @j n)
        (swap! j (constantly 0))))
    (doseq [k (range (dec k-max) 0 -1)
            :let [prev (.get state (dec @i))]]
      (.set state @i (->> (bit-xor prev (unsigned-bit-shift-right prev 62))
                       (unchecked-multiply 2862933555777941757)
                       (bit-xor (.get state @i))
                       (unchecked-add (- @i))))
      (swap! i inc)
      (when (>= @i N)
        (.set state 0 (.get state (dec N)))
        (swap! i (constantly 1))))
    (.set state 0 (bit-shift-left 1 63))
    state))

(extend-type MersenneTwisterState
  prng/PRNGState

  (clone [state]
    (.clone state))

  (next-state [state]
    (doto (prng/clone state)
      (next-state!)))

  (jump-state [state jump-size]
    (when-not (integer? jump-size)
      (throw
        (IllegalArgumentException. "jump-size must be an integer")))
    (doto (prng/clone state)
      (jump-state! jump-size)))

  (->long [state]
    (temper (.get state (dec (.size state)))))

  (->double [state]
    (long->double (prng/->long state))))

(defmulti seed-state
  "Initialize a MersenneTwisterState with a provided seed. The seed may
  be a long or a long[]."
  class)

(defmethod seed-state (class (long 0))
  [seed]
  (doto (seed-long* seed)
    ; Advance the initial state by one step to match the output of the
    ; reference implementation.
    (next-state!)))

(defmethod seed-state (class (long-array 0))
  [seed]
  (doto (seed-longs* seed)
    ; Advance the initial state by one step to match the output of the
    ; reference implementation.
    (next-state!)))

