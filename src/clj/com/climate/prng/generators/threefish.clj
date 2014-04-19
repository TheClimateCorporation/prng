(ns com.climate.prng.generators.threefish
  "Counter-based pseudorandom number generator that uses the Threefish
  block cipher.

  This implementation uses the public-domain Threefish library in the
  Bouncy Castle Java package.

  This paper describes counter-based PRNGs:

      Salmon J.K., Moraes M.A., Dror R.O., Shaw D.E. (2011) Parallel
      random numbers: as easy as 1, 2, 3. Proceedings of 2011
      International Conference for High Performance Computing,
      Networking, Storage and Analysis.
      http://dx.doi.org/10.1145/2063384.2063405
  "
  (:require
    [com.climate.prng.core :as prng]
    [com.climate.prng.utils :refer [long->double]])
  (:import
    [org.bouncycastle.crypto.engines ThreefishEngine]
    [com.climate.prng.generators.threefish ThreefishState]))

(set! *warn-on-reflection* true)

; Implementation

(defn- pad-left-with-zeros
  [s n]
  (-> (format "%%%ds" n)
    (format s)
    (.replace \space \0)))

(defn- longs->big-integer
  [^longs la]
  {:post [(not (neg? (.signum %)))
          (<= (.bitLength %) (* (alength la) Long/SIZE))]}
  (let [^String s (->> la
                    (map #(pad-left-with-zeros (Long/toHexString %) 16))
                    (apply str))]
    (BigInteger. s 16)))

(defn- big-integer->seq-of-longs
  [^BigInteger bi n]
  (->> (pad-left-with-zeros (.toString bi 16) (* n 16))
    (partition 16)
    (map #(.longValue (BigInteger. ^String (apply str %) 16)))))

(defn- increment-long-array!
  "Treating a long[] as one big bitwise-encoded unsigned integer,
  mutate the array in place to update the encoded value by the provided
  increment/decrement.

  The two's complement binary representations of the long elements are
  concatenated in big-endian order, and the resulting binary
  representation is interpreted as an unsigned integer.

  The binary representation will be wrapped within the fixed number of
  bits in case of overflow. This wraparound imposes a finite period on
  the Threefish PRNG."
  [^longs la x]
  ; TODO replace these hacks with efficient bit operations
  (let [^BigInteger counter (longs->big-integer la)
        divisor (BigInteger.
                  ^String (apply str (repeat (* (alength la) 16) \f))
                  16)
        counter-plus-x (-> counter
                    (.add (BigInteger/valueOf (long x)))
                    (.remainder divisor))
        counter-plus-x (if (neg? counter-plus-x)
                         (.add counter-plus-x divisor)
                         counter-plus-x)]
    (doseq [[i l] (map-indexed
                    vector
                    (big-integer->seq-of-longs counter-plus-x (alength la)))]
      (aset-long la i l))))

(defn- jump-state!
  "Given an initial state and a jump size, mutate the state in place to
  jump ahead by the specified jump size and return nil. This impure
  function is intended for private use."
  [^ThreefishState state jump-size]
  (let [^longs la (.getInput state)]
    (increment-long-array! la jump-size)
    (.setInput state la)))

; Public interface

(extend-type ThreefishState
  prng/PRNGState

  (clone [state]
    (.clone state))

  (next-state [state]
    (doto (prng/clone state)
      (jump-state! 1)))

  (jump-state [state jump-size]
    (when-not (integer? jump-size)
      (throw
        (IllegalArgumentException. "jump-size must be an integer")))
    (doto (prng/clone state)
      (jump-state! jump-size)))

  (->long [state]
    (.getOutput state 0))

  (->double [state]
    (long->double (prng/->long state))))

(defmulti seed-state
  "Initialize a ThreefishState with a provided seed. The seed may be a
  long[] of length 4, 8, or 16."
  class)

(defmethod seed-state (class (long-array 0))
  [^longs seed]
  (ThreefishState. seed))
