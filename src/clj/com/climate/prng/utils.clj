(ns com.climate.prng.utils
  "Shared helper functions for pseudorandom number generators.")

(defn long->double
  "Given a long word (in which all bits are assumed to be significant),
  use the last 52 bits of its binary representation to create a double
  between 0.0 and 1.0."
  [^long x]
  (-> x
    ; Zero the first 12 bits, leaving a 52-bit mantissa
    (bit-and 0x000fffffffffffff)
    ; Multiply by 0x1.0p-52d (unfortunately Clojure does not recognize
    ; hexademical literals) to set the exponent
    (unchecked-multiply (Double/longBitsToDouble 0x3cb0000000000000))))
