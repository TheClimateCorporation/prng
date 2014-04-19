(ns com.climate.prng.core
  "Core library for pseudorandom number generators.")

(defprotocol PRNGState
  "Protocol for functional pseudorandom number generators"

  (clone [state]
    "Return a copy of this state.")

  (next-state [state]
    "Return a new state that is one step ahead of this state.")

  (jump-state [state jump-size]
    "Return a new state that is `jump-size` steps ahead of this state.")

  (->long [state]
    "Read a long from this state.")

  (->double [state]
    "Read a double from this state."))
