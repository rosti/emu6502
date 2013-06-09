(ns emu6502.utils
  (:use clojure.test))

(defn barr
  "A helper function for easy writing of some bytes to a Java short array."
  [& bytes]
  (short-array (map #(Short/parseShort (name %) 16) bytes)))

(defn between?
  "Check if value is between start and end"
  [start end value]
  (and (>= value start) (>= end value)))

(defn is-not
  "A short way to say (is (not ... in tests"
  ([form] (is (not form)))
  ([form msg] (is (not form) msg)))

