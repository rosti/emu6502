(ns emu6502.utils)

(defn barr
  "A helper function for easy writing of some bytes to a Java short array."
  [& bytes]
  (short-array (map #(Short/parseShort (name %) 16) bytes)))

(defn between?
  "Check if value is between start and end"
  [start end value]
  (and (>= value start) (>= end value)))

(defn includes?
  "Check if element is in collection"
  [col el]
  (not (empty? (filter #(= % el) col))))

(defn bytify
  [value]
  (bit-and value 0xff))

(defn wordify
  [value]
  (bit-and value 0xffff))

(defn inc-byte
  [value]
  (bytify (inc value)))

(defn dec-byte
  [value]
  (bytify (+ value 0xFF)))

(defn inc-word
  [value]
  (wordify (inc value)))

(defn dec-word
  [value]
  (wordify (+ value 0xFFFF)))

(defn same-sign?
  [a b]
  (zero? (bit-and (bit-xor a b) 0x80)))

(defn each
  [func coll]
  (doseq [x coll] (func x)))

