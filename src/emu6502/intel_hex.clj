(ns emu6502.intel-hex
  (:use emu6502.memory-map
        emu6502.utils
        [clojure.string :only [split-lines trim]]))

(defn load-record
  [mem-map record]
  (let [byte-vec (mapv #(Short/parseShort % 16)
                       (re-seq  #"[0-9A-Fa-f][0-9A-Fa-f]" record))
        data-len (nth byte-vec 0)
        address (bit-or (bit-shift-left (nth byte-vec 1) 8) (nth byte-vec 2))
        rec-type (nth byte-vec 3)]
    (when (zero? rec-type)
      (dotimes [off data-len]
        (write-byte mem-map (+ address off) (nth byte-vec (+ 4 off)))))))

(defn load-str
  "Load string in Intel HEX format into the memory map"
  [mem-map ihex-str]
  (->> ihex-str
       split-lines
       (map trim)
       (filter #(.startsWith % ":"))
       (each #(load-record mem-map %))))

(defn load-path
  "Load Intel HEX from file into the memory map"
  [mem-map path]
  (load-str mem-map (slurp path)))

