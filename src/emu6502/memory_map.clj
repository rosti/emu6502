(ns emu6502.memory-map
  (:use emu6502.utils))

(defn empty-memory-map [] [])

(defn area-start
  "Get start location of an area"
  [area]
  (:start area))

(defn area-end
  "Get end location of an area"
  [area]
  (:end area))

(defn area-get-byte
  "Get the get-byte function of an area"
  [area]
  (:get-byte area))

(defn area-put-byte
  "Get the put-byte function of an area"
  [area]
  (:put-byte area))

(defn area-data
  "Get the data item of an area"
  [area]
  (:data area))

(def dummy-area {:start 0x0000 :end 0xFFFF
                 :get-byte (constantly 0)
                 :put-byte (fn [& args])
                 :data nil})

(defn map-area
  "Maps an area of the received memory map and returns new memory map."
  [memory-map start end get-byte put-byte data]
  (conj memory-map {:start start :end end
                    :get-byte get-byte :put-byte put-byte :data data}))

(defn get-area
  "Get area definition from memory map for a given address
  or a dummy area if that address is not mapped"
  [memory-map address]
  (let [inarea? #(between? (area-start %) (area-end %) address)
        area (first (filter inarea? memory-map))]
    (if area area dummy-area)))

(defn read-byte
  "Read a byte at the given address in the memory map"
  [memory-map address]
  (let [area (get-area memory-map address)
        get-byte (area-get-byte area)
        data (area-data area)
        inaddr (- address (area-start area))]
    (get-byte data inaddr)))

(defn write-byte
  "Write a byte at the given address in the memory map"
  [memory-map address value]
  (let [area (get-area memory-map address)
        put-byte (area-put-byte area)
        data (area-data area)
        inaddr (- address (area-start area))]
    (put-byte data inaddr value)))

(defn read-word
  "Read a word from a given address in the memory map
  Note that the word is read in Little Endian format (as is done in real 6502)"
  [memory-map address]
  (let [low-part (read-byte memory-map address)
        high-part (read-byte memory-map (inc address))]
    (bit-or low-part (bit-shift-left high-part 8))))

(defn write-word
  "Write a word to a given address in the memory map
  Note that the word is written in Little Endian format (as is done in real 6502)"
  [memory-map address word]
  (let [low-part (bit-and word 0xFF)
        high-part (bit-and (bit-shift-right word 8) 0xFF)]
    (write-byte memory-map address low-part)
    (write-byte memory-map (inc address) high-part)))

(defn data-area
  "Map a byte array to an area"
  [memory-map start & byte-arr]
  (let [array (apply barr byte-arr)
        end (dec (+ start (count array)))]
    (map-area memory-map start end aget aset-short array)))

(defn bss-area
  "Map a zeroed array as an area"
  [memory-map start size]
  (let [array (short-array size)
        end (dec (+ start size))]
    (map-area memory-map start end aget aset-short array)))

(defn cmp-data
  "Compare the data at a given address in the memory map with the one provided"
  [memory-map address & byte-arr]
  (let [data (vec (apply barr byte-arr))
        data-size (count data)
        map-data (mapv #(read-byte memory-map %)
                       (range address (+ address data-size)))]
    (= data map-data)))

