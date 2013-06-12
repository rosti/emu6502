(ns emu6502.memory-map-test
  (:use clojure.test
        emu6502.memory-map
        emu6502.utils))

(defn no-read [& args] 0)
(defn no-write [& args] (is false))

(deftest memory-map-byte-test
  (testing "Read unmapped byte"
    (is (zero? (read-byte (empty-memory-map) 0x3FC))))
  (testing "Write unmapped byte"
    (let [address 0x3FC
          mem-map (empty-memory-map)]
      (write-byte mem-map address 0xFF)
      (is (zero? (read-byte mem-map address)))))
  (testing "Read mapped byte"
    (let [mem-map (-> (empty-memory-map)
                      (map-area 0x0000 0x00FF (constantly 0x3C) no-write nil)
                      (data-area 0x0200 :A5 :B3 :C6 :D7)
                      (map-area 0x4000 0xF000 (constantly 0xFF) no-write nil))]
      (is (= 0xA5 (read-byte mem-map 0x200)))))
  (testing "Write to mapped byte"
    (let [magic-byte 0x3D
          address 0x200
          mem-map (-> (empty-memory-map)
                      (bss-area 0x0100 0x1000))]
      (write-byte mem-map address magic-byte)
      (is (= magic-byte (read-byte mem-map address))))))

(deftest memory-map-word-test
  (testing "Read unmapped word"
    (is (zero? (read-word (empty-memory-map) 0x3FC))))
  (testing "Write unmapped word"
    (let [address 0x3FC
          mem-map (empty-memory-map)]
      (write-word mem-map address 0xFFFF)
      (is (zero? (read-word mem-map address)))))
  (testing "Read mapped word"
    (let [mem-map (-> (empty-memory-map)
                      (map-area 0x0000 0x00FF (constantly 0x3C) no-write nil)
                      (data-area 0x0100 :4A :A5 :3D :4B)
                      (map-area 0x4000 0xF000 (constantly 0xFF) no-write nil))]
      (is (= 0x3DA5 (read-word mem-map 0x101)))))
  (testing "Write to mapped word"
    (let [address 0x200
          magic-word 0x3DA5
          test-writer #(let [value (if (even? %2) 0xA5 0x3D)]
                         (is (= value %3)))
          mem-map (-> (empty-memory-map)
                      (map-area 0x0000 0x00FF no-read no-write nil)
                      (map-area 0x0100 0x1000 no-read test-writer nil)
                      (map-area 0x4000 0xF000 no-read no-write nil))]
      (write-word mem-map address magic-word))))

(deftest cmp-data-test
  (let [mem-map (-> (empty-memory-map)
                    (map-area 0x0000 0x00FF (constantly 0x3C) no-write nil)
                    (data-area 0x0100 :4A :A5 :3D :4B)
                    (map-area 0x4000 0xF000 (constantly 0xFF) no-write nil))]
    (testing "cmp-data returns true if byte arrays are the same"
      (is (cmp-data mem-map 0x0100 :4A :A5 :3D :4B)))
    (testing "cmp-data returns false if byte arrays are different"
      (is-not (cmp-data mem-map 0x0100 :4A :B6 :85 :4B)))))

