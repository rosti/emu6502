(ns emu6502.core-xyincdec-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest xyincdec-instructions-test
  (testing "DEX works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :CA))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x26 (get-reg cpu-state :P)))
      (is (= 0x00 (get-reg cpu-state :X)))))
  (testing "DEX works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :CA))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA4 (get-reg cpu-state :P)))
      (is (= 0xFF (get-reg cpu-state :X)))))
  (testing "INX works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :E8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x26 (get-reg cpu-state :P)))
      (is (= 0x00 (get-reg cpu-state :X)))))
  (testing "INX works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :E8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA4 (get-reg cpu-state :P)))
      (is (= 0x80 (get-reg cpu-state :X)))))
  (testing "DEY works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :88))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x26 (get-reg cpu-state :P)))
      (is (= 0x00 (get-reg cpu-state :Y)))))
  (testing "DEY works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :88))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA4 (get-reg cpu-state :P)))
      (is (= 0xFF (get-reg cpu-state :Y)))))
  (testing "INY works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :C8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x26 (get-reg cpu-state :P)))
      (is (= 0x00 (get-reg cpu-state :Y)))))
  (testing "INY works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :C8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA4 (get-reg cpu-state :P)))
      (is (= 0x80 (get-reg cpu-state :Y))))))

