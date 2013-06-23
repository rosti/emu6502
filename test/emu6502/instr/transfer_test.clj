(ns emu6502.core-transfer-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest transfer-fromA-test
  (testing "TAX works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :AA))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :X))))))
  (testing "TAX works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :AA))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xA4 (get-reg cpu-state :P))
               (= 0xFF (get-reg cpu-state :X))))))
  (testing "TAY works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :Y))))))
  (testing "TAY works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xA4 (get-reg cpu-state :P))
               (= 0xFF (get-reg cpu-state :Y)))))))

(deftest transfer-fromX-test
  (testing "TXA works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :8A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :A))))))
  (testing "TXA works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :8A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xA4 (get-reg cpu-state :P))
               (= 0xFF (get-reg cpu-state :A))))))
  (testing "TXS works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :9A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :S))))))
  (testing "TXS works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :9A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xA4 (get-reg cpu-state :P))
               (= 0xFF (get-reg cpu-state :S)))))))

(deftest tya-instruction-test
  (testing "TYA works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :98))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :A))))))
  (testing "TYA works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :98))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xA4 (get-reg cpu-state :P))
               (= 0xFF (get-reg cpu-state :A)))))))

(deftest tsx-instruction-test
  (testing "TSX works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :BA))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0xFF)
      (set-reg cpu-state :S  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :X))))))
  (testing "TSX works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :BA))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xA4 (get-reg cpu-state :P))
               (= 0xFF (get-reg cpu-state :X)))))))
 
