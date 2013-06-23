(ns emu6502.core-eor-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest eor-instruction-test
  (testing "EOR immidiate works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :49 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR immidiate works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :49 :FF))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :45 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :FF)
                    (data-area 0x400 :45 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :55 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :FF)
                    (data-area 0x400 :55 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :4D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :FF)
                    (data-area 0x400 :4D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :5D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :FF)
                    (data-area 0x400 :5D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR absolute,Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :59 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR absolute,Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :FF)
                    (data-area 0x400 :59 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR (zeropage,X) works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :41 :20)
                    (data-area 0x807 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR (zeropage,X) works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :41 :20)
                    (data-area 0x807 :FF))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "EOR (zeropage),Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :51 :20)
                    (data-area 0x807 :00 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "EOR (zeropage),Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :51 :20)
                    (data-area 0x807 :00 :FF))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P)))))))
 
