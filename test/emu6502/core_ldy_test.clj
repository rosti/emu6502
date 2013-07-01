(ns emu6502.core-ldy-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest ldy-instruction-test
  (testing "LDY immidiate works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A0 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :Y)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDY immidiate works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A0 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :Y)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDY zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :A4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :Y)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDY zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :A4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :Y)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDY zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :B4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :Y)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDY zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80 :00)
                    (data-area 0x400 :B4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :Y)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDY absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :AC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :Y)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDY absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :AC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :Y)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDY absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :BC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :Y)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDY absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80 :00)
                    (data-area 0x400 :BC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :Y)))
      (is (= 0x26 (get-reg cpu-state :P))))))
 
