(ns emu6502.core-ldx-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest ldx-instruction-test
  (testing "LDX immidiate works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A2 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :X))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "LDX immidiate works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A2 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :X))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "LDX zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :A6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :X))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "LDX zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :A6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :X))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "LDX zeropage,Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :B6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :X))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "LDX zeropage,Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80 :00)
                    (data-area 0x400 :B6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :X))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "LDX absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :AE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :X))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "LDX absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :AE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :X))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "LDX absolute,Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :BE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :X))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "LDX absolute,Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80 :00)
                    (data-area 0x400 :BE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :X))
               (= 0x26 (get-reg cpu-state :P)))))))
 
