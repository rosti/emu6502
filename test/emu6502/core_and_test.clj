(ns emu6502.core-and-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest and-instruction-test
  (testing "AND immidiate works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :29 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND immidiate works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :29 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :25 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :25 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :35 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :35 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :2D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :2D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :3D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :FF :80)
                    (data-area 0x400 :3D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND absolute,Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :39 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND absolute,Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :FF :80)
                    (data-area 0x400 :39 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND (zeropage,X) works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :21 :20)
                    (data-area 0x807 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND (zeropage,X) works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :21 :20)
                    (data-area 0x807 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "AND (zeropage),Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :31 :20)
                    (data-area 0x807 :00 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xF0)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "AND (zeropage),Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :31 :20)
                    (data-area 0x807 :FF :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P)))))))
 
