(ns emu6502.core-ora-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest ora-instruction-test
  (testing "ORA immidiate works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :09 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA immidiate works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :09 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :05 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :05 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :15 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80 :00)
                    (data-area 0x400 :15 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :0D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :0D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :1D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80 :00)
                    (data-area 0x400 :1D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA absolute,Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :19 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA absolute,Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80 :00)
                    (data-area 0x400 :19 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA (zeropage,X) works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :01 :20)
                    (data-area 0x807 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA (zeropage,X) works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :01 :20)
                    (data-area 0x807 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P))))))
  (testing "ORA (zeropage),Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :11 :20)
                    (data-area 0x807 :00 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (get-reg cpu-state :A))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "ORA (zeropage),Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :11 :20)
                    (data-area 0x807 :80 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x26 (get-reg cpu-state :P)))))))
 
