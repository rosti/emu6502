(ns emu6502.core-cmp-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest cmp-instruction-test
  (testing "CMP immidiate works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :C9 :7E))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP immidiate works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :C9 :7F))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP zeropage works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :7E)
                    (data-area 0x400 :C5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP zeropage works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :7E)
                    (data-area 0x400 :C5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP zeropage,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :7E)
                    (data-area 0x400 :D5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP zeropage,X works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :7F)
                    (data-area 0x400 :D5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP absolute works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :7E)
                    (data-area 0x400 :CD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP absolute works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :7F)
                    (data-area 0x400 :CD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP absolute,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :7E)
                    (data-area 0x400 :DD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP absolute,X works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :7F)
                    (data-area 0x400 :DD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP absolute,Y works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :7E)
                    (data-area 0x400 :D9 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP absolute,Y works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :7F)
                    (data-area 0x400 :D9 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP (zeropage,X) works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :C1 :20)
                    (data-area 0x807 :7E))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP (zeropage,X) works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :C1 :20)
                    (data-area 0x807 :7F))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CMP (zeropage),Y works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :D1 :20)
                    (data-area 0x807 :00 :7E))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CMP (zeropage),Y works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :D1 :20)
                    (data-area 0x807 :00 :7F))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x00)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P))))))

