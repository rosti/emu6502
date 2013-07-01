(ns emu6502.core-lda-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest lda-instruction-test
  (testing "LDA immidiate works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A9 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA immidiate works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :A9 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :A5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :A5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :B5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80 :00)
                    (data-area 0x400 :B5 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :AD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :AD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :BD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80 :00)
                    (data-area 0x400 :BD :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA absolute,Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :B9 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA absolute,Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80 :00)
                    (data-area 0x400 :B9 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA (zeropage,X) works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :A1 :20)
                    (data-area 0x807 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA (zeropage,X) works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :A1 :20)
                    (data-area 0x807 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "LDA (zeropage),Y works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :B1 :20)
                    (data-area 0x807 :00 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (get-reg cpu-state :A)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "LDA (zeropage),Y works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :B1 :20)
                    (data-area 0x807 :80 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x26 (get-reg cpu-state :P))))))
 
