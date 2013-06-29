(ns emu6502.core-cpy-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest cpy-instruction-test
  (testing "CPY immidiate works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :C0 :80))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CPY immidiate works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :C0 :7F))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CPY zeropage works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :C4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CPY zeropage works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :7E)
                    (data-area 0x400 :C4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CPY absolute works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :CC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CPY absolute works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :7F)
                    (data-area 0x400 :CC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P))))))

