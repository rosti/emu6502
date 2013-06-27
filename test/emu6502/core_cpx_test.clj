(ns emu6502.core-cpx-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest cpx-instruction-test
  (testing "CPX immidiate works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :E0 :7E))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CPX immidiate works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :E0 :7F))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CPX zeropage works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :7E)
                    (data-area 0x400 :E4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CPX zeropage works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :7E)
                    (data-area 0x400 :E4 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "CPX absolute works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :7E)
                    (data-area 0x400 :EC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "CPX absolute works, sets N flag and clears C flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :7F)
                    (data-area 0x400 :EC :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x00)
      (set-reg cpu-state :P  0x23)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xA0 (get-reg cpu-state :P))))))

