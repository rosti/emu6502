(ns emu6502.core-stx-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest stx-instruction-test
  (testing "STX zeropage works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :86 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (read-byte mem-map 0x20)))))
  (testing "STX zeropage,Y works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :00)
                    (data-area 0x400 :96 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (read-byte mem-map 0x21)))))
  (testing "STX absolute works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :8E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (read-byte mem-map 0x205))))))
