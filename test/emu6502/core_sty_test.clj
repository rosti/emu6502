(ns emu6502.core-sty-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest sty-instruction-test
  (testing "STY zeropage works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :84 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (read-byte mem-map 0x20)))))
  (testing "STY zeropage,X works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :00)
                    (data-area 0x400 :94 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (read-byte mem-map 0x21)))))
  (testing "STY absolute works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :8C :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :Y  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (read-byte mem-map 0x205))))))
