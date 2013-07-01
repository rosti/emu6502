(ns emu6502.core-intr-sr-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest interrupt-instructions-test
  (testing "BRK works"
    (let [mem-map (-> (empty-memory-map)
                    (bss-area  0x100 0x10)
                    (data-area 0x400 :00 :EA)
                    (data-area 0xFFFE :34 :12))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x0F)
      (set-reg cpu-state :P  0x20)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x1234 (get-reg cpu-state :PC)))
      (is (= 0x24 (get-reg cpu-state :P)))
      (is (= 0x30 (read-byte mem-map 0x10D)))
      (is (= 0x0402 (read-word mem-map 0x10E)))))
  (testing "RTI works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x100 :00 :30 :34 :12)
                    (data-area 0x400 :40))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x1234 (get-reg cpu-state :PC)))
      (is (= 0x20 (get-reg cpu-state :P))))))

(deftest subroutine-instructions-test
  (testing "JSR works"
    (let [mem-map (-> (empty-memory-map)
                    (bss-area  0x100 0x10)
                    (data-area 0x400 :20 :34 :12))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x0F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x1234 (get-reg cpu-state :PC)))
      (is (= 0x0402 (read-word mem-map 0x10E)))))
  (testing "RTS works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x100 :00 :33 :12)
                    (data-area 0x400 :60))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x1234 (get-reg cpu-state :PC))))))

