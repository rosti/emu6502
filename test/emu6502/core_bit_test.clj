(ns emu6502.core-bit-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest bit-instruction-test
  (testing "BIT zeropage works and sets N,V flags, clears Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :F0)
                    (data-area 0x400 :24 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :P  0x26)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xE4 (get-reg cpu-state :P)))))
  (testing "BIT zeropage works, sets Z flag and clears N,V flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :24 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :P  0xE4)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "BIT absolute works and sets N,V flags, clears Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :F0)
                    (data-area 0x400 :2C :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :P  0x26)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xE4 (get-reg cpu-state :P)))))
  (testing "BIT absolute works, sets Z flag and clears N,V flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :2C :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :P  0xE4)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x26 (get-reg cpu-state :P))))))

