(ns emu6502.core-inc-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest inc-instruction-test
  (testing "INC zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :7F)
                    (data-area 0x400 :E6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (read-byte mem-map 0x20)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "INC zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :FF)
                    (data-area 0x400 :E6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x20)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "INC zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :7F)
                    (data-area 0x400 :F6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (read-byte mem-map 0x21)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "INC zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :FF)
                    (data-area 0x400 :F6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x21)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "INC absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :7F)
                    (data-area 0x400 :EE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (read-byte mem-map 0x205)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "INC absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :FF)
                    (data-area 0x400 :EE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x205)))
      (is (= 0x26 (get-reg cpu-state :P)))))
  (testing "INC absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :7F)
                    (data-area 0x400 :FE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x80 (read-byte mem-map 0x206)))
      (is (= 0xA4 (get-reg cpu-state :P)))))
  (testing "INC absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :FF)
                    (data-area 0x400 :FE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x206)))
      (is (= 0x26 (get-reg cpu-state :P))))))
 
