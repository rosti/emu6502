(ns emu6502.core-asl-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest asl-instruction-test
  (testing "ASL accumulator works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :0A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x55)
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xAA (get-reg cpu-state :A)))
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "ASL accumulator works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :0A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (get-reg cpu-state :A)))
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "ASL zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :55)
                    (data-area 0x400 :06 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xAA (read-byte mem-map 0x20)))
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "ASL zeropage works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :06 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x20)))
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "ASL zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :55)
                    (data-area 0x400 :16 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xAA (read-byte mem-map 0x21)))
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "ASL zeropage,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :16 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x21)))
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "ASL absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :55)
                    (data-area 0x400 :0E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xAA (read-byte mem-map 0x205)))
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "ASL absolute works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :0E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x205)))
      (is (= 0x27 (get-reg cpu-state :P)))))
  (testing "ASL absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :55)
                    (data-area 0x400 :1E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0xAA (read-byte mem-map 0x206)))
      (is (= 0xA0 (get-reg cpu-state :P)))))
  (testing "ASL absolute,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :1E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x00 (read-byte mem-map 0x206)))
      (is (= 0x27 (get-reg cpu-state :P))))))
 
