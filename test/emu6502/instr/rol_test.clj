(ns emu6502.core-rol-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest rol-instruction-test
  (testing "ROL accumulator works and sets C,N flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :2A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xD5)
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xAB (get-reg cpu-state :A))
               (= 0xA1 (get-reg cpu-state :P))))))
  (testing "ROL accumulator works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :2A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x80)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "ROL zeropage works and sets C,N flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :D5)
                    (data-area 0x400 :26 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xAB (read-byte mem-map 0x20))
               (= 0xA1 (get-reg cpu-state :P))))))
  (testing "ROL zeropage works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :80)
                    (data-area 0x400 :26 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x20))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "ROL zeropage,X works and sets C,N flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :D5)
                    (data-area 0x400 :36 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xAB (read-byte mem-map 0x21))
               (= 0xA1 (get-reg cpu-state :P))))))
  (testing "ROL zeropage,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :80)
                    (data-area 0x400 :36 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x21))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "ROL absolute works and sets C,N flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :D5)
                    (data-area 0x400 :2E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xAB (read-byte mem-map 0x205))
               (= 0xA1 (get-reg cpu-state :P))))))
  (testing "ROL absolute works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :80)
                    (data-area 0x400 :2E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x205))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "ROL absolute,X works and sets C,N flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :D5)
                    (data-area 0x400 :3E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xAB (read-byte mem-map 0x206))
               (= 0xA1 (get-reg cpu-state :P))))))
  (testing "ROL absolute,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :80)
                    (data-area 0x400 :3E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x206))
               (= 0x27 (get-reg cpu-state :P)))))))
 
