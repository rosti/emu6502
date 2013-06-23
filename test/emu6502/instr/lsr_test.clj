(ns emu6502.core-lsr-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest lsr-instruction-test
  (testing "LSR accumulator works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :4A))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (get-reg cpu-state :A))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "LSR zeropage works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :01)
                    (data-area 0x400 :46 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x20))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "LSR zeropage,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :01)
                    (data-area 0x400 :56 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x21))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "LSR absolute works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :01)
                    (data-area 0x400 :4E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x205))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "LSR absolute,X works and sets Z,C flags"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :01)
                    (data-area 0x400 :5E :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x206))
               (= 0x27 (get-reg cpu-state :P)))))))
 
