(ns emu6502.core-sta-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest sta-instruction-test
  (testing "STA zeropage works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00)
                    (data-area 0x400 :85 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x20)))))
  (testing "STA zeropage,X works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :00)
                    (data-area 0x400 :95 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x21)))))
  (testing "STA absolute works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00)
                    (data-area 0x400 :8D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x205)))))
  (testing "STA absolute,X works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :00)
                    (data-area 0x400 :9D :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x206)))))
  (testing "STA absolute,Y works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :00)
                    (data-area 0x400 :99 :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x206)))))
  (testing "STA (zeropage,X) works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :00 :07 :08)
                    (data-area 0x400 :81 :20)
                    (data-area 0x807 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x807)))))
  (testing "STA (zeropage),Y works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20  :07 :08)
                    (data-area 0x400 :91 :20)
                    (data-area 0x807 :00 :00))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0x7F)
      (set-reg cpu-state :Y  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x7F (get-byte mem-map 0x808))))))
