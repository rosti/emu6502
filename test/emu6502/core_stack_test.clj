(ns emu6502.core-stack-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest stack-instructions-test
  (testing "PHP works"
    (let [mem-map (-> (empty-memory-map)
                    (bss-area  0x100 0x10)
                    (data-area 0x400 :08))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x0F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x24 (read-byte mem-map 0x10F))
               (= 0x0E (get-reg cpu-state :S))))))
  (testing "PLP works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x100 :00 :A0)
                    (data-area 0x400 :28))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x01 (get-reg cpu-state :S))
               (= 0xA0 (get-reg cpu-state :P))))))
  (testing "PHA works"
    (let [mem-map (-> (empty-memory-map)
                    (bss-area  0x100 0x10)
                    (data-area 0x400 :48))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :A  0xFF)
      (set-reg cpu-state :S  0x0F)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0xFF (read-byte mem-map 0x10F))
               (= 0x0E (get-reg cpu-state :S))))))
  (testing "PLA works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x100 :00 :A0)
                    (data-area 0x400 :68))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x01 (get-reg cpu-state :S))
               (= 0xA4 (get-reg cpu-state :P))
               (= 0xA0 (get-reg cpu-state :A))))))
  (testing "PLA works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x100 :00 :00)
                    (data-area 0x400 :68))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :S  0x00)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x01 (get-reg cpu-state :S))
               (= 0x26 (get-reg cpu-state :P))
               (= 0x00 (get-reg cpu-state :A)))))))

