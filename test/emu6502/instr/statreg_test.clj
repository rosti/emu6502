(ns emu6502.core-statreg-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest statreg-instructions-test
  (testing "CLC works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :18))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x21)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x20 (get-reg cpu-state :P)))))
  (testing "SEC works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :38))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x25 (get-reg cpu-state :P)))))
  (testing "CLI works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :58))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x20 (get-reg cpu-state :P)))))
  (testing "SEI works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :78))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x20)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x24 (get-reg cpu-state :P)))))
  (testing "CLD works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :D8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x28)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x20 (get-reg cpu-state :P)))))
  (testing "SED works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :F8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x2C (get-reg cpu-state :P)))))
  (testing "CLV works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :B8))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x60)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x20 (get-reg cpu-state :P))))))

