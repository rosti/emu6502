(ns emu6502.core-branch-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest branch-instructions-test
  (testing "BPL takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :10 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BPL does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :10 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0xA4)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BMI takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :30 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0xA4)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BMI does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :30 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BVC takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :50 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BVC does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :50 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x64)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BVS takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :70 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x64)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BVS does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :70 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BCC takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :90 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BCC does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :90 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x25)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BCS takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :B0 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x25)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BCS does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :B0 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BNE takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :D0 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BNE does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :D0 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x26)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC)))))
  (testing "BEQ takes a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :F0 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :P  0x26)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0404 (get-reg cpu-state :PC)))))
  (testing "BEQ does not take a jump"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :F0 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0402 (get-reg cpu-state :PC))))))
 
