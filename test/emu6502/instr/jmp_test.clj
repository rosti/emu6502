(ns emu6502.core-jmp-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest jmp-instruction-test
  (testing "JMP absolute works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x400 :4C :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x0205 (get-reg cpu-state :PC)))))
  (testing "JMP indirect works"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :34 :12)
                    (data-area 0x400 :6C :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (= 0x1234 (get-reg cpu-state :PC))))))

