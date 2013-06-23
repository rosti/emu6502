(ns emu6502.core-dec-test
  (:use clojure.test
        emu6502.core
        emu6502.memory-map))

(deftest dec-instruction-test
  (testing "DEC zeropage works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :81)
                    (data-area 0x400 :C6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (read-byte mem-map 0x20))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "DEC zeropage works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :01)
                    (data-area 0x400 :C6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x20))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "DEC zeropage,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :81)
                    (data-area 0x400 :D6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (read-byte mem-map 0x21))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "DEC zeropage,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x20 :00 :01)
                    (data-area 0x400 :D6 :20))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x21))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "DEC absolute works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :81)
                    (data-area 0x400 :CE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (read-byte mem-map 0x205))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "DEC absolute works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :01)
                    (data-area 0x400 :CE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x205))
               (= 0x27 (get-reg cpu-state :P))))))
  (testing "DEC absolute,X works and sets N flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :81)
                    (data-area 0x400 :DE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x80 (read-byte mem-map 0x206))
               (= 0xA4 (get-reg cpu-state :P))))))
  (testing "DEC absolute,X works and sets Z flag"
    (let [mem-map (-> (empty-memory-map)
                    (data-area 0x205 :00 :01)
                    (data-area 0x400 :DE :05 :02))
          cpu-state (new-cpu-state mem-map)]
      (set-reg cpu-state :X  0x01)
      (set-reg cpu-state :PC 0x400)
      (run-single cpu-state)
      (is (and (= 0x00 (read-byte mem-map 0x206))
               (= 0x27 (get-reg cpu-state :P)))))))
 
