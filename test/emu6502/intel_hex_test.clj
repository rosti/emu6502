(ns emu6502.intel-hex-test
  (:use clojure.test
        emu6502.intel-hex
        emu6502.memory-map
        emu6502.utils))

(def sample-ihex-code
":10010000214601360121470136007EFE09D2190140
:100110002146017EB7C20001FF5F16002148011988
:10012000194E79234623965778239EDA3F01B2CAA7
:100130003F0156702B5E712B722B732146013421C7
:00000001FF")

(deftest intel-hex-tests
  (testing "Load correct Intel HEX code from string"
    (let [mem-map (-> (empty-memory-map) (bss-area 0x100 0x100))]
      (load-str mem-map sample-ihex-code)
      (is (= 0x3F (read-byte mem-map 0x0130)))
      (is (= 0x23 (read-byte mem-map 0x0123)))
      (is (= 0x19 (read-byte mem-map 0x011F)))
      (is (= 0x00 (read-byte mem-map 0x0109)))))
  (testing "Valid line without ':' is not loaded"
    (let [mem-map (-> (empty-memory-map) (bss-area 0x100 0x100))
          ihex-code "10010000214601360121470136007EFE09D2190140"]
      (load-str mem-map ihex-code)
      (is (not= 0x21 (read-byte mem-map 0x0100)))))
  (testing "Too short line throws IndexOutOfBoundsException exception"
    (let [mem-map (-> (empty-memory-map) (bss-area 0x100 0x100))
          ihex-code ":100100002146013601214701"]
      (is (thrown? IndexOutOfBoundsException (load-str mem-map ihex-code))))))
