(ns emu6502.utils-test
  (:use clojure.test
        emu6502.utils))

(deftest barr-test
  (testing "barr with no args is empty"
    (is (empty? (barr))))
  (testing "barr results are the same as manual byte array"
    (let [generated (vec (barr :3D :5A :6c :33))
          realone [0x3D 0x5A 0x6C 0x33]]
      (is (= realone generated)))))

(deftest between-test
  (testing "between returns true on correct input"
    (is (between? 3 5 4)))
  (testing "between returns false when value is before range"
    (is-not (between? 3 5 1)))
  (testing "between returns false when value is after range"
    (is-not (between? 3 5 6)))
  (testing "between returns true when bottom boundary"
    (is (between? 3 5 3)))
  (testing "between returns true when top boundary"
    (is (between? 3 5 5))))

