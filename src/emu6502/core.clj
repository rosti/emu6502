(ns emu6502.core
  (:use emu6502.memory-map
        emu6502.utils))

(defn new-cpu-state
  "Create a new 6502 cpu state"
  [memory-map]
  {:A  (atom 0x00)
   :X  (atom 0x00)
   :Y  (atom 0x00)
   :S  (atom 0xFF)
   :P  (atom 0x24)
   :PC (atom (read-word memory-map 0xFFFC))
   :memory-map memory-map})

(defn get-reg
  "Get the value of a CPU register"
  [cpu-state reg]
  (deref (cpu-state reg)))

(defn set-reg
  "Set a value to a CPU register"
  [cpu-state reg value]
  (reset! (cpu-state reg) value))

(defn update-reg
  "Update value of a CPU register"
  [cpu-state reg update]
  (swap! (cpu-state reg) update))

(defn advance-pc
  "Increment PC and return its previous value"
  [cpu-state]
  (let [addr (get-reg cpu-state :PC)]
    (update-reg cpu-state :PC inc-word)
    addr))

(defn read-pc
  "Return byte at (PC) and advance PC "
  [cpu-state]
  (read-byte (cpu-state :memory-map) (advance-pc cpu-state)))

(defn invalid-opcode-error
  "Raise exception when invalid opcode is detected"
  [cpu-state opcode]
  (throw (RuntimeException. (str "Invalid opcode: " opcode))))

(defn store-reg
  "Store register to the given address"
  [cpu-state reg address]
  (write-byte (cpu-state :memory-map) address (get-reg cpu-state reg)))

(defn load-reg
  "Load register with the byte at the given address and return the value"
  [cpu-state reg address]
  (let [value (read-byte (cpu-state :memory-map) address)]
    (set-reg cpu-state reg value)
    value))

(defn transfer-reg
  "Transfer value of one register to another and return the value"
  [cpu-state source destination]
  (let [value (get-reg cpu-state source)]
    (set-reg cpu-state destination value)
    value))

; Opcode helper functions
(defn get-instr
  [opcode]
  (bit-and (bit-shift-right opcode 5) 0x07))

(defn get-addr-mode
  [opcode]
  (bit-and (bit-shift-right opcode 2) 0x07))

; Status register helpers
(def sr-flag-carry 0x01)
(def sr-flag-zero 0x02)
(def sr-flag-irqdisable 0x04)
(def sr-flag-decimal 0x08)
(def sr-flag-break 0x10)
(def sr-flag-alwaysone 0x20)
(def sr-flag-overflow 0x40)
(def sr-flag-negative 0x80)

(defn sr-update-flag
  "Set a flag in the status register when condition is true"
  [cpu-state flag condition?]
  (update-reg cpu-state :P #((if (condition?) bit-or bit-and-not) % flag)))

(defn sr-set-nz
  "Clear and set N and Z flags"
  [cpu-state value]
  (let [byte-value (bytify value)]
    (sr-update-flag cpu-state sr-flag-zero #(zero? byte-value))
    (sr-update-flag cpu-state sr-flag-negative #(not (zero? (bit-and byte-value 0x80))))))

(defn sr-set-carry
  "Clear or set carry flag if value > 0xFF"
  [cpu-state value]
  (sr-update-flag cpu-state sr-flag-carry #(> value 0xFF)))

; Address mode functions
; - return absolute address
(defn addr-mode-imm
  [cpu-state opcode]
  (advance-pc cpu-state))

(defn addr-mode-zpg
  [cpu-state opcode]
  (read-pc cpu-state))

(defn addr-mode-zpg-x
  [cpu-state opcode]
  (bytify (+ (addr-mode-zpg cpu-state opcode) (get-reg cpu-state :X))))

(defn addr-mode-zpg-y
  [cpu-state opcode]
  (bytify (+ (addr-mode-zpg cpu-state opcode) (get-reg cpu-state :Y))))

(defn addr-mode-abs
  [cpu-state opcode]
  (let [low-part (read-pc cpu-state)
        high-part (read-pc cpu-state)]
    (bit-or low-part (bit-shift-left high-part 8))))

(defn addr-mode-abs-x
  [cpu-state opcode]
  (wordify (+ (addr-mode-abs cpu-state opcode) (get-reg cpu-state :X))))

(defn addr-mode-abs-y
  [cpu-state opcode]
  (wordify (+ (addr-mode-abs cpu-state opcode) (get-reg cpu-state :Y))))

(defn addr-mode-ind
  [cpu-state opcode]
  (let [zpg-addr (read-pc cpu-state)]
    (read-word (cpu-state :memory-map) zpg-addr)))

(defn addr-mode-ind-x
  [cpu-state opcode]
  (let [zpg-addr (bytify (+ (read-pc cpu-state) (get-reg cpu-state :X)))]
    (read-word (cpu-state :memory-map) zpg-addr)))

(defn addr-mode-ind-y
  [cpu-state opcode]
  (let [zpg-addr (read-pc cpu-state)]
    (wordify (+ (read-word (cpu-state :memory-map) zpg-addr)
                (get-reg cpu-state :Y)))))

; Major instruction group BIT (cc = 00)
(defn group-bit
  [cpu-state opcode])

; Major instruction group ORA (cc = 01)
(defn bit-arithmetics
  [cpu-state address func]
  (update-reg cpu-state :A #(func % (read-byte (cpu-state :memory-map) address)))
  (get-reg cpu-state :A))

(defn instruction-ora
  [cpu-state address]
  (bit-arithmetics cpu-state address bit-or))

(defn instruction-and
  [cpu-state address]
  (bit-arithmetics cpu-state address bit-and))

(defn instruction-eor
  [cpu-state address]
  (bit-arithmetics cpu-state address bit-xor))

(defn instruction-sta
  [cpu-state address]
  (store-reg cpu-state :A address))

(defn instruction-lda
  [cpu-state address]
  (load-reg cpu-state :A address))

(defn overflow-dec?
  [a b c]
  (and (not (zero? (bit-xor (bit-shift-right c 1) c))) (same-sign? a b)))

(defn instruction-add-bin
  [cpu-state curr-a value]
  (let [full-sum (+ value curr-a (bit-and (get-reg cpu-state :P) sr-flag-carry))]
    (sr-set-carry cpu-state full-sum)
    (sr-update-flag cpu-state sr-flag-overflow #(and (not (same-sign? curr-a full-sum))
                                                     (same-sign? curr-a value)))
    full-sum))

(defn instruction-add-dec
  [cpu-state curr-a value]
  (let [full-low (+ (bit-and curr-a 0x0F)
                    (bit-and value 0x0F)
                    (bit-and (get-reg cpu-state :P) sr-flag-carry))
        trim-low (if (< full-low 0x0A) full-low (+ 0x10 (- full-low 0x0A)))
        full-value (+ trim-low (bit-and curr-a 0xF0) (bit-and value 0xF0))
        full-high (if (> full-value 0x99) (+ full-value 0x60) full-value)]
    (sr-update-flag cpu-state sr-flag-overflow #(overflow-dec? curr-a value full-value))
    full-high))

(defn instruction-add
  [cpu-state value]
  (let [curr-a (get-reg cpu-state :A)
        adder (if (zero? (bit-and (get-reg cpu-state :P) sr-flag-decimal))
                instruction-add-bin instruction-add-dec)
        full-value (adder cpu-state curr-a value)
        trim-value (bytify full-value)]
    (sr-set-carry cpu-state full-value)
    (set-reg cpu-state :A trim-value)
    trim-value))

(defn instruction-adc
  [cpu-state address]
  (instruction-add cpu-state (read-byte (cpu-state :memory-map) address)))

(defn instruction-sbc-dec
  [cpu-state value]
  (let [curr-a (get-reg cpu-state :A)
        full-low (+ (bit-and curr-a 0x0F)
                    (bit-and value 0x0F)
                    (bit-and (get-reg cpu-state :P) sr-flag-carry))
        trim-low (if (< full-low 0x10) (bit-and (- full-low 0x06) 0x0F) full-low)
        full-value (+ trim-low (bit-and curr-a 0xF0) (bit-and value 0xF0))
        full-high (if (< full-value 0x100) (bytify (+ full-value 0xA0)) full-value)
        trim-value (bytify full-high)]
    (sr-update-flag cpu-state sr-flag-overflow #(overflow-dec? curr-a value full-value))
    (sr-set-carry cpu-state full-high)
    (set-reg cpu-state :A trim-value)
    trim-value))

(defn instruction-sbc
  [cpu-state address]
  (let [value (bit-xor (read-byte (cpu-state :memory-map) address) 0xFF)
        sub (if (zero? (bit-and (get-reg cpu-state :P) sr-flag-decimal))
              instruction-add instruction-sbc-dec)]
    (sub cpu-state value)))

(defn instruction-cmp
  [cpu-state address]
  (let [result (+ 1 (get-reg cpu-state :A)
                  (bit-xor (read-byte (cpu-state :memory-map) address) 0xFF))]
    (sr-set-carry cpu-state result)
    result))

; Address mode
(def group-ora-address-modes
  [addr-mode-ind-x
   addr-mode-zpg
   addr-mode-imm
   addr-mode-abs
   addr-mode-ind-y
   addr-mode-zpg-x
   addr-mode-abs-y
   addr-mode-abs-x])

; Instruction codes
(def group-ora-instructions
  [instruction-ora
   instruction-and
   instruction-eor
   instruction-adc
   instruction-sta
   instruction-lda
   instruction-cmp
   instruction-sbc])

(defn group-ora
  "Handle instructions in the ORA group"
  [cpu-state opcode]
  ; There is no instruction STA imm (opcode 0x89)
  (if (= opcode 0x89) (invalid-opcode-error cpu-state opcode))
  (let [instruction (get-instr opcode)
        func (nth group-ora-instructions instruction)
        addr-mode (nth group-ora-address-modes (get-addr-mode opcode))
        result (func cpu-state (addr-mode cpu-state opcode))]
    ; Don't set N, Z flags if the instruction is STA
    (if (not= instruction 0x04) (sr-set-nz cpu-state result))))

; Major instruction group ASL (cc = 10)

; Subgroup transfers
(def transfer-instructions
  { 0xA8 [:A :Y]
    0x98 [:Y :A]
    0x8A [:X :A]
    0x9A [:X :S]
    0xAA [:A :X]
    0xBA [:S :X]})

(defn transfer-instruction-opcode?
  [opcode]
  (includes? (keys transfer-instructions) opcode))

(defn transfer-instruction
  [cpu-state opcode]
  (if-not (transfer-instruction-opcode? opcode)
    (invalid-opcode-error cpu-state opcode))
  (sr-set-nz cpu-state (apply transfer-reg cpu-state (transfer-instructions opcode))))

; Subgroup ASL
(defn instruction-asl
  [cpu-state old-value]
  (let [new-value (bit-shift-left old-value 1)]
    (sr-set-carry cpu-state new-value)
    new-value))

(defn instruction-lsr
  [cpu-state old-value]
  (let [new-value (bit-shift-right old-value 1)]
    (sr-update-flag cpu-state sr-flag-carry #(not (zero? (bit-and old-value 1))))
    new-value))

(defn instruction-rol
  [cpu-state old-value]
  (let [new-value (bit-or (bit-shift-left old-value 1)
                          (bit-and (get-reg cpu-state :P) 1))]
    (sr-set-carry cpu-state new-value)
    new-value))

(defn instruction-ror
  [cpu-state old-value]
  (let [new-value (bit-or (bit-shift-right old-value 1)
                          (bit-shift-left (bit-and (get-reg cpu-state :P) 1) 7))]
    (sr-update-flag cpu-state sr-flag-carry #(not (zero? (bit-and old-value 1))))
    new-value))

(defn accumulator-mode
  [func]
  (fn [cpu-state]
    (let [result (func cpu-state (get-reg cpu-state :A))]
      (set-reg cpu-state :A (bytify result))
      (sr-set-nz cpu-state result))))

(defn memory-mode
  [func]
  (fn [cpu-state address]
    (let [mem-map (cpu-state :memory-map)
          result (func cpu-state (read-byte mem-map address))]
      (write-byte mem-map address result)
      result)))

(defn instruction-stx
  [cpu-state address]
  (store-reg cpu-state :X address))

(defn instruction-ldx
  [cpu-state address]
  (load-reg cpu-state :X address))

(defn inc-dec-memory
  [cpu-state address modifier]
  (let [mem-map (cpu-state :memory-map)
        new-value (modifier (read-byte mem-map address))]
    (write-byte mem-map address new-value)
    new-value))

(defn instruction-dec
  [cpu-state address]
  (inc-dec-memory cpu-state address dec-byte))

(defn instruction-inc
  [cpu-state address]
  (inc-dec-memory cpu-state address inc-byte))

(def group-asl-mode-map
  [{:func (memory-mode instruction-asl)
      1 addr-mode-zpg
      2 (accumulator-mode instruction-asl)
      3 addr-mode-abs
      5 addr-mode-zpg-x
      7 addr-mode-abs-x}
   {:func (memory-mode instruction-rol)
      1 addr-mode-zpg
      2 (accumulator-mode instruction-rol)
      3 addr-mode-abs
      5 addr-mode-zpg-x
      7 addr-mode-abs-x}
   {:func (memory-mode instruction-lsr)
      1 addr-mode-zpg
      2 (accumulator-mode instruction-lsr)
      3 addr-mode-abs
      5 addr-mode-zpg-x
      7 addr-mode-abs-x}
   {:func (memory-mode instruction-ror)
      1 addr-mode-zpg
      2 (accumulator-mode instruction-ror)
      3 addr-mode-abs
      5 addr-mode-zpg-x
      7 addr-mode-abs-x}
   {:func instruction-stx
      1 addr-mode-zpg
      3 addr-mode-abs
      5 addr-mode-zpg-y
      7 addr-mode-abs-y}
   {:func instruction-ldx
      0 addr-mode-imm
      1 addr-mode-zpg
      3 addr-mode-abs
      5 addr-mode-zpg-y
      7 addr-mode-abs-y}
   {:func instruction-dec
      1 addr-mode-zpg
      3 addr-mode-abs
      5 addr-mode-zpg-x
      7 addr-mode-abs-x}
   {:func instruction-inc
      1 addr-mode-zpg
      3 addr-mode-abs
      5 addr-mode-zpg-x
      7 addr-mode-abs-x}])

(defn invoke-asl-subgroup
  [cpu-state opcode]
  (let [instr (get-instr opcode)
        mode (get-addr-mode opcode)
        desc (nth group-asl-mode-map instr)
        func (desc :func)
        mode-handler (desc mode)]
    (cond (and mode-handler (= mode 2)) ; Check for accumulator mode
            (mode-handler cpu-state)
          (not mode-handler)            ; Check for invalid mode for this instruction
            (invalid-opcode-error cpu-state opcode)
          :else (let [result (func cpu-state (mode-handler cpu-state opcode))]
                  ; Don't set N,Z flags if the instruction is STX
                  (if-not (= instr 4) (sr-set-nz cpu-state result))))))

(defn instruction-dex
  [cpu-state]
  (update-reg cpu-state :X dec-byte)
  (sr-set-nz cpu-state (get-reg cpu-state :X)))

(defn group-asl
  [cpu-state opcode]
  (cond (= 0xEA opcode) ; NOP instruction
          nil
        (= 0xCA opcode) ; DEX instruction
          (instruction-dex cpu-state)
        (transfer-instruction-opcode? opcode) ; Transfer mode (TAX, TXA, TSX, TXS) instruction
          (transfer-instruction cpu-state opcode)
        :else (invoke-asl-subgroup cpu-state opcode)))

(def opcode-groups
  [group-bit
   group-ora
   group-asl
   invalid-opcode-error])

(defn run-single
  "Run a single instruction"
  [cpu-state]
  (let [opcode (read-pc cpu-state)
        group-handler (nth opcode-groups (bit-and opcode 0x03))]
    (group-handler cpu-state opcode)))

