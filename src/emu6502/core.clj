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
  (sr-update-flag cpu-state sr-flag-zero #(zero? value))
  (sr-update-flag cpu-state sr-flag-negative #(not (zero? (bit-and value 0x80)))))

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
  [cpu-state opcode address]
  (bit-arithmetics cpu-state address bit-or))

(defn instruction-and
  [cpu-state opcode address]
  (bit-arithmetics cpu-state address bit-and))

(defn instruction-eor
  [cpu-state opcode address]
  (bit-arithmetics cpu-state address bit-xor))

(defn instruction-sta
  [cpu-state opcode address]
  (write-byte (cpu-state :memory-map) address (get-reg cpu-state :A)))

(defn instruction-lda
  [cpu-state opcode address]
  (let [value (read-byte (cpu-state :memory-map) address)]
    (set-reg cpu-state :A value)
    value))

(defn overflow-dec?
  [a b c]
  (and (not (zero? (bit-xor (bit-shift-right c 1) c))) (same-sign? a b)))

(defn instruction-add-bin
  [cpu-state curr-a value]
  (let [full-sum (+ value curr-a (bit-and (get-reg cpu-state :P) sr-flag-carry))]
    (sr-update-flag cpu-state sr-flag-carry #(> full-sum 0xFF))
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
    (sr-update-flag cpu-state sr-flag-carry #(> full-value 0xFF))
    (set-reg cpu-state :A trim-value)
    trim-value))

(defn instruction-adc
  [cpu-state opcode address]
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
    (sr-update-flag cpu-state sr-flag-carry #(> full-high 0xFF))
    (set-reg cpu-state :A trim-value)
    trim-value))

(defn instruction-sbc
  [cpu-state opcode address]
  (let [value (bit-xor (read-byte (cpu-state :memory-map) address) 0xFF)
        sub (if (zero? (bit-and (get-reg cpu-state :P) sr-flag-decimal))
              instruction-add instruction-sbc-dec)]
    (sub cpu-state value)))

(defn instruction-cmp
  [cpu-state opcode address]
  (let [result (+ 1 (get-reg cpu-state :A)
                  (bit-xor (read-byte (cpu-state :memory-map) address) 0xFF))]
    (sr-update-flag cpu-state sr-flag-carry #(> result 0xFF))
    (bytify result)))

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
  (if (= opcode 0x89) (invalid-opcode-error cpu-state opcode))
  (let [instruction (get-instr opcode)
        func (nth group-ora-instructions instruction)
        addr-mode (nth group-ora-address-modes (get-addr-mode opcode))
        result (func cpu-state opcode (addr-mode cpu-state opcode))]
    (if (not= instruction 0x04) (sr-set-nz cpu-state result))))

; Major instruction group ASL (cc = 10)
(defn group-asl
  [cpu-state opcode])

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

