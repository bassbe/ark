;;;; Decoder for Project 3

;; OPCODES
(defconstant +opcode-lw+ #b100011)
(defconstant +opcode-sw+ #b101011)

;; FUNCTION CODES
(defconstant +function-code-add+ #b100000)
(defconstant +function-code-sub+ #b100010)

;; BITMASKS
(defconstant +bitmask-opcode+ #xFC000000)
(defconstant +bitmask-srcreg-1+ #x03E00000)
(defconstant +bitmask-srcreg-2+ #x001F0000 )
(defconstant +bitmask-destination-reg+ #x0000F800 )
(defconstant +bitmask-function+ #x0000003F )
(defconstant +bitmask-offset+ #x0000FFFF)

;; SHIFTS
;; Since function is already in least sig. bit position. No shift needed
(defconstant +shift-opcode+ -26)
(defconstant +shift-srcreg1+ -21)
(defconstant +shift-srcreg2+ -16)
(defconstant +shift-destination-reg+ -11)

;; ;; Use for opcode, s1/2reg's
;; (defun extract-bits (hex-value bitmask shift)
;;   "Bitwise &'s the hex-value and bitmask, then shifts right"
;;   (ash (logand hex-value bitmask) shift))


;; Variables to be returned/identified
;; Not all will be used each time.
(defvar opcode 0)
(defvar s1reg 0)
(defvar s2reg 0)
(defvar offset 0)
(defvar destreg 0)
(defvar funct 0)

(defun deMips (hexNum)
  "Dissassemble mips instruction"
  ;; extract opcode
  (setq opcode (ash (logand hexNum +bitmask-opcode+) +shift-opcode+))
  ;; extract src 1 register
  (setq s1reg (ash (logand hexNum +bitmask-srcreg-1+) +shift-srcreg1+))
  ;; extract src 2 register
  (setq s2reg (ash (logand hexNum +bitmask-srcreg-2+) +shift-srcreg2+))
  ;; determine destreg (if needed)
  (setq destreg (ash (logand hexNum +bitmask-destination-reg+) +shift-destination-reg+))
  ;; determine funct (if needed)
  (setq funct (logand hexNum +bitmask-function+))
  ;; Determine offset
  (setq offset (logand hexNum +bitmask-offset+))
  ;; conditional statement to detemrine function code
  (cond ((and (eq opcode 0) (eq funct +function-code-add+))
	 (setq funct "add"))
	 
	((and (eq opcode 0) (eq funct +function-code-sub+))
	 (setq funct "sub"))

	((eq opcode +opcode-lw+)
	(setq opcode "lb"))

	((eq opcode +opcode-sw+)
	(setq opcode "sb"))

	((and (eq opcode 0)
	     (eq s1reg 0)
	     (eq s2reg 0)
	     (eq funct 0))
	 (setq opcode "nop")))
  
  (format t "Opcode: ~a~% Src1 ~a~% Src2 ~a~% Destreg is ~a~% Function is ~a~% Offset is ~a~%"
  	  opcode s1reg s2reg destreg funct offset)
  )
