;;;; Class: CSC 472: Computer Architecture
;;;; Project 3: Pipelined Datapath

;;;; Simulate how a pipelined datapath works. Uses 5 functions, IF, ID, EX, MEM, WB, and 8 structs 
;;;; that emulate the READ/WRITE versions of Pipeline Registers. The program will loop that calls
;;;; all five functions, prints the appropriate information (the 32 registers and both READ and
;;;; WRITE versions of the pipeline registers) and then copy the WRITE version of the pipeline
;;;; registers into the READ version for use in next cycle. Each cycle simulates one clock cycle.

;;;; Will only be implementing instructions: nop, add, sub, sb, lb. sb/lb = save/loab byte because
;;;; the Main_Mem array stores bytes.
;;==================================================================================================
;; Defining Variables

;; Main Memory
(defvar Main_Mem (make-array 1024)); 1k array

;; Array of Integers
(defvar Regs (make-array 32))

(defvar instruction-cache (make-array '(12) :initial-contents
				      '(#xa1020000 #x810AFFFC #x00831820
					#x01263820 #x01224820 #x81180000
					#x81510010 #x00624022 #x00000000
					#x00000000 #x00000000 #x00000000)))

;Index through Instruction Cache array
(defvar indexer 0)
;; Program Counter
(defvar PC #x7A004)
;; Cycle Number (for the print statements)
(defvar clock-cycle 1)

;;--------------------------------------------------------------------------------------------------
;; DEFINING STRUCTS

;; IF/ID Write
(defstruct if-id-write
  (Inst 0); instruction in hexidecimal
  (IncrPC 0)); program counter

;; IF/ID Read
(defstruct if-id-read
  (Inst 0); instruction in hexidecimal
  (IncrPC 0)); program counter


;; ID/EX Write
(defstruct id-ex-write
  (c-RegDst 0);Control
  (c-ALUSrc 0);Control
  (c-ALUOp 0);Control
  (c-MemRead 0);Control
  (c-MemWrite 0);Control
  (c-MemToReg 0);Control
  (c-RegWrite 0);Control
  
  (IncrPC 0);program counter
  (ReadReg1Value 0);register 1 value
  (ReadReg2Value 0);register 2 value
  (SEOffset 0); Offset
  (WriteReg20_16 0);register
  (WriteReg15_11 0)
  (function 0))

;; ID/EX Read
(defstruct id-ex-read
  (c-RegDst 0);Control
  (c-ALUSrc 0);Control
  (c-ALUOp 0);Control
  (c-MemRead 0);Control
  (c-MemWrite 0);Control
  (c-MemToReg 0);Control
  (c-RegWrite 0);Control
  (c-IncrPC 0);Control
  (c-ReadReg1);Control
  (IncrPC 0);program counter
  (ReadReg1Value 0);register 1 value
  (ReadReg2Value 0);register 2 value
  (SEOffset 0); Offset
  (WriteReg20_16 0);register
  (WriteReg15_11 0)
  (function 0))

;; EX/MEM Write
(defstruct ex-mem-write
  (c-MemRead 0);Control
  (c-MemWrite 0);Control
  (c-MemToReg 0);Control
  (c-RegWrite 0);Control
  (Zero 0)
  (ALUResult 0)
  (SBValue 0)
  (WriteRegNum 0))

;; EX/MEM Read
(defstruct ex-mem-read
  (c-MemRead 0);Control
  (c-MemWrite 0);Control
  (c-MemToReg 0);Control
  (c-RegWrite 0);Control
  (Zero 0)
  (ALUResult 0)
  (SBValue 0)
  (WriteRegNum 0))

;; MEM/WB Write
(defstruct mem-wb-write
  (c-MemToReg 0);Control
  (c-RegWrite 0);Control
  (LBDataValue 0)
  (ALUResult 0)
  (WriteRegNum 0))

;; MEM/WB Read
(defstruct mem-wb-read
  (c-MemToReg 0);Control
  (c-RegWrite 0);Control
  (LBDataValue 0)
  (ALUResult 0)
  (WriteRegNum 0))

;;--------------------------------------------------------------------------------------------------
;; INITIALIZE MEMORY AND STRUCT OBJECTS

(defun initialize-Main_Mem()
  "Set Main_Mem sots  #x()00 - #x3FF with the 00-FF values"
  (let((arrayMax (- (array-total-size Main_Mem) 1)))
    (loop for i from 0 to arrayMax
       do(setf
	  (aref Main_Mem i)
	  (logand #xFF i)))))

(defun initialize-Regs()
  "Set regs 0-31 with values #x100 + register number"
  (let ((regcount 31))
    (loop for i from 0 to regcount
       do(setf
	  (aref Regs i)
	  (+ #x100 i)))))

;; Tester for Main_Mem
(defun Main_Mem-array-display ()
  (loop for i from 0 to 1023
     do(format t "Spot ~x has value ~x~%"
	       i
	       (aref Main_Mem i))))

;; Tester for 
(defun Regs-array-display ()
  (loop for i from 0 to 31
     do(format t "Spot ~a has value ~x~%"
	       i (aref Regs i))))

;; Create Pipeline Registers
(defvar ifWrite (make-if-id-write)); IF/ID Write
(defvar ifRead (make-if-id-read)); IF/ID Read
(defvar idWrite (make-id-ex-write)); ID/EX Write
(defvar idRead (make-id-ex-read)); ID/EX REad
(defvar exWrite (make-ex-mem-write)); EX/MEM Write
(defvar exRead (make-ex-mem-read)); EX/MEM Read
(defvar memWrite (make-mem-wb-write)); MEM/WB Write
(defvar memRead (make-mem-wb-read)); MEM/WB Reado

;;--------------------------------------------------------------------------------------------------
;; DECODER FUNCTIONS
(defun calcOpcode (hexNum)
  "Calcualte opcode code"
  (let ((bitmask-opcode #xFC000000); define opcode bitmask
	(shift-opcode -26); define shiftamt for opcode
	(opcode 0)); define temp var opcode
    (setq opcode (ash (logand hexNum bitmask-opcode) shift-opcode)))); bitmask and shift

(defun calcSrc1reg (hexNum)
  "Calculate src1reg code"
  (let ((bitmask-src1reg #x03E00000)
	(shift-src1reg -21)
	(src1reg 0))
    (setq src1reg (ash (logand hexNum bitmask-src1reg) shift-src1reg))))

(defun calcSrc2reg (hexNum)
  "Calcuate src2reg code"
  (let ((bitmask-src2reg #x001F0000)
	(shift-src2reg -16)
	(src2reg 0))
    (setq src2reg (ash (logand hexNum bitmask-src2reg) shift-src2reg))))

(defun calcDestreg (hexNum)
  "Calcualte destination register"
  (let ((bitmask-destreg #x0000F800)
	(shift-destreg -11)
	(destreg 0))
    (setq destreg (ash (logand hexNum bitmask-destreg) shift-destreg))))

(defun calcFunct (hexNum)
  "Calculate Funct value"
  (let ((bitmask-funct #x0000003F)
	(funct 0)); funct doesn't need shift, already in LSB position
    (setq funct (logand hexNum bitmask-funct))))

(defun calcOffset (hexNum)
  "Calculate the offset value"
  (let ((bitmask-offset #x0000FFFF)
	(initialOffset 0)
	(offset 0))
    (setq initialOffset (logand hexNum bitmask-offset))
    (cond ((<= initialOffset 32); if offset positive and < 32, it's a register and ok
	   (setq offset initialOffset)); set as offset
	  ((>= initialOffset 32); if offset >32, its twos compliment
	   ;; Algirithm used to compute offset from negative value
	   (setq offset (- (logand (1- (ash 1 11)) initialOffset) (logand (ash 1 11) initialOffset)))))))

;;--------------------------------------------------------------------------------------------------
;; THE 4 MAIN FUNCTIONS

(defun IF_stage()
  "Fetch instruction and load to WRITE"
  ;; Fetch the next instruction from Instruction Cache &
  ;; put instruction in WRITE version of IF/ID pipeline register
  ;; (setf ifWrite-Inst (aref instruction-cache indexer)) ;
  (setf (if-id-write-Inst ifWrite) (aref instruction-cache indexer))
  (setf (if-id-write-IncrPC ifWrite) PC)
  (setq indexer (+ indexer 1))); update indexer to next position in cache
  
(defun ID_stage()
  "Read and decode instruction. Fetch registers and write values to WRITE ID/EX"
  (let ((hexInst 0)
	(opcode 0)
	(src1reg 0)
	(src2reg 0)
	(destreg 0)
	(funct 0)
	(offset 0)
	(funct-code-add #b100000)
	(funct-code-sub #b100010)
	(opcode-lb #b100011)
	(opcode-sb #b101011))
    
    ;; Read an instruction from READ version of IF/ID pipeline register
    (setq hexInst (if-id-read-Inst ifRead))

    ;; DECODING STAGE- nested loops are tricky in lisp, so I'm calculating everything to simplify.
    ;; Calculate opcode code
    (setq opcode (calcOpcode hexInst))
    ;; Calculate src1reg code
    (setq src1reg (calcSrc1reg hexInst))
    ;; Calculate src2reg code
    (setq src2reg (calcSrc2reg hexInst))
    ;; Calculate destination register
    (setq destreg (calcDestreg hexInst))
    ;; Calculate Function
    (setq funct (calcFunct hexInst))
    ;; Calculate offset
    (setq offset (calcOffset hexInst))
    ;; For the conditional statement:
    ;; Do the register fetching	
    ;; Write the values to the WRITE version of the ID/EX pipeline register
    ;; if opcode 0, loop through to find right right code
    (cond ((and (eq opcode 0) (eq funct funct-code-add ));if ADD function
	   (setf (id-ex-write-c-RegDst idWrite) 1)
	     (setf (id-ex-write-c-ALUSrc idWrite) 0)
	     (setf (id-ex-write-c-ALUOp idWrite) 10)
	     (setf (id-ex-write-c-MemRead idWrite) 0)
	     (setf (id-ex-write-c-MemWrite idWrite) 0)     ;; set an add's control bits
	     (setf (id-ex-write-c-MemToReg idWrite) 0)
	     (setf (id-ex-write-c-RegWrite idWrite) 0)
	     (setf (id-ex-write-IncrPC idWrite) PC)
	     (setf (id-ex-write-ReadReg1Value idWrite) (aref Regs src1reg)); fetch reg1 and add
	     (setf (id-ex-write-ReadReg2Value idWrite) (aref Regs src2reg)); fetch reg2 and add
	     (setf (id-ex-write-SEOffset idWrite) 0)
	     (setf (id-ex-write-WriteReg20_16 idWrite) src1reg); set value of src1reg
	     (setf (id-ex-write-WriteReg15_11 idWrite) src2reg); set value of src2reg
	     (setf (id-ex-write-function idWrite) 20)); function code for Add; end add setters
	   ;; if opcode is sub
	   ((and (eq opcode 0) (eq funct funct-code-sub));condition for Sub 
	     (setf (id-ex-write-c-RegDst idWrite) 1)
	     (setf (id-ex-write-c-ALUSrc idWrite) 0)
	     (setf (id-ex-write-c-ALUOp idWrite) 10)
	     (setf (id-ex-write-c-MemRead idWrite) 0)
	     (setf (id-ex-write-c-MemWrite idWrite) 0)     ;; set an sub's control bits
	     (setf (id-ex-write-c-MemToReg idWrite) 1)
	     (setf (id-ex-write-c-RegWrite idWrite) 0)
	     (setf (id-ex-write-IncrPC idWrite) PC)
	     (setf (id-ex-write-ReadReg1Value idWrite) (aref Regs src1reg)); fetch reg1 and add
	     (setf (id-ex-write-ReadReg2Value idWrite) (aref Regs src2reg)); fetch reg2 and add
	     (setf (id-ex-write-SEOffset idWrite) 0)
	     (setf (id-ex-write-WriteReg20_16 idWrite) src1reg); set value of src1reg
	     (setf (id-ex-write-WriteReg15_11 idWrite) src2reg); set value of src2reg
	     (setf (id-ex-write-function idWrite) 2))
	   ;; if opcode is lb
	   ((eq opcode opcode-lb); if opcode signifies Load Bit
	     (setf (id-ex-write-c-RegDst idWrite) 0)
	     (setf (id-ex-write-c-ALUSrc idWrite) 1)
	     (setf (id-ex-write-c-ALUOp idWrite) 00)
	     (setf (id-ex-write-c-MemRead idWrite) 1)
	     (setf (id-ex-write-c-MemWrite idWrite) 0)     ;; set to lb's control bits
	     (setf (id-ex-write-c-MemToReg idWrite) 1)
	     (setf (id-ex-write-c-RegWrite idWrite) 1)
	     (setf (id-ex-write-IncrPC idWrite) PC)
	     (setf (id-ex-write-ReadReg1Value idWrite) (aref Regs src1reg)); fetch reg1 and add
	     (setf (id-ex-write-ReadReg2Value idWrite) (aref Regs src2reg)); fetch reg2 and add
	     (setf (id-ex-write-SEOffset idWrite) offset)
	     (setf (id-ex-write-WriteReg20_16 idWrite) src1reg); set value of src1reg
	     (setf (id-ex-write-WriteReg15_11 idWrite) src2reg); set value of src2reg
	    (setf (id-ex-write-function idWrite) 0)); end load bit actions
	   
	   ((eq opcode opcode-sb);; opcode is Save Bit
	    (setf (id-ex-write-c-RegDst idWrite) 0)f
	     (setf (id-ex-write-c-ALUSrc idWrite) 1)
	     (setf (id-ex-write-c-ALUOp idWrite) 00)
	     (setf (id-ex-write-c-MemRead idWrite) 0)
	     (setf (id-ex-write-c-MemWrite idWrite) 1)     ;; set an sb's control bits
	     (setf (id-ex-write-c-MemToReg idWrite) 0)
	     (setf (id-ex-write-c-RegWrite idWrite) 0)
	     (setf (id-ex-write-IncrPC idWrite) PC)
	     (setf (id-ex-write-ReadReg1Value idWrite) (aref Regs src1reg)); fetch reg1 and add
	     (setf (id-ex-write-ReadReg2Value idWrite) (aref Regs src2reg)); fetch reg2 and add
	     (setf (id-ex-write-SEOffset idWrite) offset)
	     (setf (id-ex-write-WriteReg20_16 idWrite) src1reg); set value of src1reg
	     (setf (id-ex-write-WriteReg15_11 idWrite) src2reg); set value of src2reg
	    (setf (id-ex-write-function idWrite) 0))))); end save bit setters

             
(defun EX_stage()
  "Preform requested instruction, "
  ;; Read out of ID/EX pipeline
          ;ex/mem pipeline         id/ex pipeline
  (setf (ex-mem-write-c-MemRead exWrite) (id-ex-read-c-MemRead idRead))
  (setf (ex-mem-write-c-MemWrite exWrite) (id-ex-read-c-MemWrite idRead))
  (setf (ex-mem-write-c-MemToReg exWrite) (id-ex-read-c-MemToReg idRead))
  (setf (ex-mem-write-c-RegWrite exWrite) (id-ex-read-c-RegWrite idRead))

  (let ((this-MemRead 0)
	(this-MemWrite 0)
	(this-MemToReg 0)
	(this-RegWrite 0))

	(setf this-MemRead (ex-mem-write-c-MemRead exWrite))
	(setf this-MemWrite (ex-mem-write-c-MemWrite exWrite))
	(setf this-MemToReg (ex-mem-write-c-MemToReg exWrite))
	(setf this-RegWrite (ex-mem-write-c-RegWrite exWrite))

	(cond ((and ;check if control bits are for add
		(eq this-MemRead 0)
		(eq this-MemWrite 0)
		(eq this-MemToReg 0)
		(eq this-RegWrite 1))
	       (setf (ex-mem-write-Zero exWrite) #xF);;add actions
		(setf (ex-mem-write-ALUResult exWrite);; set ALUResult
		      (+ (id-ex-read-ReadReg1Value idRead) (id-ex-read-ReadReg2Value idRead)))
		(setf (ex-mem-write-SBValue exWrite) (id-ex-read-WriteReg15_11 idRead))
		(setf (ex-mem-write-WriteRegNum exWrite) (id-ex-read-WriteReg20_16 idRead))); end add actions
	      
	       ((and ;check if control bits are for sub
		(eq this-MemRead 0)
		(eq this-MemWrite 0)
		(eq this-MemToReg 0)
		(eq this-RegWrite 1))
		
		(setf (ex-mem-write-Zero exWrite) #xF) ; set Zero to #xF
		 (setf (ex-mem-write-ALUResult);; perform subtraction 
		       (- (id-ex-read-ReadReg1Value idRead) (id-ex-read-ReadReg2Value idRead)))
		 (setf (ex-mem-write-SBValue exWrite) (id-ex-read-ReadReg1Value idRead))
		 (setf (ex-mem-write-WriteRegNum exWrite) 0))

	       ((and ;; check if control bits indicate lw
	        (eq this-MemRead 1)
		(eq this-MemWrite 0)
		(eq this-MemToReg 1)
		(eq this-RegWrite 1)); control bits for lw

		(setf (ex-mem-write-Zero exWrite) #xF)    ; perform lw sets
		 (setf (ex-mem-write-SBValue exWrite) (id-ex-read-WriteReg15_11 idRead)); set to src1reg
		 (setf (ex-mem-write-WriteRegNum exWrite) (id-ex-read-WriteReg20_16 idRead))
		 (setf (ex-mem-write-ALUResult exWrite)
		       (aref Regs ; get reg address to read from.
			     (+ ; add offset and reg num
			      (id-ex-read-SEOffset idRead) (id-ex-read-WriteReg15_11 idRead)))))

	       ((and ; if control bits signify sb
	        (eq this-MemRead 0)
		(eq this-MemWrite 1)
		(eq this-MemToReg 0)
		(eq this-RegWrite O))

		(setf (ex-mem-write-Zero exWrite) #xF); set Zero
		 (setf (ex-mem-write-ALUResult exWrite); Set ALUResult
		       (aref Regs
			     (+
			      (id-ex-read-SEOffset idRead) (id-ex-read-WriteReg15_11 idRead))))
		 ;; set SW value
		 (setf (ex-mem-write-SBValue exWrite) (id-ex-read-WriteReg15_11 idRead))
		 ;; set Write
		 (setf (ex-mem-write-WriteRegNum exWrite) 0)))))

(defun MEM_stage()
  "If lb, index into Main Memmory and get value, otherwise, pass info from READ Ex_Mem to WRITE
   version of Mem_WB"
  ;; Read in info from ex-mem
  (setf (mem-wb-write-c-MemToReg memWrite) (ex-mem-read-c-MemToReg exRead))
  (setf (mem-wb-write-c-RegWrite memWrite) (ex-mem-read-c-RegWrite exRead))
  
  ;; IF lb
  ;; Use address calculated in EX stage to index into Main Memory and get value there.
  (let ((this-MemToReg (mem-wb-write-c-MemToReg memWrite))
	(this-RegWrite (mem-wb-write-c-RegWrite memWrite)))

    (cond ((and
	    (eq this-MemToReg 1); contorl bits for lb
	    (eq this-RegWrite 1));control bits for lb
	   (setf (mem-wb-write-LBDataValue memWrite);; set LBDataValue to former ALU Result
		  (ex-mem-read-ALUResult exRead))
	    (setf (mem-wb-write-WriteRegNum memWrite) (ex-mem-read-WriteRegNum exRead)))))); set Write Reg Num

(defun WB_stage ()
  "Write to registers"
  ;; Read info from READ of MEM_WB
  (let ((this-MemToReg 0);read in control
	(this-RegWrite 0); read in control
	(this-LBDataValue 0);;
	(this-ALUResult 0)
	(this-WriteRegNum 0))

    (setf this-MemToReg (mem-wb-read-c-MemToReg memRead))
    (setf this-RegWrite (mem-wb-read-c-RegWrite memRead))
    (setf this-LBDataValue (mem-wb-read-LBDataValue memRead))
    (setf this-ALUResult (mem-wb-read-ALUResult memRead))
    (setf this-WriteRegNum (mem-wb-read-WriteRegNum memRead))

    (cond ((and;; if LB, then load data to register
	    (eq this-MemToReg 1)
	    (eq this-RegWrite 1))
	   (setf (aref Regs this-WriteRegNum) this-LBDataValue))
	  ((and ;; if Add, then write to register
	    (eq this-MemToReg 0 )
	    (eq this-RegWrite 1))
	   (setf (aref Regs this-WriteRegNum) this-ALUResult))
	  ((and;; if subtract, write to register
	    (eq this-MemToReg 1)
	    (eq this-RegWrite 0)
	   (setf (aref Regs this-WriteRegNum) this-ALUResult))
	  ))))
  


(defun Print_out_everything ()
  "Prints out 32 Registers and Pipeline Registers"
  
  (format t "CLOCK CYCLE: ~a~%~%" indexer)
  (loop for i from 0 to 31
     do(format t "Register ~a contains: ~x~%"
	       i
	       (aref Regs i)))
  (format t "~%")
  
  ;; IF/ID  Write Pipeline
  (format t "[IF/ID Write Pipeline]----------------------------------------
Inst: ~x
IncrPC: ~x~%~%"
	  (if-id-read-Inst ifRead)
	  (if-id-read-IncrPC ifRead))
  
  ;; IF/ID Read Pipeline
  (format t "[IF/ID Pipeline]----------------------------------------
Inst: ~x
IncrPC: ~x~%~%"
	 
	  (if-id-write-Inst ifWrite)
	  (if-id-write-IncrPC ifWrite))

  ;; ID/EX Write Pipeline
  (format t "[ID/EX Write Pipeline]----------------------------------------
RegDst: ~x
ALUSrc: ~x
ALUOp: ~x
MemRead: ~x
MemWrite: ~x
MemToReg: ~x
RegWrite: ~x
IncrPC: ~x
ReadReg1Value: ~x
ReadReg2Value: ~x
SEOffset: ~x
WriteReg20_16: ~x
WriteReg15_11: ~x
Function: ~x~%~%"
	  (id-ex-write-c-RegDst idWrite)
	  (id-ex-write-c-ALUSrc idWrite)
	  (id-ex-write-c-ALUOp idWrite)
	  (id-ex-write-c-MemRead idWrite)
	  (id-ex-write-c-MemWrite idWrite)
	  (id-ex-write-c-MemToReg idWrite)
	  (id-ex-write-c-RegWrite idWrite)

	  (id-ex-write-IncrPC idWrite)
	  (id-ex-write-ReadReg1Value idWrite)
	  (id-ex-write-ReadReg2Value idWrite)
	  (id-ex-write-SEOffset idWrite)
	  (id-ex-write-WriteReg20_16 idWrite)
	  (id-ex-write-WriteReg15_11 idWrite)
	  (id-ex-write-function idWrite))

    ;; ID/EX Read Pipeline
  (format t "[ID/EX Read Pipeline]----------------------------------------
RegDst: ~x
ALUSrc: ~x
ALUOp: ~x
MemRead: ~x
MemWrite: ~x
MemToReg: ~x
RegWrite: ~x~%
IncrPC: ~x
ReadReg1Value: ~x
ReadReg2Value: ~x
SEOffset: ~x
WriteReg20_16: ~x
WriteReg15_11: ~x
Function: ~x~%~%"
	  (id-ex-read-c-RegDst idRead)
	  (id-ex-read-c-ALUSrc idRead)
	  (id-ex-read-c-ALUOp idRead)
	  (id-ex-read-c-MemRead idRead)
	  (id-ex-read-c-MemWrite idRead)
	  (id-ex-read-c-MemToReg idRead)
	  (id-ex-read-c-RegWrite idRead)

	  (id-ex-read-IncrPC idRead)
	  (id-ex-read-ReadReg1Value idRead)
	  (id-ex-read-ReadReg2Value idRead)
	  (id-ex-read-SEOffset idRead)
	  (id-ex-read-WriteReg20_16 idRead)
	  (id-ex-read-WriteReg15_11 idRead)
	  (id-ex-read-function idRead))

  ;; Ex/Mem Write Pipeline
  (format t "[EX/Mem Pipeline]------------------------------------------------
MemRead: ~x
MemWrite: ~x
MemToReg: ~x
RegWrite: ~x
Zero:~x
ALUResult: ~x
SBValue: ~x
WriteRegNum: ~x~%~%"
	  (ex-mem-write-c-MemRead exWrite)
	  (ex-mem-write-c-MemWrite exWrite)
	  (ex-mem-write-c-MemToReg exWrite)
	  (ex-mem-write-c-RegWrite exWrite)

	  (ex-mem-write-Zero exWrite)
	  (ex-mem-write-ALUResult exWrite)
	  (ex-mem-write-SBValue exWrite)
	  (ex-mem-write-WriteRegNum exWrite))

  ;; EX/MEM Read Pipeline
    ;; Ex/Mem Write Pipeline
  (format t "[EX/Mem Read Pipeline]----------------------------------------------
MemRead: ~x
MemWrite: ~x
MemToReg: ~x
RegWrite: ~x
Zero:~x
ALUResult: ~x
SBValue: ~x
WriteRegNum: ~x~%~%"
	  (ex-mem-read-c-MemRead exRead)
	  (ex-mem-read-c-MemWrite exRead)
	  (ex-mem-read-c-MemToReg exRead)
	  (ex-mem-read-c-RegWrite exRead)
	  (ex-mem-read-Zero exRead)
	  (ex-mem-read-ALUResult exRead)
	  (ex-mem-read-SBValue exRead)
	  (ex-mem-read-WriteRegNum exRead))

  ;; Mem/WB Write Pipeline
  (format t "[MEM/WB Write Pipeline]----------------------------------------
MemToReg: ~x
RegWrite: ~x
LBDataValue: ~x
ALUResult: ~x
WriteRegNum: ~x~%~%"
	  (mem-wb-write-c-MemToReg memWrite)
	  (mem-wb-write-c-RegWrite memWrite)
	  (mem-wb-write-LBDataValue memWrite)
	  (mem-wb-write-ALUResult memWrite)
	  (mem-wb-write-WriteRegNum memWrite))

    ;; Mem/WB Read Pipeline
  (format t "[MEM/WB Read Pipeline]----------------------------------------
MemToReg: ~x
RegWrite: ~x
LBDataValue: ~x
ALUResult: ~x
WriteRegNum: ~x~%~%"
	  (mem-wb-read-c-MemToReg memRead)
	  (mem-wb-read-c-RegWrite memRead)
	  (mem-wb-read-LBDataValue memRead)
	  (mem-wb-read-ALUResult memRead)
	  (mem-wb-read-WriteRegNum memRead))

  (format t "================================================================================~%"))



(defun Copy_write_to_read ()
  "Copy WRITE versions of pipeline registers into READ version for next cycle"
  
  ;; Copy IF/ID Write to Read
  (setf (if-id-read-Inst ifRead); set Inst
	(if-id-write-Inst ifWrite))
  (setf (if-id-read-IncrPC ifRead) ; set IncrPC
	(if-id-write-IncrPC ifWrite))
  
  ;; Copy ID/EX Write to Read
  (setf (id-ex-read-c-RegDst idRead); take the read value
	(id-ex-write-c-RegDst idWrite)); set it to the write value
  (setf (id-ex-read-c-ALUSrc idRead)
	(id-ex-write-c-ALUSrc idWrite))
  (setf (id-ex-read-c-ALUOp idRead)
	(id-ex-write-c-ALUOp idWrite))
  (setf (id-ex-read-c-MemRead idRead)
	(id-ex-write-c-MemRead idWrite))
  (setf (id-ex-read-c-MemWrite idRead)
	(id-ex-write-c-MemWrite idWrite))
  (setf (id-ex-read-c-MemToReg idRead)
	(id-ex-write-c-MemToReg idWrite))
  (setf (id-ex-read-c-RegWrite idRead)
	(id-ex-write-c-RegWrite idWrite))
  (setf (id-ex-read-IncrPC idRead)
	(id-ex-write-IncrPC idWrite))
  (setf (id-ex-read-ReadReg1Value idRead)
	(id-ex-write-ReadReg1Value idWrite))
  (setf (id-ex-read-ReadReg2Value idRead)
	(id-ex-write-ReadReg2Value idWrite))
  (setf (id-ex-read-SEOffset idRead)
	(id-ex-write-SEOffset idWrite))
  (setf (id-ex-read-WriteReg20_16 idRead)
	(id-ex-write-WriteReg20_16 idWrite))
  (setf (id-ex-read-WriteReg15_11 idRead)
	(id-ex-write-WriteReg15_11 idWrite))
  (setf (id-ex-read-function idRead)
	(id-ex-write-function idWrite))
  
  ;; Copy EX/MEM Write to Read
  (setf (ex-mem-read-c-MemRead exRead)
	(ex-mem-write-c-MemRead exWrite))
  (setf (ex-mem-read-c-MemWrite exRead)
	(ex-mem-write-c-MemWrite exWrite))
  (setf (ex-mem-read-c-MemToReg exRead)
	(ex-mem-write-c-MemToReg exWrite))
  (setf (ex-mem-read-c-RegWrite exRead)
	(ex-mem-write-c-RegWrite exWrite))
  (setf (ex-mem-read-Zero exRead)
	(ex-mem-write-Zero exWrite))
  (setf (ex-mem-read-ALUResult exRead)
	(ex-mem-write-ALUResult exWrite))
  (setf (ex-mem-read-SBValue exRead)
	(ex-mem-write-SBValue exWrite))
  (setf (ex-mem-read-WriteRegNum exRead)
	(ex-mem-write-WriteRegNum exWrite))
  
  ;; Copy MEM/WB Write to Read
  (setf (mem-wb-read-c-MemToReg memRead)
	(mem-wb-write-c-MemToReg memWrite))
  (setf (mem-wb-read-c-RegWrite memRead)
	(mem-wb-write-c-RegWrite memWrite))
  (setf (mem-wb-read-LBDataValue memRead)
	(mem-wb-write-LBDataValue memWrite))
  (setf (mem-wb-read-ALUResult memRead)
	(mem-wb-write-ALUResult memWrite))
  (setf (mem-wb-read-WriteRegNum memRead)
	(mem-wb-write-WriteRegNum memWrite)))

;;--------------------------------------------------------------------------------------------------
(defun main-loop ()
  "Main loop that acts as clock cycle"

  (setq indexer 0)
  (setq PC #x7A004)
  (initialize-regs)
  (initialize-main_mem)
  (format t "================================================================================~%")
  ;; Loop
  (loop for i from 0 to 11
    do (IF_stage)
       (ID_stage)
       (EX_stage)
       (MEM_stage)
       (WB_stage)
       (Print_out_everything)
       (Copy_write_to_read)
     ;; add function to incremenet program counter
       (incf PC 4)))
  

;; (main) ;this calls the main mehtod
;;--------------------------------------------------------------------------------------------------
;; DEBUGGING FUNCTIONS

(defun print-Main_Mem()
  "For debugging and testing purposes"
  (let((arrayMax (- (array-total-size Main_Mem) 1)))
       
    (loop for i from 0 to arrayMax
       do(format t "Value ~x is in slot ~x~%"
		 (aref Main_Mem i)
		 i))))

(defun print-regs()
  "For debugging and testing purposes"
  (let((arrayMax (- (array-total-size Regs) 1)))
       
    (loop for i from 0 to arrayMax
       do(format t "Value ~x is in slot ~x~%"
		 (aref Regs i)
		 i))))
;;--------------------------------------------------------------------------------------------------
;;==================================================================================================
