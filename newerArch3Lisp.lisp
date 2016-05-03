;;; Computer Architecture
;;; Project 3
;;; Benjamin Bass

;;; C-c C-q (close parens at point)
;;; C-c C-z (go to repl)
;;==============================================================================
;; Defining Variables

;; Main Memory
(defvar Main_Mem (make-array 1024)); 1k array
;; Array of Integers
(defvar Regs (make-array 32))

;;------------------------------------------------------------------------------
;; DEFINING STRUCTS

;; IF/ID Write
(defstruct if-id-write
  (Inst 0)
  (decoded-instruction 0)
  (IncrPC 0))

;; IF/ID Read
(defstruct if-id-read
  (Inst 0)
  (decoded-instruction 0)
  (IncrPC 0))

;; ID/EX Write
(defstruct id-ex-write
  (RegDst 0)
  (ALUSrc 0)
  (ALUOp 00)
  (MemRead 0)
  (MemWrite 0)
  (Branch 0)
  (MemToReg 0)
  (RegWrite 0))

;; ID/EX Read
(defstruct id-ex-read)


;; EX/MEM Write
(defstruct ex-mem-write)

;; EX/MEM Read
(defstruct ex-mem-read)

;; MEM/WB Write
(defstruct mem-wb-write)

;; MEM/WB Read
(defstruct mem-wb-read)


;;------------------------------------------------------------------------------
;; INITIALIZE MEMORY AND STRUCTS

;; Populate Main Memory
(defun initialize-Main_Mem()
  "Set Main_Mem sots  #x()00 - #x3FF with the 00-FF values"
  (let((arrayMax (- (array-total-size Main_Mem) 1)))
       
    (loop for i from 0 to arrayMax
       do(setf
	  (aref Main_Mem i)
	  (logand #xFF i)))))

;; Populate registers
(defun initialize-Regs()
  "Set regs 0-31 with values #x100 + register number"
  (let ((regcount 31))
    (loop for i from 0 to regcount
       do(setf
	  (aref Regs i)
	  (+ #x100 i)))))

;; Populate Regs

;;------------------------------------------------------------------------------
;; 4 MAIN FUNCTIONS


(defun IF-stage()
  "Instruction Fetch From Memory"
  (format t "This is IF"))

(defun ID-stage()
  "Instruction Decode & Register Read"
  (format t "This lis ID Stage"))

(defun EX-stage()
  "Execute Operand or Calcualte Address"
  (format t "This is the EX stage"))

(defun MEM-stage()
  "Access Memory Operand"
  (format t "This is the MEM stage"))

(defun WB-stage ()
  "Write Results Back to Register"
  (format t "This is the WB stage"))

(defun Print-Out-Everything()
  "Prints out 32 Registers and Pipeline Registers"
  (format t "this prints out everything..."))

(defun main ()
  "Main loop that acts as clock cycle"
 
  )

;; (main) ;this calls the main mehtod
;;------------------------------------------------------------------------------
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
;;------------------------------------------------------------------------------
;; Read in Text
(defun take-in-instructions ()
  "Reads in text from file and prints it out"
  (let ((in (open "/Users/benjaminbass/seacloud/class/arch/ark/readin3.txt"
		  :if-does-not-exist nil)));;open stream

    (when in                                 ;in is false after loop completes
    (loop for line = (read-line in nil)      ;Not understood
       while line do (format t "~a~%" line)) ;while next line exists, print it
    (close in))))                            ;close in to exit while

;;------------------------------------------------------------------------------
;; MAIN METHOD



;; (main)
;;==============================================================================
