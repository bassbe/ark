;;; ARCHITECTURE PROJECT #2
;;; Benjamin Bass
;;; 14 March 2016


;;; Create main memmory array 16x16
(defvar main-memory (make-array 2048))
;;; Create cache
(defvar cache (make-array '(16)))

;; When structure object is created, will be initialized to
(defstruct cache-struct
  (slot 0)
  (valid 0)
  (tag 0)
  (data  (make-array '(16))))

(defun initialize-main-memory ()
  ;; Loop through and populate array
  (dotimes (i (array-total-size main-memory) ) ; loop through array
    (setf
     (aref main-memory i) ;fetch memory location
     (logand #xFF i) ))) ; bitmask with last letter to extract last hex digit

;; Fill cache with cache-struct objects
(defun initialize-cache-array ()
  (let ((n 0))
    (loop
       (when (>= n 16) (return))
       (setf ;sets variable
	(aref cache n); reference n position in cache
	(make-cache-struct)); make cache structure
       (setf (cache-struct-slot (aref cache n)) n)
       (incf n)))); increment n

(defun display-cache ()
  (format t "Slot Valid  Tag      Data~%")
  (dotimes (i 16)
    (format t " ~x     ~a     ~x        ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x~%"
	  (cache-struct-slot (aref cache i))
	  (cache-struct-valid (aref cache i))
	  (cache-struct-tag (aref cache i))
	  (aref (cache-struct-data (aref cache i)) 0)
	  (aref (cache-struct-data (aref cache i)) 1)
	  (aref (cache-struct-data (aref cache i)) 2)
	  (aref (cache-struct-data (aref cache i)) 3)
	  (aref (cache-struct-data (aref cache i)) 4)
	  (aref (cache-struct-data (aref cache i)) 5)
	  (aref (cache-struct-data (aref cache i)) 6)
	  (aref (cache-struct-data (aref cache i)) 7)
	  (aref (cache-struct-data (aref cache i)) 8)
	  (aref (cache-struct-data (aref cache i)) 9)
	  (aref (cache-struct-data (aref cache i)) 10)
	  (aref (cache-struct-data (aref cache i)) 11)
	  (aref (cache-struct-data (aref cache i)) 12)
	  (aref (cache-struct-data (aref cache i)) 13)
	  (aref (cache-struct-data (aref cache i)) 14)
	  (aref (cache-struct-data (aref cache i)) 15))))

(defun take-command ()
  (let ((command 0))
  (format t "(R)ead, (W)rite, (D)isplay Cache, (E)xit:~%")
  (finish-output nil)
  (setq command (read))))
  
(defun prompt-repeat ()
  (let ((current-command 0))
  (loop
     (setq current-command (take-command))
    (cond ((string-equal current-command "R" ) (read-into-cache))
	  ((string-equal current-command "W") (write-to-memory))
	  ((string-equal current-command "D") (display-cache))
	  ((string-equal current-command "E") (return))
	  (t "Nothing Evaluated")))))

(defun write-to-memory ()
  "Function writes to main memory"
  (format t "Write cache values to main memory.~%"))

(defun read-into-cache ()
  "Read from memoty into cache"
  (let ((address-value 0))
    (setq address-value (get-address))
    (format t "Address Value is: ~a~%" address-value)))

(defun get-address ()
(format t "What address would you like to read in?~%")
  (setq address-value (read)))
