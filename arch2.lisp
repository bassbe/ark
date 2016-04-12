;;; ARCHITECTURE PROJECT #2
;;; Benjamin Bass
;;; 14 March 2016

;; to run the program, call (prompt-repeat) function in repl.

;;; Create main memmory array 16x16
(defvar main-memory (make-array 2048))
;;; Create cache
(defvar cache (make-array '(16)))

;; When structure object is created, will be initialized to
(defstruct cache-struct
  (slot 0)
  (valid 0)
  (tag 0)
  (dirty 0)
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
  (format t "Slot Valid  Tag  Dirty     Data~%")
  (dotimes (i 16)
    (format t " ~x     ~a     ~x    ~a        ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x  ~x~%"
	  (cache-struct-slot (aref cache i))
	  (cache-struct-valid (aref cache i))
	  (cache-struct-tag (aref cache i))
	  (cache-struct-dirty (aref cache i))
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
	  ((string-equal current-command "W") (write-into-cache))
	  ((string-equal current-command "D") (display-cache))
	  ((string-equal current-command "E") (return))
	  (t "Nothing Evaluated")))))

(defun get-offset (address)
  "extract slot assuming lsb is offset"
  (let ((bitmask #x000F))
    (logand bitmask address)))

(defun get-address-read ()
  (let ((address-value 0))
(format t "What address value would you like?~%")
(setq address-value (read))))

(defun get-value-read ()
  (let ((read-in-value 0))
  (format t "What value would you like?~%")
  (setq read-in-value (read))))

(defun get-address-to-write-to ()
  (let ((address-value 0))
(format t "What address would you like to write to?~%")
(setq address-value (read))))

(defun get-data-to-write ()
  (let ((data 0))
  (format t "What value would you like written to this address?~%")
  (setq data (read))))

(defun get-valid (slot)
  (cache-struct-valid (aref cache slot)))

(defun get-slot (address)
  "Get slot assuming 1 byte offset"
  (let ((bitmask #x0F0)
	(slotnum 0))
    ;; if Ash shift is neg, left, pos, right
    (setq slotnum (ash (logand bitmask address) -4))))

(defun get-tag (address)
  (let ((bitmask #xF00))
    (ash (logand address bitmask) -8)))

(defun get-dirty (slot)
    (cache-struct-dirty (aref cache slot)))

;; Test with #xA #xE #x7AE
(defun hit-or-miss (slot offset data) ;valid for hit/miss, slot for data retrieval, offset for data retrieval
  (let ((valid 0)
	(found-value 0)
	(found-tag 0)
	(data-tag 0)
	(tag-from-data 0)
       	(tag-bitmask #xF00))
    ;; populate vars
    (setq found-tag (cache-struct-tag (aref cache slot)))
    (setq data-tag (get-tag data))
    (setq valid (get-valid slot))
    (setq tag-from-data (ash (logand data tag-bitmask) -8))
    (cond ((and (= valid 1) (= data-tag found-tag))
	   (setq found-value (aref (cache-struct-data (aref cache slot)) offset))
	    (format t "Found value ~x at that address value! Cache Hit!~%" found-value));
	  ((= valid 0)
	   (read-in-slot-data slot data) ; reads memory data into slots
	   (setq found-value (aref (cache-struct-data (aref cache slot)) offset))
	   (setf (cache-struct-valid (aref cache slot)) 1)
	   (format t "Cache Miss! Read in value ~x~%" found-value))
	  ((and (= valid 1) (not (= data-tag found-tag)))
	   (read-in-slot-data slot data)
	   (setq found-value (aref (cache-struct-data (aref cache slot)) offset))
	   (setf (cache-struct-tag (aref cache slot)) data-tag);; set new tag
	   (format t "Cache Miss! Not same tag. Found value ~x at address  ~a.~%"
		   found-value
		   data)))))


(defun write-into-cache ()
  "Read from memory into cache"
  (let ((address 0)
	(data 0)	
	(slot 0)
	(offset 0)
	(found-tag 0))

    (setq address (get-address-read))
    (setq data (get-value-read))
    (setq slot (get-slot address))
    (setq offset (get-offset address))
    (setq found-tag (cache-struct-tag (aref cache slot)))
;;    (setq found-tag (get-tag address))
    
    (hit-or-miss-write address data slot offset found-tag)));; need to check write

(defun hit-or-miss-write (address-to-write-to data-to-write slot offset found-tag ) ;valid for hit/miss, slot for data retrieval, offset for data retrieval
  (let ((valid 0)
	(new-tag 0)
	(dirty 0))
    ;; populate vars
    (setq new-tag (get-tag address-to-write-to))
    
    (setq valid (get-valid slot))
    (setq dirty (get-dirty slot))
    
        ;; if Valid=1, DirtyBit=1, Tag=Matches 
        (cond ((and (= valid 1) (= new-tag found-tag)) ; done
	       ;;print ("Wrote value ~x, to address ~x." value address)
	       (setf (aref (cache-struct-data (aref cache slot)) offset) data-to-write)
	       (format t "The value ~x has been written to the address ~a (Cache Hit!)~%"
		       data-to-write
		       address-to-write-to))
	      
	    ;;if Valid=0 (never been here before)
	      ((= valid 0) ; done

	       (read-in-slot-data slot address-to-write-to); read in data from main memmory
	       (setf (cache-struct-dirty (aref cache slot)) 1);; dirty = 1
	       (setf (cache-struct-tag (aref cache slot)) new-tag);; set tag
	       (setf (cache-struct-valid (aref cache slot)) 1);; valid = 1
	       (format t "Cache Miss! Slot was empty. Value ~x was written to address ~a. Blocks moved in too.~%"
		       data-to-write
		       address-to-write-to))
	      
	      ;; valid = 1, dirty = 1, tag != tag
	  ((and (= valid 1) (= dirty 1) (not(= new-tag found-tag)))
	   ;; write out to main memory

	   (write-to-main slot found-tag)
	   (read-in-slot-data slot address-to-write-to); read in data
	   (setf (cache-struct-tag (aref cache slot)) new-tag); set new tag
	   (format t "Cache Miss! Wrote value ~x to address ~a~%"
		   data-to-write
		   address-to-write-to)))))


(defun write-to-main (slot tag)
  (let ((beginning-address 0)
	(ending-address)
	(tagnumber 0)
	(slotnumber 0))
	  (setq tagnumber (ash tag 8))
	  (setq slotnumber (ash slot 4))
	  (setq beginning-address (+ slotnumber tagnumber))
	  (setq ending-address (+ beginning-address #xF))

	  (loop for i from beginning-address to ending-address for j from 0 to #xF
	       do (setf (aref main-memory i) 
		     (aref (cache-struct-data (aref cache slot)) j)))))


(defun read-into-cache ()
  "Read from memory into cache"
  (let ((slot 0)
	(offset 0)
	(data 0))
    (setq data (get-address-read))
    (setq slot (get-slot data))
    (setq offset (get-offset data))
    (hit-or-miss slot offset data)))

(defun read-in-slot-data (slot data-address)
  (let ((beginning-address 0)
	(start-bitmask #xFF0)
	(ending-address 0))
    ;; set the tag of the row to match the data read in
    (setf (cache-struct-tag (aref cache slot)) (get-tag data-address))
    ;; set the beginning address to use in loop
    (setq beginning-address (logand start-bitmask data-address))
    ;; set ending address to use in loop
    (setq ending-address (+ beginning-address #x00F))
    ;; update valid bit to 1
    (setf (cache-struct-valid (aref cache slot)) 1)
    ;; populate cache row data. Span 0-F
    (loop for i from beginning-address to ending-address for j from 0 to #xF
       do ;;set data slot in cache to memory location. Loop it to all 16
	 (setf (aref (cache-struct-data (aref cache slot)) j)
	       (logand #x0FF (aref main-memory i))))))
