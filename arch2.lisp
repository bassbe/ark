;;; ARCHITECTURE PROJECT #2
;;; Benjamin Bass
;;; 14 March 2016


;;; Create main memmory array 16x16
(setq main-memory (make-array 2048))

(defun initialize-main-memory ()
  ;; Loop through and populate array
  (dotimes (i (- (array-total-size main-memory) 1)) ; loop through array
    (setf
     (aref main-memory i) ;fetch memory location
     (logand #xFF i) ))) ; bitmask with last letter to extract last hex digit


;;; Create cache.
(setq cache (make-array '(16)))

(defun initialize-cache-array ()
  (dotimes (i (array-total-size cache) 1)
    (setq
     (aref cache i))
    (make-cacheStruct
     :valid 0
     :slot 0
     :tag 0
     :value 0)))




(defstruct cacheStruct valid slot tag value )

;;;Print Array
;;; (dotimes ((i (array-total-size cache-memory) 1)
;;; 	   (format
     






;; (initialize-cache) ; to Zero Everything
;; (display-cache)
;; (write-to-cache)
;; (write-to-memory)
;; (read-from-memory)


;;;; Main Function
;; (initialize-main-memory)
;; (initialize-cache)
;; (prompt-for-instruction)
;;   stop when told




;;TESTER CODE
;; Print main slots and it's contents to check of initialized properly
  ;; (dotimes (i (- (array-total-size main-memory) 1))
  ;;   (format t "Location: ~x has value: ~x~%" i (aref main-memory i)))
