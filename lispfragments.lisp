;; (defun write-to-cache ()
;;   "Function writes to main cache"
;;   (let ((data-to-write 0)
;; 	(address-to-write-to 0)
;; 	(offset 0)
;; 	(slot 0)
;; 	(data 0))
;;     (setq address-to-write-to (get-address-to-write-to))
;;     (setq data-to-write (get-data-to-write))
;;     (setq slot (get-slot data))
;;     (setq offset (get-offset data))

    ;; (cond ((and (= valid 1) (= data-tag found-tag))
    ;; 	   (setq found-value (aref (cache-struct-data (aref cache slot)) offset))
    ;; 	    (format t "Wrote value ~x at that address value! Cache Hit!~%" found-value));
    ;; 	  ((= valid 0)
    ;; 	   (read-in-slot-data slot data) ; reads memory data into slots
    ;; 	   (setq found-value (aref (cache-struct-data (aref cache slot)) offset))
    ;; 	   (setf (cache-struct-valid (aref cache slot)) 1)
    ;; 	   (format t "Cache Miss! Read in value ~x~%" found-value)))))
