;;;Read input and print it out from file.
;;; Benjamin Bass

(defun read-how-we-do ()
  "Reads in text from file and prints it out"
  (let ((in (open "/Users/benjaminbass/seacloud/class/arch/ark/readin.txt"
		  :if-does-not-exist nil)))

    (when in
    (loop for line = (read-line in nil)
       while line do (format t "Go like: ~a~%" line))
    (close in))))



;; Changed from code from PLC
;; 
;; (let ((in (open "/some/file/name.txt" :if-does-not-exist nil)))
;;   (when in
;;     (loop for line = (read-line in nil)
;;          while line do (format t "~a~%" line))
;;     (close in)))
