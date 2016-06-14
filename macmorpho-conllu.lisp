
(in-package :cl-tag-rewriting)


(defun write-conllu-lines (sent out)
  (let* ((counter 0)
	 (lines (mapcar (lambda (tk) (list (write-to-string (incf counter))
					   (car tk) "_" (cadr tk) "_" "_" "0" "_" "_" "_"))
			sent)))
    (dolist (l lines)
      (write-line (reduce (lambda (as tk) (format nil "~a~a~a" as #\Tab tk)) l) out))
    (write-line "" out)))


(defun convert-conllu (input output)
  (with-open-files ((out output :direction :output :if-exists :supersede)
		    (in input))
    (do ((line (read-line in nil nil)
	       (read-line in nil nil)))
	((null line))
      (write-conllu-lines (mapcar (lambda (tk) (cl-ppcre:split "_" tk))
				  (cl-ppcre:split "[ ]+" line))
			  out))))


