
(in-package :string-rewriting)


(defun write-conllu-lines (sent out)
  (let* ((counter 0)
	 (lines (mapcar (lambda (tk) (list (format "~a" (incf counter))
					   (car tk) "_" (cadr tk) "_" "_" "0" "_" "_" "_"))
			sent)))
    (dolist (l lines)
      (write-line (reduce (lambda (as tk) (format nil "~a~a~a" as #\Tab tk)) l) out))
    (write-line "" out)))


(defun convert-conllu (input output)
  (with-open-files ((out output :direction :output :if-exists :supersede)
		    (in input))
    (with-open-file (in filename)
      (do ((line (read-line in nil nil)
		 (read-line in nil nil)))
	  ((null line)
	   (reverse res))
	(write-conllu-lines (mapcar (lambda (tk) (cl-ppcre:split "_" tk))
				    (cl-ppcre:split "[ ]+" line))
			    out)))))


