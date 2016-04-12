
(in-package :string-rewriting)

(defparameter *special-chars* "([\\.\\?\\]\\[\\)\\(])")
(defparameter *debug-rules* nil)
(defparameter *debug-compilation* nil)

;; (defmacro with-open-files-1 (args &rest body)
;;   (let ((res `(progn ,@body)))
;;     (reduce (lambda (acc e) `(with-open-file (,@e) ,acc)) args
;; 	    :initial-value res)))

;; (defmacro with-open-files-2 (args &rest body)
;;   (let ((res `(progn ,@body)))
;;     (dolist (a (reverse args) res)
;;       (setf res `(with-open-file (,@a)
;; 		   ,res)))))


(defmacro with-open-files (args &body body)
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
	  (with-open-files
	      ,(rest args) ,@body)))))


(defun trim (s)
  (string-trim '(#\Space #\Return #\Newline #\Tab) s))

(defun escape-special-chars (string)
  (regex-replace-all *special-chars* string '("\\" :match)))

(defun parse-side (string)
  (mapcar (lambda (x) (split-sequence #\_ x :remove-empty-subseqs t)) 
          (split-sequence #\Space string :remove-empty-subseqs t)))

(defun compile-lhs-pattern (string)
  (if (search "*" string)
    (regex-replace-all "\\*" string "(\\S+)")
    (format nil "(~a)" string)))

(defun compile-lhs-type (string)
  string)

(defun compile-rhs-pattern (string count)
  (regex-replace-all "\\*" string (format nil "\\\\~a" count)))

(defun compile-rhs-type (string)
  string)

(defun compile-lhs (string)
  (flet ((process (s) 
           (format nil "~a_~a" (compile-lhs-pattern (first s)) 
                   (compile-lhs-type (second s)))))
    (format nil "(?:\\s+|^)~{~a~^ ~}(?=\\s+|$)" (mapcar #'process (parse-side (escape-special-chars string))))))

(defun compile-rhs (string)
  (let ((count 0))
    (flet ((process (s) 
             (incf count)
             (format nil "~a_~a" (compile-rhs-pattern (first s) count) 
		      (compile-rhs-type (second s)))))
      (format nil " ~{~a~^ ~}" (mapcar #'process (parse-side string))))))

(defun valid-length (rule)
  (or  (= 2 (length rule)) (= 3 (length rule))))

(defun valid-rule (rule)
  (labels ((valid-token (token)
             (= 2 (length token)))
           (valid-side (side)
             (every #'identity (mapcar #'valid-token (parse-side side)))))
    (when (valid-length rule)
      (and (valid-side (car rule)) (valid-side (cadr rule))))))

(defun valid-regex-rule (rule) (valid-length rule))

(defun compile-rule (rule)
  (if (valid-rule rule)
      (let ((compiled-lhs (compile-lhs (first rule)))
	    (compiled-rhs (compile-rhs (second rule))))
        (when *debug-compilation*
          (format *debug-io* "compiled-lhs = ~a~%compiled-rhs = ~a~%" compiled-lhs compiled-rhs))
	(list (create-scanner compiled-lhs)
	      compiled-rhs (third rule) compiled-lhs compiled-rhs))
      nil))

(defun compile-regex-rule (rule)
  (if (valid-regex-rule rule)
      (let ((compiled-lhs (first rule))
	    (compiled-rhs (second rule)))
	(list (create-scanner compiled-lhs)
	      compiled-rhs (third rule) compiled-lhs compiled-rhs))
      nil))


(defun apply-rule (rule line)
  (if rule
      (multiple-value-bind (result matchp) 
	  (regex-replace-all (first rule) line (second rule))
	(when (and *debug-rules* matchp)
	  (format *error-output* "[~a] => [~a] (~a)~%:~a:~%:~a:~%~%##~%"
		  (fourth rule) (fifth rule) (third rule) line result))
	(values (trim result) matchp))
      (values line nil)))

(defun apply-rules (rules line)
  (let ((result line)
	(applied nil))
    (loop for rule in rules
	  for x from 0 
	  do (multiple-value-bind (res matchp)
		 (apply-rule rule result)
	       (setf result res)
	       (if matchp (push x applied))))
    (values result applied)))

(defun process-file (rules filename-in filename-out &optional (filename-log "out.log"))
  (with-open-files ((in  filename-in  :direction :input)
		    (log filename-log :direction :output :if-exists :append :if-does-not-exist :create)
		    (out filename-out :direction :output :if-exists :supersede))
    (do ((line (read-line in nil)
	       (read-line in nil))
	 (linen 0 (+ 1 linen)))
	((null line))
      (multiple-value-bind (output applied)
	  (apply-rules rules (trim line))
	(format log "~a ~a ~{~a ~}~%" filename-in linen applied)
	(write-line output out)))))


(defun read-rules (filename)
  (with-open-file (stream filename)
    (loop for i = (read stream nil) while i collect i)))


(defun compile-rules (rules)
  (let (grammar)
    (dolist (r rules (reverse grammar))
      (case (car r)
	((->)  (push (compile-rule (cdr r)) grammar))
	((r->) (push (compile-regex-rule (cdr r)) grammar))))))


(defun save-grammar-doc (grammar filename)
  (let* ((len (length grammar))
	 (agrammar (make-array len :initial-contents grammar)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (dotimes (r len)
	(let ((rule (aref agrammar r)))
	  (format out "[~a] ~a => ~a~%" r (cadr rule) (caddr rule)))))))


(defun tabulate-log (in-name out-name)
  (with-open-files ((out out-name :direction :output :if-exists :supersede)
		    (in in-name))
    (do ((line (read-line in nil nil)
	       (read-line in nil nil)))
	((null line))
      (let ((data (split-sequence #\Space line)))
	(mapcar (lambda (id) (format out "~a ~a ~a~%" (car data) (cadr data) id))
		(cddr data))))))



