(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)

(defpackage :string-rewriting
  (:use :cl :cl-ppcre :split-sequence))

(in-package :string-rewriting)

(defparameter *special-chars* "([\\.\\?\\]\\[\\)\\(])")

(defparameter *debug-rules* nil)
(defparameter *debug-compilation* nil)

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

(defun compile-rules (rules)
  (mapcar #'compile-rule rules))

(defun apply-rule (rule line)
  (if rule
      (multiple-value-bind (result matchp) 
	  (regex-replace-all (first rule) line (second rule))
	(when (and *debug-rules* matchp)
	  (format *error-output* "[~a] => [~a] (~a)~%:~a:~%:~a:~%~%##~%" (fourth rule) (fifth rule) (third rule) line result))
	(trim result))
      line))

(defun apply-rules (rules line)
  (let ((result line))
    (dolist (rule rules)
      (setf result (apply-rule rule result)))
    result))

(defun process-file (rules filename-in filename-out)
  (with-open-file (in filename-in :direction :input)
    (with-open-file (out filename-out :direction :output :if-exists :supersede)
      (do ((line (read-line in nil)
                 (read-line in nil)))
          ((null line))
        (write-line (apply-rules rules (trim line)) out)))))

;; (defmacro define-rule (&rest args)
;;   (let* ((pos (position '=> args))
;;          (lhs (subseq args 0 pos))
;;          (rhs (subseq args (1+ pos))))
;;     `(push (cons (quote ,lhs) (quote ,rhs)) *grammar*)))

