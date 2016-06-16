;; Copyright 2016 IBM

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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


