
(asdf:defsystem #:cl-pcp
  :serial t
  :version "0.0.2"
  :description "Common Lisp corpus tags rewriting system"
  :author "Fabricio Chalub <fchalub@br.ibm.com> and Alexandre Rademaker <alexrad@br.ibm.com>"
  :license "CC BY 4.0"
  :depends-on (#:cl-ppcre #:split-sequence)
  :components ((:file "package")
	       (:file "rewriting"        :depends-on ("package"))
	       (:file "macmorpho-conllu" :depends-on ("package"))))
