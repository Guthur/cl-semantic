;;;; skunk-graph.asd

(asdf:defsystem #:cl-semantic
  :description "Describe here"
  :author "Your Name <your.name@example.com>"
  :license "MIT"
  :depends-on (#:alexandria #:drakma #:cl-json #:hunchentoot #:local-time #:lparallel)
  :serial t
  :components ((:module base
                         :pathname "src"
                         :components ((:file "package")
                                      (:file "cl-semantic")))))

