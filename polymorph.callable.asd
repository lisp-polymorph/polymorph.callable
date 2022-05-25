;;;; polymorph.callable.asd

(asdf:defsystem #:polymorph.callable
  :description "Callable interface for polymorph.stl"
  :author "Commander Thrashdin"
  :license  "MIT"
  :version "0.2"
  :serial t
  :depends-on (#:introspect-ctype
               #:polymorphic-functions)
  :components ((:module
                "src"
                :serial t
                :components
                ((:file "package")
                 (:file "polymorph.callable"))))

  :in-order-to ((asdf:test-op (asdf:test-op :polymorph.callable/test))))

(asdf:defsystem #:polymorph.callable/test
  :description "Unit tests for polymorph.callable"
  :license "MIT"
  :serial t
  :depends-on (#:polymorph.callable #:fiveam)
  :components ((:module
                "test"
                :serial t
                :components
                ((:file "util")
		 (:file "test"))))

  :perform (test-op (o s)
             (uiop:symbol-call '#:polymorph.callable/test '#:test-polymorph.callable)))
