;;;; polymorph.callable.asd

(asdf:defsystem #:polymorph.callable
  :description "Callable interface for polymorph.stl"
  :author "Commander Thrashdin"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:compiler-macro #:trivial-form-ctype #:polymorph.utility)
  :components ((:module
                "src"
                :serial t
                :components
                ((:file "package")
                 (:file "polymorph.callable")))))
