;;;; package.lisp

(defpackage #:polymorph.callable
  (:use #:cl
        #:polymorphic-functions
        #:alexandria
        #:introspect-ctype
        #:polymorph.utility)

  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop)
                    (:cltl2 :cl-environments.cltl2))
  (:shadow #:funcall #:apply)
  (:export #:funcall #:apply))
