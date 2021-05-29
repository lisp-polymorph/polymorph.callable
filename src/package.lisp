;;;; package.lisp

(defpackage #:polymorph.callable
  (:use #:cl #:polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:shadow #:funcall #:apply)
  (:export #:funcall #:apply))
