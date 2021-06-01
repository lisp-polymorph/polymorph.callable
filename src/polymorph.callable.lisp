;;;; polymorph.callable.lisp

(in-package #:polymorph.callable)



(define-polymorphic-function funcall (function &rest arguments) :overwrite t)


(defpolymorph funcall ((function function) &rest args) t
  (cl:apply function args))


(defpolymorph-compiler-macro funcall (function &rest) (function &rest args
                                                                &environment env)

  (let* ((function (form-expand function))
         (fun-type (%form-type function env))
         (return-type (if (listp fun-type)
                          (third fun-type)
                          t)))
    (if (listp function)
        (cond ((eql (car function) 'function)
               `(the ,return-type (,(cadr function) ,@args)))
              ((eql (car function) 'lambda)
               `(the ,return-type (,function ,@args)))
              (t `(the ,return-type (funcall ,function ,@args))))
        `(the ,return-type (funcall ,function ,@args)))))


(defpolymorph funcall ((function symbol) &rest args) t
  (cl:apply (fdefinition function) args))


(defpolymorph-compiler-macro funcall (symbol &rest) (function &rest args
                                                              &environment env)
  (if (and (constantp function env) (listp function))
      (let* ((fun-type (%form-type `(function ,(second function)) env))
             (return-type (if (listp fun-type)
                              (third fun-type)
                              t)))
        `(the ,return-type (,(second function) ,@args)))
      `(funcall ,function ,@args)))



(define-polymorphic-function apply (function arg &rest arguments) :overwrite t)



(defpolymorph apply ((function function) (arg t) &rest args) t
  (cl:apply #'cl:apply function arg args))


(defpolymorph-compiler-macro apply (function t &rest) (function arg &rest args
                                                                &environment env)
  (let* ((fun-type (%form-type function env))
         (return-type (if (listp fun-type)
                          (third fun-type)
                          t)))
    `(the ,return-type (cl:apply ,function ,arg ,@args))))


(defpolymorph apply ((function symbol) (arg t) &rest args) t
  (cl:apply #'cl:apply function arg args))


(defpolymorph-compiler-macro apply (symbol t &rest) (function arg &rest args
                                                              &environment env)
  (if (and (constantp function env) (listp function))
      (let* ((fun-type (%form-type `(function ,(second function)) env))
             (return-type (if (listp fun-type)
                              (third fun-type)
                              t)))
        `(the ,return-type (cl:apply ,function ,arg ,@args)))
      `(cl:apply ,function ,arg ,@args)))
