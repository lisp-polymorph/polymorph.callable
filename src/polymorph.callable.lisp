;;;; polymorph.callable.lisp

(in-package #:polymorph.callable)

(defun function-return-type (form env)
  "Determine the return type of a function.

FORM is a form. The type of FORM is determined and if it is of a
FUNCTION TYPE, the function's return type is returned, otherwise T is
returned.

ENV is the environment in which FORM is found.

Returns the function return type, if the type of FORM is a function."

  (with-type-info (whole (type params) env)
      form

    (if (and (eq type 'function)
             (length= params 2))

        (let ((type (second params)))
          (if (eq type '*) t type))

        t)))

(define-polymorphic-function funcall (function &rest arguments) :overwrite t)


(defpolymorph funcall ((function function) &rest args) t
  (cl:apply function args))


(defpolymorph-compiler-macro funcall (function &rest) (function &rest args
                                                                &environment env)

  (let* ((function (form-expand function))
         (return-type (function-return-type function env)))

    `(the ,return-type
          ,(if (typep function '(or symbol (cons (eql lambda) cons)))
               (cons function args)
               `(cl:funcall ,function ,@args)))))


(defpolymorph funcall ((function symbol) &rest args) t
  (cl:apply (fdefinition function) args))


(defpolymorph-compiler-macro funcall (symbol &rest) (function &rest args
                                                              &environment env)
  (with-type-info (whole (type type-params) env)
      function

    (or
     (when (and (eql type 'eql)
                (length= type-params 1)
                (symbolp (first type-params)))

       (let* ((name (first type-params))
              (lexical-p (nth-value 1 (cltl2:function-information name env))))

         ;; Check that there isn't a lexical definition for a function
         ;; of the same name.
         ;;
         ;; For correctness, this requires good CLTL2 support, either
         ;; native or that the code is contained in the
         ;; CL-ENVIRONMENTS-CL code walker package.

         (unless lexical-p
           `(the ,(function-return-type `(function ,name) env)
                 (,name ,@args)))))

     `(cl:funcall ,function ,@args))))



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
