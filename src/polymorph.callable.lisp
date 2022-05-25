;;;; polymorph.callable.lisp

(in-package #:polymorph.callable)

(defun function-return-type (form env)
  "Determine the return type of a function.

FORM is a form. The type of FORM is determined and if it is of a
FUNCTION TYPE, the function's return type is returned, otherwise T is
returned.

ENV is the environment in which FORM is found.

Returns the function return type, if the type of FORM is a function."

  (with-type-info (whole (type &optional params (return-type 't)) env)
      form
    (declare (ignore params))

    (if (eq return-type '*)
        t
        return-type)))

(define-polymorphic-function funcall (function &rest arguments) :overwrite t)


(defpolymorph funcall ((function function) &rest args) t
  (cl:apply function args))


(defpolymorph-compiler-macro funcall (function &rest) (function &rest args
                                                                &environment env)

  (let* ((function (form-expand function))
         (return-type (function-return-type function env)))

    (print `(the ,return-type
                ,(if (typep function '(cons (eql lambda) cons))
                     (cons function args)
                     `(cl:funcall ,function ,@args))))))


(defpolymorph funcall ((function symbol) &rest args) t
  (cl:apply function args))


(defpolymorph-compiler-macro funcall (symbol &rest) (function &rest args
                                                              &environment env)
  (with-type-info (_ (type &optional name) env)
      function

    (or
     (when (and (constantp function env)
                (eql type 'eql))

       ;; For correctness, this requires good CLTL2 support, either
       ;; native or that the code is contained in the
       ;; CL-ENVIRONMENTS-CL code walker package.

       (multiple-value-bind (type lexical-p info)
           (cltl2:function-information name env)

         ;; Check that there isn't a lexical definition for a
         ;; function of the same name.

         (if (and (eq type :function)
                  (not lexical-p))

             ;; Check that the function is declared inline

             `(the ,(function-return-type `(function ,name) env)
                   ,(if (eq (cdr (assoc 'inline info)) 'inline)
                        `(,name ,@args)
                        `(cl:funcall ,function ,@args)))

             ;; Get return type in global environment
             `(the ,(function-return-type `(function ,name) nil)
                   (cl:funcall ,function ,@args)))))

     `(cl:funcall ,function ,@args))))



(define-polymorphic-function apply (function arg &rest arguments) :overwrite t)



(defpolymorph apply ((function function) (arg t) &rest args) t
  (cl:apply #'cl:apply function arg args))


(defpolymorph-compiler-macro apply (function t &rest) (function arg &rest args
                                                                &environment env)

  (let* ((function (form-expand function))
         (return-type (function-return-type function env)))

    `(the ,return-type (cl:apply ,function ,arg ,@args))))


(defpolymorph apply ((function symbol) (arg t) &rest args) t
  (cl:apply #'cl:apply function arg args))


(defpolymorph-compiler-macro apply (symbol t &rest) (function arg &rest args
                                                              &environment env)

  (with-type-info (_ (type &optional name) env)
      function

    (if (and (constantp function env)
             (eql type 'eql))

        `(the ,(function-return-type `(function ,name) nil)
              (cl:apply ,function ,arg ,@args))

        `(cl:apply ,function ,arg ,@args))))
