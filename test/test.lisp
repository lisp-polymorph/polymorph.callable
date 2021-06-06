;;; Unit tests for polymorph.callable

(in-package #:polymorph.callable/test)

;;; Test suite definition

(def-suite polymorph.callable
    :description "Master test suite for polymorph.callable")

(in-suite polymorph.callable)

(defun test-polymorph.callable ()
  (run! 'polymorph.callable))


;;; Definitions

(declaim (inline my-add my-car))

(declaim (ftype (function * number) my-add))

(defun my-add (x &rest xs)
  (reduce #'+ xs :initial-value x))

(defun my-car (cons)
  (car cons))

(defun (setf my-car) (value cons)
  (setf (car cons) value))

;;; Tests

;;;; FUNCALL

(test-optimize funcall-function
  "Test FUNCALL on FUNCTION object"

  (let ((f #'my-car))
    (is (= 6 (funcall #'my-add 1 2 3)))
    (is (eq 'x (funcall f '(x y))))))

(test-optimize funcall-local-function
  "Test FUNCALL on a local FUNCTION"

  (flet ((my-add (x) (1+ x)))
    (let ((f #'my-add))
      (is (= 2 (funcall #'my-add 1)))
      (is (= 5 (funcall f 4))))))

(test-optimize funcall-setf-function
  "Test FUNCALL on (SETF ...) function"

  (let ((cell (list 1 2 3)))
    (is (eq 'a (funcall #'(setf my-car) 'a cell)))
    (is (equal '(a 2 3) cell))))

(test-optimize funcall-lambda
  "Test FUNCALL on LAMBDA function"

  (is (= 110 (funcall #'(lambda (a b) (* a b)) 10 11)))
  (is (= 3 (funcall (lambda (x) (/ x 4)) 12))))

(test-optimize funcall-symbol
  "Test FUNCALL on SYMBOL"

  (declare (inline my-car))

  (let ((my-add 'my-add))
    (flet ((my-car (x)
             (error "Gotcha. Lexical MY-CAR function called instead of global function.")))
      (declare (inline my-car))

      (is (= 15 (funcall 'my-add 4 5 6)))
      (is (= 30 (funcall my-add 10 20)))
      (is (eq 'c (funcall 'my-car '(c d e)))))))

;;;; APPLY

(test-optimize apply-function
  "Test APPLY on FUNCTION object"

  (let ((f #'my-add))
    (is (= 12 (apply #'my-add 1 2 3 '(4 2))))
    (is (= 18 (apply f 1 2 3 4 '(5 3))))))

(test-optimize apply-local-function
  "Test APPLY on a local FUNCTION"

  (flet ((my-add (x) (1+ x)))
    (let ((f #'my-add))
      (is (= 2 (apply #'my-add '(1))))
      (is (= 5 (apply f '(4)))))))

(test-optimize apply-setf-function
  "Test APPLY on (SETF ...) function"

  (let ((cell (list 1 2 3)))
    (is (eq 'a (apply #'(setf my-car) (list 'a cell))))
    (is (equal '(a 2 3) cell))))

(test-optimize apply-lambda
  "Test APPLY on LAMBDA function"

  (is (= 110 (apply #'(lambda (a b) (* a b)) 10 '(11))))
  (is (= 3 (apply #'(lambda (x) (/ x 4)) '(12)))))

(test-optimize apply-symbol
  "Test APPLY on SYMBOL"

  (let ((my-add 'my-add))
    (flet ((my-add (x)
             (error "Gotcha. Lexical MY-ADD function called instead of global function.")))
      (declare (inline my-add))

      (is (= 15 (apply 'my-add 4 5 '(3 3))))
      (is (= 30 (apply my-add 10 15 '(2 3)))))))
