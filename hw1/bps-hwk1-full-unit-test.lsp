;;;; Homework 1 Unit test
;;;; Jon Wetzel
;;;; 1/10/2013
(in-package :cl-user)

;; Instructions: 
;; 0) Write your assignment code in two files named "problem-4-4.lsp" and 
;;    "problem-4-6.lsp" (if your platform uses a different file extension for
;;    lisp, use that instead)  This file tests both of these problems.
;; 1) load the tre code
;; 2) load your code (if it wasn't included in step 1)
;; 3) load lisp-unit.lsp
;; 4) call (use-package :lisp-unit)
;; 5) load this file.  loading this file will also run all the tests
;;    after the first load, you may call (run-tests) to rerun the tests


(lisp-unit::remove-all-tests :cl-user) ;; cleanup old tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code used by tests

(setq *hw1-solution-file* "hw1-solution") ;; Put your solution file name here

(setq *tre-path* "d:\\bps-qrg\\bps\\answers\\hw1\\") ;; Put your path to solution in here

(defun setup-test-tre-1 ()
  (in-tre (create-tre "Ex1" :debugging nil)) 
  (run-forms *tre*
             '(
               ;; A simple version of Modus Ponens
               (rule (implies ?ante ?conse)
	    (rule ?ante
           (assert! ?conse)))
               ;; A simple version of negation elimination
               (rule (not (not ?x)) (assert! ?x))
               (assert! '(implies (human Turing) (mortal Turing)))
               (assert! '(not (not (human Turing)))))))

(defun setup-test-tre-2 ()
  (in-tre (create-tre "Ex1 + Ken" :debugging nil)) 
  (run-forms *tre*
             '(;; A simple version of Modus Ponens
               (rule (implies ?ante ?conse)
                     (rule ?ante
                           (assert! ?conse)))
               ;; A simple version of negation elimination
               (rule (not (not ?x)) (assert! ?x))
               (assert! '(implies (human Turing) (mortal Turing)))
               (assert! '(not (not (human Turing))))
               (assert! '(implies (human Ken) (mortal Ken)))
               (assert! '(not (not (human Ken))))
               (assert! '(godot Turing)))))

(defun setup-test-tre-3 ()
  (in-tre (create-tre "and test" :debugging nil)) 
  (bps-load-file *tre-path* *hw1-solution-file*)  ;;load rules
  (run-forms *tre*
             '((assert! '(blue Sky))
               (assert! '(cloudy Sky))
               (assert! '(show (and (blue Sky) (cloudy Sky))))))
  )

(defun setup-test-tre-4 ()
  (in-tre (create-tre "iff test" :debugging nil)) 
  (bps-load-file *tre-path* *hw1-solution-file*)  ;; load rules
  (run-forms *tre*
             '((assert! '(implies (foo ?x) (bar ?x)))
               (assert! '(implies (bar ?x) (foo ?x)))
               (assert! '(show (iff (foo ?x) (bar ?x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit Tests

(lisp-unit::define-test "multi-fetch tests"
    (setup-test-tre-1)
  (lisp-unit::assert-equality #'lisp-unit::set-equal
                   '(((human Turing)))
                   (multi-fetch '((human ?x))))

    (setup-test-tre-1)
  (lisp-unit::assert-equality #'lisp-unit::set-equal
                   '(((human Turing) (implies (human Turing) (mortal Turing))))
                   (multi-fetch '((human ?x) (implies (human ?x) ?y))))

    (setup-test-tre-1)
  (lisp-unit::assert-equality #'lisp-unit::set-equal
                   nil
                   (multi-fetch '((human ?x) (implies (human ?x) ?y) (godot ?x))))
    (setup-test-tre-2)
  (lisp-unit::assert-equality #'lisp-unit::set-equal
                   '(((human Turing) (implies (human Turing) (mortal Turing)) (godot Turing)))
                   (multi-fetch '((human ?x) (implies (human ?x) ?y) (godot ?x)))))

(lisp-unit::define-test "and introduction test"
    (setup-test-tre-3)
  (lisp-unit::assert-equal '((and (blue Sky) (cloudy Sky)))
                (fetch '(and (blue Sky) (cloudy Sky)))))

(lisp-unit::define-test "biconditional introduction test"
    (setup-test-tre-4)
  (lisp-unit::assert-equal '((iff (foo x) (bar x)))
                (fetch '(iff (foo x) (bar x)))))

(lisp-unit::run-tests)
