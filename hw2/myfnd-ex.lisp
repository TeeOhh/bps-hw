;; -*- Mode: Lisp; -*-

;;;; Natural deduction examples for Ftre.

;;; Copyright (c) 1993-1996 Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :cl-user)

(defvar *nd-rules* "myfnd")

(defun setup-ftre (title &key (debugging nil)
		      (debugging-contexts nil)
		      (max-depth 5)) 
  (in-ftre (create-ftre title :debugging debugging
			:debugging-contexts debugging-contexts
			:max-depth max-depth))
  (bps-load-file *ftre-path* *nd-rules*))

(defun ex1 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests NI, CE, OI, and contradiction detection.
  (setup-ftre "Ex 1" :debugging debugging
	:debugging-contexts debugging-contexts 
              :max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(premise (implies p q)))
		     (assert! '(premise (not q)))
		     (assert! '(goal (not p)))))))

(defun ex2 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests CI and OE.
  (setup-ftre "Ex 2" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(premise (or (not P) R)))
		     (assert! '(premise (implies R Q)))
		     (assert! '(goal (implies P Q)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More examples

(defun ex3 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests AE and AI.
  (setup-ftre "Ex 3" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(premise (and A B)))
		     (assert! '(premise (and B C)))
		     (assert! '(goal (and A B C)))))))

(defun ex4 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests BI and "Star Trek" problem
  (setup-ftre "Ex 4" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(premise contradiction))
		     (assert! '(goal (iff P Q)))))))

(defun ex5 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))  ;tests indirect proof.
  (setup-ftre "Ex 5" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(premise (implies (not p) q)))
		     (assert! '(premise (not q)))
		     (assert! '(goal p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yet more examples

(defun ex6 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 6" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(premise (iff (and R L (not P)) J)))
		     (assert! '(premise (implies (not A) (not R))))
		     (assert! '(premise (not A)))
		     (assert! '(goal (not J)))))))

(defun ex7 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 7" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre*
                 '((assert! '(premise (implies (and J C) P)))
                   (assert! '(premise (iff (and M C) (or P J))))
                   (assert! '(premise J))
                   (assert! '(goal (and P M)))))))

(defun ex8 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5))     ;; A tough one
  (setup-ftre "Ex 8" :debugging debugging
	:debugging-contexts debugging-contexts
	:max-depth max-depth)
  (time (solved? *ftre*
		   '((assert! '(goal (implies (implies p q)
					      (or (not p) q))))))))

(defun ex9 (&key (debugging nil) (debugging-contexts nil)
		 (max-depth 5)) 
  (setup-ftre "Ex 9" :debugging debugging
	:debugging-contexts debugging-contexts 
	:max-depth max-depth)
  (time (solved? *ftre* '((assert! '(premise (or (and F G) (and G B))))
			    (assert! '(premise (implies F (not G))))
			    (assert! '(goal B))))))


