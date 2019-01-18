;; -*- Mode: Lisp; -*-

;;;; Variables and unification
;;;;  Modified: forbus on Tue Apr 2 10:20:10 1996

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :cl-user)

(defun variable? (x)
  (and (symbolp x)
       (char= #\? (elt (symbol-name x) 0))))

(defun unify (a b &optional (bindings nil))
   (cond ((equal a b) bindings)
	 ((variable? a) (unify-variable a b bindings))
	 ((variable? b) (unify-variable b a bindings))
	 ((or (not (listp a)) (not (listp b))) :fail)
	 ((not (eq :fail
		   (setq bindings
			 (unify (car a) (car b) bindings))))
	  (unify (cdr a) (cdr b) bindings))
	 (t :fail)))

(defun unify-variable (var exp bindings &aux val)
  ;; Must distinguish no value from value of nil
  (setq val (assoc var bindings))
  (cond (val (unify (cdr val) exp bindings))
	;; If safe, bind <var> to <exp>
	((free-in? var exp bindings)
	 (cons (cons var exp) bindings))
	(t :fail)))

(defun free-in? (var exp bindings)
  ;; Returns nil if <var> occurs in <exp>,
  ;;    assuming <bindings>.
  (cond ((null exp) t)
	((equal var exp) nil)
	((variable? exp)
	 (let ((val (assoc exp bindings)))
	   (if val 
	       (free-in? var (cdr val) bindings)
	     t)))
	((not (listp exp)) t)
	((free-in? var (car exp) bindings)
	 (free-in? var (cdr exp) bindings))))