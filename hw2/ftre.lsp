;; -*- Mode: Lisp; -*-

;;;; Fast Tiny Rule Engine.
;;;;  Modified: Ron Ferguson on Mon Jan 12 15:56:19 1998

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :cl-user)

(defvar *ftre-path* nil "Ftre's path")
(defvar *ftre-files* nil "Ftre's files")
(defvar *fqueen-rule-file* nil "Ftre's version of the n-queens rule")

(setq *ftre-path* (make-bps-path "ftre"))

(setq *ftre-files*
      '("finter"    ;; Interface
        "fdata"     ;; Database
        "frules"    ;; Rule system
        "unify"     ;; Unifier
        "funify"    ;; Open-coding for unification
        ;; "fnd-ex"    ;; Natural deduction examples for ftre
        "fqueens"   ;; n-queens setup for Ftre
        ))

(setf *fqueen-rule-file* "fqrule")

(defun load-ftre (&key (action :compile-if-newer))
  (bps-load-files *ftre-path* *ftre-files* :action action))

(defun help-ftre (&optional (stream t))
   "Print helpful information about the Ftre."
   (dolist (line
             '("Ftre Help:"
               "-----------------------------------------"
               "To load Ftre:   (load-ftre)"
               "Shakedown routine:  (shakedown-ftre)"
               ""
               "NQueens: "
               "  (test-queens <from> <to>) ;; e.g., (test-queens 4 5) "
               ""
               "Natural deduction: "
               "  (ex1 :debugging t :debugging-contexts t)"
               "  (ex2 :debugging t :debugging-contexts t)"
               "  (ex3 :debugging t :debugging-contexts t)"
               "  (ex4 :debugging t :debugging-contexts t)"
               ""))
      (terpri stream) (princ line stream))
   (terpri stream))

(defun shakedown-ftre ()
   "Run set of representation exercises on FTRE."
   (format t "~%Beginning shakedown...~%")
   (test-queens 3 5)
   (ex1)
   (ex2)
   (ex3)
   (format t "~%~%  ...shakedown ended.~%~%"))

(help-ftre)