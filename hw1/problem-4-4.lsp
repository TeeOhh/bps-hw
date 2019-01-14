;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: problem-4-4.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 13, 2019 12:52:34
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-fetch (patterns &optional unifiers)
  ;based on last bindings, (sublis bindings (car pattern))
  ;unify on pattern replaced by sublis above
  ;ex: bindings: (x . Turing) (unify (human Turing) some_assertion)
  (dolist (candidate (get-candidates pattern tre) unifiers)
    (setq bindings (unify pattern candidate bindings))
    (unless (eq bindings :fail)
      (multi-fetch (cdr patterns) (sublis bindings pattern)))))


(defun fetch (pattern &optional (tre *tre*) &aux bindings unifiers)
  "Returns the list of facts which unify with the pattern."
  (dolist (candidate (get-candidates pattern tre) unifiers)
    (setq bindings (unify pattern candidate))
    (unless (eq bindings :fail)
      (push (sublis bindings pattern) unifiers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
