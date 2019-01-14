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
;((human socrates) (implies (human socrates) (mortal socrates)) (mortal socrates))
;((human socrates) (implies (human socrates) (mammal socrates)) (mortal socrates))

(defun multi-fetch (patterns &aux bindings unifiers)
  (dolist (pattern patterns
                   (let ((bindings-temp '()))
                     (dolist (candidate (get-candidates pattern tre) unifiers)
                       (dolist (binding bindings something)
                         ;maybe replace binding in bindings?
                         (setq binding (unify pattern candidate binding))
                         (unless (eq binding :fail)
                           (append binding bindings-temp)))
                         ;at the end of iteration, setq bindings to bindings-temp list
                         (push (sublis bindings-temp pattern) unifiers)))))))

(defun fetch (pattern &optional (tre *tre*) &aux bindings unifiers)
  "Returns the list of facts which unify with the pattern."
  (dolist (candidate (get-candidates pattern tre) unifiers)
    (setq bindings (unify pattern candidate))
    (unless (eq bindings :fail)
      (push (sublis bindings pattern) unifiers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
