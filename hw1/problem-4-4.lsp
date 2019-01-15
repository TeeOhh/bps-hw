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
;for pattern in patterns
;for candidate in candidates
;for each consistent binding set, binding set = (unify pattern candidate binding)
;;;(defun multi-fetch (patterns &optional (tre *tre*) &aux bindings unifiers)
;;;  (do* ((pattern-tracker patterns (cdr pattern-tracker))
;;;       (temp-bindings '(nil) '(nil)))
;;;      ((null pattern-tracker) (make-unifiers patterns bindings))
;;;    (dolist (candidate (get-candidates (car pattern-tracker) tre))
;;;      (do* ((binding-tracker bindings (cdr binding-tracker))
;;;            (temp-binding :fail (unify (car pattern-tracker) candidate (car binding-tracker))))
;;;           ((null binding-tracker) (setq bindings temp-bindings))
;;;        (unless (eq temp-binding :fail)
;;;          (append temp-binding temp-bindings))))
;;;    (print temp-bindings)
;;;    (setq bindings temp-bindings)))

(defun multi-fetch (patterns &optional (tre *tre*) (consis-bindings '(nil)) &aux bindings)
  (do* ((pattern-tracker patterns (cdr pattern-tracker))
       (cur-pattern (car pattern-tracker) (car pattern-tracker))
       (temp-binding '() '()))
      ((null pattern-tracker) (make-unifiers patterns consis-bindings))
    (dolist (candidate (get-candidates cur-pattern tre))
      (dolist (consis-binding consis-bindings)
        (setq bindings (unify cur-pattern candidate consis-binding))
        (unless (eq bindings :fail)
          (setq temp-binding (append (list bindings) temp-binding)))))
    (setq consis-bindings temp-binding)))


(defun make-unifiers (patterns bindings-set &aux unifiers)
  (mapcar #'(lambda (bindings)
              (sublis bindings patterns))
    bindings-set))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
