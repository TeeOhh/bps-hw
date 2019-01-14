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
(defun multi-fetch (patterns)
  ;need to pass bindings to each successive call
  ;fetch doesn't return bindings
  ;dolist over patterns calling (fetch pattern bindings)
  (mapcan #'(lambda (pattern) (fetch pattern)) patterns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
