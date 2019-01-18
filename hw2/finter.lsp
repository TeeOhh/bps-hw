;; -*- Mode: Lisp; -*- 

;;;; Interface and toplevel definitions for the Fast Tiny Rule Engine 
;;;;  Modified: Wednesday, January 26, 2000 at 10:33:45 by ferguson

;;; Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :cl-user)

(defstruct (ftre (:print-function ftre-printer))
  title
  (dbclass-table nil)      ;; Hash table of class-indexed dbclasses.
  (debugging nil)          ;; When non-nil, print debugging information.
  (debugging-contexts nil) ;; When non-nil, print debugging on contexts.
  (normal-queue nil)       ;; Holds rules that do not make assumptions.
  (asn-queue nil)          ;; Holds rules that make assumptions.
  (depth 0)                ;; Current stack depth for context mechanism.
  (max-depth 5)            ;; Maximum depth allowed.
  (local-data nil)         ;; Data local to the context.
  (local-rules nil)        ;; Rules local to the context.
  (rule-counter 0)         ;; Number of rules.
  (rules-run 0))           ;; Number of rules run.

(defun ftre-printer (ftre st ignore)
  (declare (ignore ignore))
   "Print representation of Ftre struct."
   (format st "<Ftre: ~A>" (ftre-title ftre)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(special *ftre*))) ;; Global default

(defvar *ftre* nil "Value of default Ftre")

(defmacro with-ftre (tre &rest forms)
   "Within extent of this form, use <tre> as the default ftre."
  `(let ((*ftre* ,tre)) ,@ forms))

(defun in-ftre (tre) 
   "reset the global value of the default ftre."
   (setq *ftre* tre))

(defmacro debugging-ftre (msg &rest args)
   "Abbreviated format statement that prints only when debugging is on."
  `(when (ftre-debugging *ftre*)
	 (format t ,msg ,@ args)))

(defmacro debugging-contexts (msg &rest args)
   "Abbreviated format statement that prints only when debugging-contexts is on."
  `(when (ftre-debugging-contexts *ftre*)
	 (format t ,msg ,@ args)))

(defun create-ftre (title &key (debugging nil)
			  (debugging-contexts nil)
			  (max-depth 5))
   "Create and return a new ftre."
   (make-ftre :title title
     :dbclass-table (make-hash-table :test #'eq)
     :debugging debugging
     :debugging-contexts debugging-contexts
     :max-depth max-depth))

(defun debug-ftre (ftre debugging context?)
   "Reset the debugging flags on this Ftre."
   (setf (ftre-debugging ftre) debugging)
   (setf (ftre-debugging-contexts ftre) context?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toplevel driver procedure

(defun run (*ftre*) 
   "User routine which evaluates user input and runs rules on the result.
    User exits by typing QUIT at prompt."
   (format t "~%>>")
   (do ((form (read) (read)))
       ((member form '(QUIT quit stop exit abort)) nil)
      (format t "~%~A" (eval form))
      (run-rules *ftre*)
      (format t "~%>>")))

(defun run-forms (*ftre* forms)
   "Evaluate the forms with the given Ftre, and run rules."
   (dolist (form forms) 
      (with-ftre *ftre* (eval form))
      (run-rules *ftre*)))

(defun show (*ftre*) 
   "Show all the data and rules in the given Ftre."
   (show-data) 
   (show-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Evaluating code in a context

(defun try-in-context (assumption form
				  &optional (*ftre* *ftre*)
				  &aux (depth 0))
   "Create context where <assumption> is valid, run rules, and return
    value for <form> before popping context."
   (setq depth (ftre-depth *ftre*))
   (when (> depth (ftre-max-depth *ftre*))
      (debugging-contexts
        "~% ~A(~D): Punting on trying ~A, too deep."
        *ftre* (ftre-depth *ftre*) assumption)
      (return-from try-in-context nil))
   (let ((old-local-data (ftre-local-data *ftre*))
         (old-local-rules (ftre-local-rules *ftre*))
         (old-normal-queue (ftre-normal-queue *ftre*))
         (old-asn-queue (ftre-asn-queue *ftre*))
         (result nil))
      (setf (ftre-normal-queue *ftre*) nil)
      (setf (ftre-asn-queue *ftre*) nil)
      (incf (ftre-depth *ftre*))
      (push (ftre-depth *ftre*) (ftre-local-data *ftre*))
      (debugging-contexts
        "~% ~A(~D): Trying ~A."
        *ftre* (ftre-depth *ftre*) assumption)
      (with-ftre *ftre*
        (if assumption (assert! assumption))
        (run-rules *ftre*)
        (debugging-contexts
          "~% ~A(~D): Context ~A for ~A."
          *ftre* (ftre-depth *ftre*) assumption form)
        (debugging-contexts
          "~%      ~D facts and ~D rules in local context."
          (- (length (ftre-local-data *ftre*))
             (length old-local-data))
          (- (length (ftre-local-rules *ftre*))
             (length old-local-rules)))
        (setq result (eval form))
        (setf (ftre-local-data *ftre*) old-local-data)
        (setf (ftre-local-rules *ftre*) old-local-rules)
        (setf (ftre-normal-queue *ftre*) old-normal-queue)
        (setf (ftre-asn-queue *ftre*) old-asn-queue)
        (decf (ftre-depth *ftre*))
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Context introduction mechanism
;; Exploring an assumption requires pushing a context.
;; Each context inherits all facts and rules from those
;; above it.  Facts and rules added inside a context
;; are flushed when that context is left.

(defun seek-in-context (assumption goal &optional (*ftre* *ftre*))
  "Assume <assumption> in new context, run rules, and check to 
   see if <goal> is true.  Return t if goal true, nil if not."
  (let ((depth (ftre-depth *ftre*)))
    (when (null goal)
	  (error "Seek-in-context requires a goal.")) 
    (when (> depth (ftre-max-depth *ftre*))
	  (debugging-contexts 
	   "~% ~A(~D): Punting on assuming ~A;"
	   *ftre* depth assumption )
	  (debugging-contexts 
	   "seeking ~A, resource limits exceeded"
	   goal)
	  (return-from seek-in-context nil))
    (let ((old-local-rules (ftre-local-rules *ftre*))
	  (old-local-data (ftre-local-data *ftre*))
	  (old-normal-queue (ftre-normal-queue *ftre*))
	  (old-asn-queue (ftre-asn-queue *ftre*))
	  (result nil))
      (setf (ftre-normal-queue *ftre*) nil)
      (setf (ftre-asn-queue *ftre*) nil)
      (incf (ftre-depth *ftre*))
      (push (ftre-depth *ftre*) (ftre-local-data *ftre*))
      (debugging-contexts
       "~% ~A(~D): Assuming ~A; seeking ~A."
       *ftre* (ftre-depth *ftre*) assumption goal)
      (if assumption (assert! assumption *ftre*))
      (with-ftre *ftre*
	 (assert! `(show ,goal))
	 (eval
	  `(rule (,goal)
		 (when (= (ftre-depth *ftre*)
			  ,(ftre-depth *ftre*))
		  (debugging-contexts
		   "~%~A (~D): Found goal ~A!"
		   *ftre* (ftre-depth *ftre*) ',goal)
		  (throw 'punt-context t)))))
      (catch 'punt-context (run-rules *ftre*))
      (debugging-contexts
	"~% ~A(~D): Retracting ~A, sought ~A."
	*ftre* (ftre-depth *ftre*) assumption goal)
      (debugging-contexts
	"~%  ~A~%  ~D facts and ~D rules in local context."
	(if (fetch goal) "Succeeded!" "Failed...")
	(- (length (ftre-local-data *ftre*))
	   (length old-local-data))
	(- (length (ftre-local-rules *ftre*))
	   (length old-local-rules)))
      (setq result (fetch goal))
      (decf (ftre-depth *ftre*))
      (setf (ftre-local-data *ftre*) old-local-data)
      (setf (ftre-local-rules *ftre*) old-local-rules)
      (setf (ftre-normal-queue *ftre*) old-normal-queue)
      (setf (ftre-asn-queue *ftre*) old-asn-queue)
      result)))
