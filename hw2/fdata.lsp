;; -*- Mode: Lisp; -*-

;;;; Database for Fast Tiny Rule Engine

;;; Copyright (c) 1988-1991, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :cl-user)

;; An assertion is just the lisp form itself.
;; The "dbclass" of an assertion is the leftmost constant
;; symbol in the form.  Dbclasses are used to store rules
;; and data.

(defstruct (dbclass (:print-function print-dbclass-struct))
     name		;a symbol
     ftre               ;ftre it belongs to
     facts		;facts of this dbclass
     rules)		;rules applicable to this dbclass

(defun print-dbclass-struct (cl st ignore)
  (declare (ignore ignore))
  (format st "<Dbclass ~D>" (dbclass-name cl)))

(defun show-data (&optional (stream *standard-output*)
			    &aux counter)
  "Show all data in the default ftre."
  (setq counter 0)
  (format stream "~%In global context: ")
  (maphash #'(lambda (key dbclass)
               (declare (ignore key))
	       (dolist (datum (dbclass-facts dbclass))
		       (incf counter)
		       (format stream "~%~A" datum)))
	   (ftre-dbclass-table *ftre*))
  (format stream "~%  ~D assertions in global context."
	  counter)
  (when (> (ftre-depth *ftre*) 0)
	(format stream "~% In current context:")
	(dolist (datum (reverse (ftre-local-data *ftre*)))
		(unless (numberp datum) (incf counter))
		(format stream "~% ~A." datum)))
  counter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Inserting data

(defun assert! (fact &optional (*ftre* *ftre*))
   "Assert that <fact> is true in given Ftre."
   (when (insert fact *ftre*) (try-rules fact *ftre*)))

(defmacro rassert! (fact) 
   "Same as assert, but with bound pattern variables filled in."
   `(assert! ,(quotize fact)))

(defun insert (fact ftre &aux dbclass)
  "Insert a single fact into the Ftre's database."
  (when (null fact) (error "~% Can't assert nil."))
  (setq dbclass (get-dbclass fact ftre))
  (cond ((member fact (dbclass-facts dbclass)
		 :test #'equal) nil)
	((= (ftre-depth ftre) 0)
	 (push fact (dbclass-facts dbclass)))
	((member fact (ftre-local-data ftre)
		 :test #'equal) nil)
	(t (push fact (ftre-local-data *ftre*)))))

(defun get-dbclass (fact ftre &aux dbclass)
  "Retrieved the indexed dbclass for the given fact."
  ;; Retrieve DBClass using first available symbol in list.
  (cond ((null fact) (error "~% nil can't be a dbclass."))
	((listp fact) (get-dbclass (car fact) ftre))
	((variable? fact) ;; *env* is no longer used.
	 (cond ((boundp fact)
		(get-dbclass (symbol-value fact) ftre))
	       (t (error "~%Dbclass unbound: ~A" fact))))
	((symbolp fact)
	 (cond ((gethash fact (ftre-dbclass-table ftre)))
	       (t (setq dbclass
			(make-dbclass :name fact :ftre ftre 
				    :facts nil :rules nil))
		  (setf (gethash
			 fact (ftre-dbclass-table ftre))
			dbclass)
		  dbclass)))
	(t (error "Bad dbclass type: ~A" fact))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Retrieving data

(defun fetch (pattern &optional (*ftre* *ftre*)
		      &aux bindings unifiers)
   "Given a pattern, retrieve all matching facts from the database."
   (dolist (candidate (get-candidates pattern *ftre*)
             unifiers)
      (setq bindings (unify pattern candidate))
      (unless (eq bindings :fail)
         (push (sublis bindings pattern) unifiers))))

(defun get-candidates (pattern ftre)
   "Get potentially matching facts from Ftre database."
   (append (ftre-local-data ftre)
     (dbclass-facts (get-dbclass pattern ftre))))

(defun map-dbclass (proc &optional (*ftre* *ftre*))
   "call the given procedure for each dbclass name and
    value in the ftre's database."
  (maphash #'(lambda (name dbclass) (declare (ignore name))
	       (funcall proc dbclass))
	   (ftre-dbclass-table *ftre*)))