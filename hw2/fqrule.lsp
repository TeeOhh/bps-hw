; -*- Mode: Lisp; -*-

;;;; N-Queens rules, Ftre version

;;; Copyright (c) 1992-1996 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :cl-user)

(rule ((queen ?column1 ?row1)
       (queen ?column2 ?row2)
       :test (not (or (= ?column1 ?column2)
		      (queens-okay? ?column1 ?row1
				    ?column2 ?row2))))
      (rassert! contradiction))
 