;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: myfnd.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: January 19, 2019 12:46:11
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 18a. All but ex7 and ex8 solved.
;;     ex1: 18 rules, 7 assertions
;;     ex2: 19 rules, 7 assertions
;;     ex3: 20 rules, 13 assertions
;;     ex4: 23 rules, 9 assertions
;;     ex5: 19 rules, 7 assertions
;;     ex6: 20 rules, 12 assertions
;;     ex7: 30 rules, 15 assertions
;;     ex8: 17 rules, 2 assertions
;;     ex9: 45 rules, 24 assertions
;; 
;; 18b.
;;    ex7: (implies (or P J) (and M C)) doesn't correctly find J.
;;         I believe this is due to the 'iff' rule. It doesn't assert show for each antecedent.
;;         Either when the split and assertions happen each doesn't get dealt with properly, or
;;         for this problem it may have something to do with matching variable bindings.
;;         For there's already a (show C), (show J), (show M), (show P). Changing iff to
;;         assert show for each antecedent works but increases cons's needed.
;;
;;    ex8: No assertions, nothing is known, simply a show statement. The set of rules does not
;;         contain logical equivalents either. So there's no way of mapping/matching (imp p q)
;;         to (or (not p) q). When seek-in-context is called it looks for q or a contradiction,
;;         since there is nothing stated in the problem, nothing will be found. This
;;         would also fail for a simple goal such as (implies (or a b) (or b a)). Or simply,
;;         (premise (or a b)) (goal (or b a)). "AND" seems to work fine. By introducing a rule
;;         for (show (or (not p) q)) and if (implies p q) to be true, asserting (or (not p) q)
;;         should result in mapping the two together.
;;
;;         After making the additions, all problems get solved! (finally...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; premise and goal rules


(rule ((premise ?x))
      (rassert! ?x))

(rule ((goal ?x))
      (rassert! (show ?x)))

(defun solved? (ftre problem)
  (run-forms ftre problem)
  (let ((binding-sublis (cadar (fetch '(goal ?x) ftre))))
    (if (fetch binding-sublis) binding-sublis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logical Equivalence Rules

(rule ((show (or ?a ?b)) (or ?b ?a))
      (rassert! (or ?a ?b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, some utilities:

(defvar *debug-nd* nil)

(defmacro debug-nd (format-string &rest args)
  `(if *debug-nd*
       (format t ,format-string ,@ args)))

(defvar *connective-list* '(implies and or iff not))

(defun simple-proposition? (x)
  (or (not (listp x))
      (not (member (car x) *connective-list*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementing the KM* natural deduction system
;; We begin by Modus ponens (conditional elimination)
;; and its friends.
(rule ((implies ?p ?q) ?p) ;; conditional elimination
      (debug-nd "~%~D: CE: ~A" (ftre-depth *ftre*) ?q)
      (rassert! ?q))

(rule ((show ?q) ;; backward chaining.
       :test (not (fetch ?q))
       (implies ?p ?q))
      (rassert! (show ?p)))

(rule ((show (or (not ?p) ?q)) (implies ?p ?q))
      (rassert! (or (not ?p) ?q)))

(a-rule ((show (implies ?p ?q)) ;; Conditional Introduction
	 :test (not (fetch `(implies ,?p ,?q))))
    (debug-nd "~%~D: Trying CI on (implies ~A ~A)."
              (ftre-depth *ftre*) ?p ?q)
        (when (seek-in-context ?p `(or ,?q contradiction))
               (debug-nd "~%~D: CI: (implies ~A ~A)"
                         (ftre-depth *ftre*) ?p ?q)
               (rassert! (implies ?p ?q))))
               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; And elimination and introduction

(rule ((and . ?conjuncts)) ;AND elimination
      (dolist (conjunct ?conjuncts)
	(debug-nd "~%~D: AE: ~A" (ftre-depth *ftre*) conjunct)
	(assert! conjunct)))

(rule ((show (and ?c1 ?c2)))
      (rassert! (show ?c1))
      (rassert! (show ?c2))
      (rule (?c1 ?c2) (rassert! (and ?c1 ?c2))))

(rule ((show (and ?c1 ?c2 ?c3)))
      (rassert! (show ?c1))
      (rassert! (show ?c2))
      (rassert! (show ?c3))
      (rule (?c1 ?c2 ?c3) (rassert! (and ?c1 ?c2 ?c3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Biconditional elimination and introduction

(rule ((iff ?p ?q)) ;; IFF elimination
    (debug-nd "~%~D: BE: ~A~%~D: BE: ~A"
              (ftre-depth *ftre*) `(implies ,?p ,?q)
              (ftre-depth *ftre*) `(implies ,?q ,?p))
      (rassert! (implies ?p ?q))
      (rassert! (implies ?q ?p))
      (rassert! (show ?p))
      (rassert! (show ?q)))

(rule ((show (iff ?p ?q)) ;IFF introduction
       :test (not (fetch `(iff ,?p ,?q))))
    (debug-nd "~%~D: BI-BC: (show (implies ~A ~A))"
			   (ftre-depth *ftre*) ?p ?q)
    (debug-nd "~%~D: BI-BC: (show (implies ~A ~A))" 
	      (ftre-depth *ftre*) ?q ?p)
    (rassert! (show (implies ?p ?q)))
    (rassert! (show (implies ?q ?p)))
    (rule ((implies ?p ?q) (implies ?q ?p))
	  (debug-nd "~%~D: BI: ~A"
		    (ftre-depth *ftre*) `(iff ,?p ,?q))
	  (rassert! (iff ?p ?q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dealing with negation

(rule ((not (not ?p)))
      (debug-nd "~%~D: NE: ~A" (ftre-depth *ftre*) ?p)
      (rassert! ?p)) ;; NOT elimination

(a-rule ((show (not ?p)) ;; NOT introduction
	 :test (not (or (fetch `(not ,?p))
			(eq ?p 'contradiction))))
    (debug-nd "~%~D: NI attempt: (not ~A)"
	      (ftre-depth *ftre*) ?p)
    (when (seek-in-context ?p 'contradiction)
      (debug-nd "~%~D: NI: ~A" (ftre-depth *ftre*) `(not ,?p))
      (rassert! (not ?p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Disjunction elimination and introduction

(rule ((show (or . ?disjuncts))) ;; OR introduction
      (dolist (?disjunct ?disjuncts)
	      (debug-nd "~%~D: OI-BC: (show ~A)"
			(ftre-depth *ftre*)  ?disjunct)
       (rlet ((?disjunct ?disjunct))
	     (rassert! (show ?disjunct))
	     (rule (?disjunct) 
		   (debug-nd "~%~D: OI: ~A"
			     (ftre-depth *ftre*) (cons 'OR ?disjuncts))
		   (assert! `(or . ,?disjuncts))))))

(rule ((show ?r) ;; OR elimination
       :test (not (or (fetch ?r)
		      (eq ?r 'contradiction)
		      (not (simple-proposition? ?r))))
       (or ?d1 ?d2)
       :test  (not (or (eq ?d1 'contradiction)
		       (eq ?d2 'contradiction))))
       (debug-nd "~%~D: OE-BC: (show (implies ~A ~A))"
		 (ftre-depth *ftre*) ?d1 ?r)
       (rassert! (show (implies ?d1 ?r)))	 
       (debug-nd "~%~D: OE-BC: (show (implies ~A ~A))"
		 (ftre-depth *ftre*) ?d2 ?r)
       (rassert! (show (implies ?d2 ?r)))
       (rule ((implies ?d1 ?r) (implies ?d2 ?r))
	     (debug-nd "~% ~D: OE: ~A" (ftre-depth *ftre*) ?r)
	     (rassert! ?r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indirect proof and contradiction detection

(a-rule ((show ?p) ;indirect proof.
	 :test (not (or (fetch ?p)
			(eq ?p 'contradiction)
			(not (simple-proposition? ?p)))))
    (debug-nd "~%~D: IP attempt: ~A."
	      (ftre-depth *ftre*) ?p)
    (when (seek-in-context `(not ,?p)
			   'contradiction)
      (debug-nd "~%~D: IP: ~A" (ftre-depth *ftre*) ?p)
      (rassert! ?p)))

(rule ((show contradiction) ;contradiction detection
       (not ?p) ?p)
	(debug-nd "~%~D: Contra: (not ~A) and ~A"
		  (ftre-depth *ftre*) ?p ?p)
	(rassert! contradiction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code

