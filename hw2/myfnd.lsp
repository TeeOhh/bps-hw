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

;; 17 a and b
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
;; 18a. All but ex7 and ex8 solved.
;;     ex1: 3 rules, 7 assertions
;;     ex2: 4 rules, 7 assertions
;;     ex3: 13 rules, 13 assertions
;;     ex4: 13 rules, 9 assertions
;;     ex5: 7 rules, 7 assertions
;;     ex6: 11 rules, 12 assertions
;;     ex7: 23 rules, 15 assertions
;;     ex8: 17 rules, 2 assertions
;;     ex9: 49 rules, 24 assertions
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
;; 18c. Changed/added rules

;; Change for ex7
(rule ((iff ?p ?q)) ;; IFF elimination
    (debug-nd "~%~D: BE: ~A~%~D: BE: ~A"
              (ftre-depth *ftre*) `(implies ,?p ,?q)
              (ftre-depth *ftre*) `(implies ,?q ,?p))
      (rassert! (implies ?p ?q))
      (rassert! (implies ?q ?p))
      (rassert! (show ?p))
      (rassert! (show ?q)))

;; Change for ex8
(rule ((show (or (not ?p) ?q)) (implies ?p ?q))
      (rassert! (or (not ?p) ?q)))

;; Added rule I think is needed
(rule ((show (or ?a ?b)) (or ?b ?a))
      (rassert! (or ?a ?b)))