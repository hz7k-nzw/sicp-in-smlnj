;;; examples for chap4_4_lazy.sml
;;;
;;; The following data are borrowed from the SICP Web Site:
;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-29.html

;;; A sample data base

(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))

(assert! (can-do-job (computer programmer)
                     (computer programmer trainee)))

(assert! (can-do-job (administration secretary)
                     (administration big wheel)))

;;; Simple queries

;;;(job ?x (computer programmer))
;;;(address ?x ?y)
;;;(supervisor ?x ?x)
;;;(job ?x (computer ?type))
;;;(job ?x (computer . ?type))

;;; Exercise 4.55
;;;(supervisor ?x (Bitdiddle Ben))
;;;(job ?x (accounting . ?y))
;;;(address ?x (Slumerville . ?y))

;;; Compound queries

;;;(and (job ?person (computer programmer))
;;;     (address ?person ?where))
;;;(or (supervisor ?x (Bitdiddle Ben))
;;;    (supervisor ?x (Hacker Alyssa P)))
;;;(and (supervisor ?x (Bitdiddle Ben))
;;;     (not (job ?x (computer programmer))))
;;;(and (salary ?person ?amount)
;;;     (lisp-value > ?amount 30000))

;;; Exercise 4.56
;;;(and (supervisor ?x (Bitdiddle Ben))
;;;     (address ?x ?y))
;;;(and (salary ?x ?y1)
;;;     (salary (Bitdiddle Ben) ?y2)
;;;     (lisp-value < ?y1 ?y2))
;;;(and (supervisor ?x ?y)
;;;     (not (job ?y (computer . ?a)))
;;;     (job ?y ?z))

;;; Rules

(assert! (rule (lives-near ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                    (address ?person-2 (?town . ?rest-2))
                    (not (same ?person-1 ?person-2)))))

(assert! (rule (same ?x ?x)))

(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))

;;;(lives-near ?x (Bitdiddle Ben))
;;;(and (job ?x (computer programmer))
;;;     (lives-near ?x (Bitdiddle Ben)))

(assert! (rule (outranked-by ?staff-person ?boss)
               (or (supervisor ?staff-person ?boss)
                   (and (supervisor ?staff-person ?middle-manager)
                        (outranked-by ?middle-manager ?boss)))))

;;; Logic as programs

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

;;;(append-to-form (a b) (c d) ?z)
;;;(append-to-form (a b) ?y (a b c d))
;;;(append-to-form ?x ?y (a b c d))

;;; Exercise 4.61

(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
               (?x next-to ?y in ?z)))

;;;(?x next-to ?y in (1 (2 3) 4))
;;;(?x next-to 1 in (2 1 3 1))

;;; Exercise 4.62
(assert! (rule (last-pair (?x) (?x))))
(assert! (rule (last-pair (?u . ?v) ?z)
               (last-pair ?v ?z)))

;;;(last-pair (3) ?x)
;;;(last-pair (1 2 3) ?x)
;;;(last-pair (2 ?x) (3))
;;;(last-pair ?x (3)) ; never stops!
