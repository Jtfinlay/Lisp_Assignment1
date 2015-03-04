; James Finlay 1263258
; CMPUT 325 - LEC B1
; Assignment # 1


;QUESTION 1
;Forms a list of pairs in reverse order from given elements.
;L: List of elements
(defun form-pair (L)
	(cond 
		((null (car L)) ())
		((null (cdr L))  (list (list (car L) (car L))))
		(t (cons
			(list (cadr L) (car L)) 
			(form-pair (cddr L) )
		))
	)
)

;QUESTION 2
;Evaluate the given expressions; in addition, if the two expressions in a pair
;evaluate to the same value, the pair will be dropped from the resulting list.
(defun drop-pair (L)
	(cond
		((null L) ())
		((null (check-pair (car L))) ())
		(t (cons
			(check-pair (car L))
			(drop-pair (cdr L))
		))
	)
)
;Check to see whether the given pair are equal.
(defun check-pair (P)
	(cond
		((= (eval (first P)) (eval (car (cdr P)))) nil)
		(T (list (eval (car P)) (eval (car (cdr P)))))
	)
)
;Example run of 'drop-pair'
(defun test-drop-pair1 ()
	(drop-pair '(((+ 2 4) (* 3 4))(6 (* 2 3))))
)
;Example run of 'drop-pair'
(defun test-drop-pair2 ()
	(drop-pair '(((+ 2 4) (* 3 4))(7 (* 2 3))))
)
;Example run of  'check-pair'
(defun test-check-pair1 ()
	(check-pair '((+ 2 4) (* 3 4)))
)
;Example run of 'check-pair'
(defun test-check-pair2 ()
	(check-pair '(6 (* 2 3)))
)

;QUESTION 3
;It takes x as a list of atoms and removed repeated atoms in x.
(defun remove-duplicate (x)
	(cond
		((null x) nil)
		((contains (car x) (cdr x)) (remove-duplicate (cdr x)))
		(T (cons (car x) (remove-duplicate (cdr x))))
	)
)
;Check whether array L contains x
(defun contains (x L)
	(cond
		((null L) nil)
		((and (null x) (null (car L))) T)
		((and (atom x) (eq (car L) x)) T)
		((null (cdr L)) nil)
		((equal x (car L)) T)
		(T (contains x (cdr L)))		
		(T (contains x (cdr L)))
	)
)
;Example run of 'remove-duplicate'
(defun test-remove-duplicate ()
	(remove-duplicate '(1 2 3 1 2 4))
)
;Example run of 'contains'
(defun test-contains1 ()
	(contains 1 '(2 3 1 4))
)
;Example run of 'contains'
(defun test-contains2 ()
	(contains 1 '(2 3 4))
)
;Example run of 'contains'
(defun test-contains3 ()
	(contains nil '(3 nil 4))
)
;Example run of 'contains'
(defun test-contains4 ()
	(contains '(nil) '((1) (2) (nil) (3 4 5)))
)



;QUESTION 4
;Count the distinct atoms in a given list
(defun my-count (L)
	(cond
		((null L) 0)
		((contains (car L) (cdr L)) (+ 0 (my-count (cdr L))))
		(T (+ 1 (my-count (cdr L))))
	)
)
;Example run of 'my-count'
(defun test-my-count ()
	(my-count '(1 2 1 3 4 3))
)


;QUESTION 5
;Return the power set of the given list.
(defun power-set (L)
	(cond
		((contains nil L) (power-wrapper L))
		(T (append (power-wrapper L) '((nil))))
	)
)
;This method uses the 'power-set' parent method as a wrapper. This method
;recursively flows through to the end of the array, creating combinations
;of the values on the callback.
(defun power-wrapper (L)
	(cond
		((null (cdr L)) (list (list (car L))))
		(T (combos (car L) (power-wrapper (cdr L))))
	)
)
;This method takes a value and a 2-D array, then combines the value into
;every internal array. It returns the combined values with the original
;inputs as a 2d array.
(defun combos (x L)
	(cond
		((null L) (list (list x)))
		(T (append 
			(list (car L))
			(list (append (car L) (list x))) 
			(combos x (cdr L))))
	)
)

;Example run of 'combos'
(defun test-combos ()
	(combos 1 '((2 3) (4)))
)
;Example run of 'power-set'
(defun test-power-set1 ()
	(power-set '(1 2 3))
)
;Example run of 'power-set'
(defun test-power-set2 ()
	(power-set '(1 2 3 4))
)



;QUESTION 6a
;Find the best/worst grade from the given list that matches the input name.
(defun findOne (Name Type L)
	(cond
		((null L) nil)
		((not (eq Name (caar L))) (findOne Name Type (cdr L)))
		((null (findOne Name Type (cdr L))) (cdar L))
		((and (eq Type 'worst) (< (gradeValue (caddar L)) 
(gradeValue (cadr (findOne Name Type (cdr L))))))
			(findOne Name Type (cdr L)))
		((and (eq Type 'best) (> (gradeValue (caddar L))
(gradeValue (cadr (findOne Name Type (cdr L))))))
			(findOne Name Type (cdr L)))
		(T (cdar L))
	)
)
;Take the grade as an atom and return an integer value
(defun gradeValue (x)
	(let
		((GR (list 'A+ 'A 'A- 'B+ 'B 'B- 'C+ 'C 'C- 'D 'F)))
		(find_index x GR)
	)
)
;Return the index of 'x in (L)
(defun find_index (x L)
	(cond
		((eq x (car L)) 0)
		(T (+ 1 (find_index x (cdr L))))
	)
)
;Example run of 'findOne'
(defun test-findOne ()
	(setq L (list (list 'john 'cmput201 'A-) (list 'lily 'cmput114 'A) (list 'ann 'cmput115 'B) (list 'john 'cmput229 'C+) (list 'lily 'cmput115 'B-) (list 'john 'cmput325 'A+) (list 'lily 'cmput229 'A)))
	(findOne 'john 'best L)
)



;QUESTION 6b
;Find all courses taken by a student and the grades in increasing order of course
;numbers
(defun findAll (Name L)
	(cond
		((null L) nil)
		((not (eq Name (caar L))) (findAll Name (cdr L)))
		(T (sort 
			(cons (cdar L) (findAll Name (cdr L))) 
			'compareClass))
	)
)
;Comparator for course numbers
(defun compareClass (a b)
	(string-lessp (car a) (car b))
)
;Example run of 'findAll'
(defun test-findAll ()
        (setq L (list (list 'john 'cmput201 'A-) (list 'lily 'cmput114 'A) (list 'ann 'cmput115 'B) (list 'john 'cmput229 'C+) (list 'lily 'cmput115 'B-) (list 'john 'cmput325 'A+) (list 'lily 'cmput229 'A)))
        (findAll 'lily L)
)


;QUESTION 7a
;Return a list of all web pages that can be reached from x, excluding x itself
(defun reached (x L)
	(cond
		((null L) nil)
		(T (remove-from x (r-reached (list x) L)))
	)
)
;Recursive wrapper for the 'reached' method that takes xs and L as lists
(defun r-reached (xs L)
	(cond
		((equal (reduceList (multi-reach xs L)) xs) xs)
		(T (r-reached (reduceList (multi-reach xs L)) L))
	)
)
;Find linkages from given nodes
(defun multi-reach (xs L)
	(cond
		((null (car xs)) nil)
		(T (append (direct-reach (car xs) L) xs (multi-reach (cdr xs) L)))
	)
)
;Find linkages on next level of given node
(defun direct-reach (x L)
	(cond
		((null L) nil)
		((not (eq x (caar L))) (direct-reach x (cdr L)))
		(T (cons (cadar L) (direct-reach x (cdr L))))
	)
)
;Sort given list and remove any duplicates
(defun reduceList (L)
	(sort (remove-duplicates L) 'string-lessp)
)
;Remove 'x from (list L)
(defun remove-from (x L)
	(cond
		((null L) nil)
		((and (atom x) (eq x (car L))) (remove-from x (cdr L)))
		(T (cons (car L) (remove-from x (cdr L))))
	)
)
;Remove all in (list X) from (list L)
(defun remove-all (X L)
	(cond
		((or (null X) (null L)) L)
		(T (remove-all (cdr X) (remove-from (car X) L)))
	)
)
;Example run of 'reached'
(defun test-reached1 ()
	(reached 'a '((a b)(b c)(b e)(p b)))
)
;Example run of 'multi-reach'
(defun test-multi-reach ()
	(multi-reach '(a b) '((a b)(b c)(b e)(p b)))
)

