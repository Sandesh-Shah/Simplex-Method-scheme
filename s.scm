(define (simplex x)

  (define pos-slack (+ (- (length (car x)) (length x)) 1))

  (define (display-table table)
    (if (null? table) (newline)
	(begin (display-row (car table))
	       (display-table (cdr table)))))

  (define (display-row row)
    (if (null? (cdr row)) (begin (display (car row))
				 (newline))
	(begin (display (car row))
	       (display "\t")
	       (display-row (cdr row)))))


  (define (create-top row counter)
    (if (null? (cdr row)) '("R.H.S")
	(cons (string-append "x" (number->string counter))
	      (create-top (cdr row) (+ counter 1)))))

  (define (create-line row)
    (if (null? row) '(---)
	(cons '---
	      (create-line (cdr row)))))

  (define line (create-line (car x)))
  (define top (cons (cons "B.V" (create-top (car x) 1)) line))

  (define (create-rest table counter)
    (if (null? (cdr table)) (cons line
				  (list (cons "Z" (car table))))
	(cons (cons (string-append "x" (number->string counter)) (car table))
	      (create-rest (cdr table) (+ counter 1)))))

  (define (noneg-elements? list)
    (cond ((null? (cdr list)) #t) ;;True that no negative elements
	  ((< (car list) 0) #f)
	  (else (noneg-elements? (cdr list)))))

  (define (last-element list)
    (if (null? (cdr list)) (car list)
	(last-element (cdr list))))

  (define (simplex-next lp tbl)
    (let ((lastrow (last-element lp)))
      (define (most-negative row)       ;;Pivot column
	(define (iter position value row)
	  (if (null? (cdr row)) '()
	      (if (> value (cadr row))
		  (cons position
			(iter (+ position 1) (cadr row)
			      (cdr row)))
		  (iter (+ position 1) value (cdr row)))))
	(last-element (cons 0
			    (iter 1 (car row) row))))

      (let ((pivot-column (most-negative lastrow)))
	(define (min-ratio lp)          ;;Pivot row

	  (define (ratio lp)
	    (if (null? (cdr lp)) '()
		(let* ((row (car lp))
		       (element (list-ref row pivot-column)))
		  (if (= element 0) (cons -1
					  (ratio (cdr lp)))
		      (cons (/ (last-element row) element)
			    (ratio (cdr lp)))))))

	  (define (iter position value l)
	    (if (null? l) '()
		(if (or (negative? (car l))
			(<= value (car l)))
			(iter (+ position 1) value (cdr l))
			(cons position
			      (iter (+ position 1) (car l)
				    (cdr l))))))

	  (define (find-mini counter ratio-l)
	    (if (null? (cdr ratio-l))
		(if (negative? (car ratio-l)) '()
		    (list counter))
		(if (<= 0 (car ratio-l))
		    (cons counter
			  (iter (+ counter 1) (car ratio-l) (cdr ratio-l)))
		    (find-mini (+ counter 1) (cdr ratio-l)))))

	  (let* ((r-list (ratio lp))
		 (it (find-mini 0 r-list)))
	    (if (null? it)
		(display "The lp has no solution")
		(last-element it))))

	(let* ((pivot-rowno (min-ratio lp))
	       (pivot-row (list-ref lp pivot-rowno))
	       (pivot-element (list-ref pivot-row pivot-column))
	       (y (map (lambda (x) (/ x pivot-element)) pivot-row)))

	  (define (row-operation row)
	    (let ((y1 (map (lambda (x)
			     (* x (list-ref row pivot-column))) y)))
	      (map - row y1)))

	  (define (new counter lp)
	    (cond ((null? lp) '())
		  ((= counter pivot-rowno)
		   (cons y
			 (new (+ counter 1) (cdr lp))))
		  (else (cons (row-operation (car lp))
			      (new (+ counter 1) (cdr lp))))))

	  (define (create-rest lp counter pivot-rowno)
	    (cond ((null? (cdr lp)) (cons line (list (cons "Z" (car lp)))))
		  ((= counter (+ pivot-rowno pos-slack))
		   (cons (cons (string-append "x" (number->string (+ pivot-column 1)))
			       (car lp))
			 (create-rest (cdr lp) (+ counter 1) pivot-rowno)))
		  (else (cons (cons (car
				     (list-ref tbl (+ (- counter pos-slack) 2)))
				    (car lp))
			      (create-rest (cdr lp) (+ counter 1) pivot-rowno)))))

	  (let ((n (new 0 lp)))
	    (define rest (create-rest n pos-slack pivot-rowno))
	    (define tab (cons (car top) (cons (cdr top) rest)))
	    (newline)
	    (display "Pivot-column: ") (display (+ pivot-column 1))
	    (newline)
	    (display "Pivot-row: ") (display (+ pivot-rowno 1))
	    (newline) (newline)
	    (display "\t \t \t Table") (newline)
	    (display-table tab) (newline) (newline)

	    (define (repeats)
	      (begin
		(write "Enter c to continue and q to quit.")
		(newline)
		(let ((input (read)))
		  (cond ((eq? input 'c)
			 (if (noneg-elements? (last-element n))
			     (display "This is the optimal solution")
			     (simplex-next n tab)))
			((eq? input 'q) (display "Good Bye."))
			(else
			 (begin (display "Error.")
				(repeats)))))))
	    (repeats))))))

  (define (repeat)
    (begin
      (write "Enter c to continue and q to quit.") (newline)
      (let ((input (read)))
	(cond ((eq? input 'c)
	       (if (noneg-elements? (last-element x))
		     (display "This is the optimal solution.")

		   (simplex-next x tab)))
	      ((eq? input 'q) (display "Good Bye."))
	      (else
	       (begin (display "Error.")
		      (repeat)))))))

  (define rest (create-rest x pos-slack))
  (define tab (cons (car top) (cons (cdr top) rest)))

  (newline)
  (display "\t \t \t Table") (newline)
  (display-table tab) (newline)
  (repeat))


(define (simplex-create)
  (define (length-equal? l)
    (let ((size (length (car l))))
      (define (iter l b)
	(if (null? l) b
	    (if b
		(iter (cdr l)  (= (length (car l)) size))
		b)))
      (iter (cdr l) #t)))

  (define (list-numb? l)
    (define (check l)
      (if (null? l) #t
	  (if (number? (car l))
	      (check (cdr l))
	      #f)))
    (define (iter l)
      (if (null? l) #t
	  (if (check (car l))
	      (iter (cdr l))
	      #f)))
    (iter l))

  (newline) (newline)
  (display '((row1) (row2)...(objective func))) (newline)
  (display "For instance:--")
  (display '((2 1 1 0 3) (1 -1 0 1 1) (-14 -4 0 0 0)))
  (newline) (newline)

  (display "Enter q to quit.") (newline)
  (write "Enter the LP in standard form in the above format:")
  (newline)

  (define x (read))

  (if (eq? 'q x) (begin (display "Good Bye.") (newline))
      (if (and (list? x) (not (null? x)) (list? (car x)))
	  (if (list-numb? x)
	      (if (length-equal? x)
		  (begin
		    (simplex x)
		    (simplex-create))

		  (begin
		    (newline)
		    (write "Rows are not of equal length.")
		    (newline) (newline)
		    (simplex-create)))
	      (begin
		(newline)
		(write "Some entries are not numbers")
		(newline) (newline)
		(simplex-create)))
	  (begin
	    (newline)
	    (write "Incorrect input")
	    (newline) (newline)
	    (simplex-create)))))
(begin
  (display "\t \t *** SIMPLEX METHOD ***")
  (display "\t By Sandesh Shah")
  (simplex-create))
