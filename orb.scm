;; -*- geiser-scheme-implementation: guile -*-

;; Solve orb-vault grid - run with (dogrid)
;;     usually returns in a second or two but is a random walk
;;     C-c C-c to exit

;; a : the playing grid "flipped" so orb is at 0,0
;; r : move in a direction - "east <-> west" or "north <-> south only". No diagonal.
;;     might not be using 0 anymore

;; dogrid-run :
;;     p     : point to add to list
;;     lst   : points traveled
;;     count : number of steps

(define a #2((22 - 9 *) (+ 4 - 18) (4 * 11 *) (* 8 - 1)))
(define r (list -1 0 1))

(set! *random-state* (random-state-from-platform))

(define (dogrid)
  (dogrid-run (list 0 0) (list) 0))

(define (dogrid-run p lst count)
  (cond ((and (> count 12)
	      (not (equal? p (list 3 3))))
	 ;; drop list and start fresh
	 (dogrid))

	((equal? p (list 3 3)) ;; made it to vault square
	 (set! lst (append lst (list 1)))

	 (if (test (cdr lst) 22)
	     (begin
	       (display lst) (newline)
	       (dogrid))
	     (dogrid)))
	(else
	 (dogrid-run (rnd-pair p)
		     (append lst (list (array-ref a (car p) (cadr p))))
		     (+ count 1)))))

(define (test lst sum)
  (cond ((null? lst)
	 (if (equal? sum 30)
	     #t
	     #f))
	((equal? (cadr lst) 22)
	 #f)
	(else
	 (test (cddr lst) (modulo
			   ((primitive-eval (car lst))
			    sum
			    (cadr lst)) 32768)))))

(define (rnd-pair n)
  (let ((rand (random 2)))
    (if (> rand 0)
	(list (car n) (rnd (cadr n)))
	(list (rnd (car n)) (cadr n))
	)))

(define (rnd n)
  (let ((rand (random 3)))
    (let ((out (+ n (list-ref r rand))))
      (if (or (> out 3)
	      (< out 0)
	      (equal? out n))
	  (rnd n)
	  out))))
