;; -*- geiser-scheme-implementation: guile -*-

;; Solve teleporter - run with (call-cf)
;;     20 min or so for full list of 32768 answers

;; hash-table caching on inner recursive call and b=0 case
;; probably better ways to do this. Much faster than not doing it though.


(define h (make-hash-table 32768))

(define (call-cf)
  ;;clear hash
  (let ((count 0)
	(max 32768))
    (while (< count max)
      (hash-clear! h)
      (if (zero? (modulo count 16)) (display "."))
      (let ((out (cf 4 1 count)))
	;;(display count) (display " : ") (display out) (newline)
	(if (equal? out 6)
	    (begin
	      (display "MATCH!!! ")
	      (display count) (newline)
	      (break))
	    (set! count (+ count 1))))))
  )

(define (cf a b c)
  (let ((name (list a b c)))
    (cond ((equal? a 0)
	   (modulo (+ b 1) 32768))
	  ((equal? b 0)
	   ;;(cf (- a 1) c c)
	   (if (hash-ref h (list (- a 1) c c))
		 (hash-ref h (list (- a 1) c c))
		 (hash-set! h (list (- a 1) c c) (cf (- a 1) c c))))
	  (else

	   (cf (- a 1)
	       (if (not (hash-ref h (list a (- b 1) c)))
	       (begin
		 (hash-set! h (list a (- b 1) c)
			    (cf a (- b 1) c))
		 (hash-ref h (list a (- b 1) c)))
	       (hash-ref h (list a (- b 1) c)))
	       c)
	   ))))


