;; -*- geiser-scheme-implementation: guile -*-

;; SYNACOR VM - by Oak Wrap (we are computing!)
;; --------------------------------------------
;; run with (run) in a repl (geiser)

;; Needs to be cleaned up.
;; entering "gmenu" accesses game menu during use

;; save file and associated program counter (pc), registers (regv), and stack need to be hard coded
;;     into vm.
;;     File in (mem-test)
;;     pc, stack, regv in (run)
;;     subtract 1 from where you wish to enter with pc


;;(use-modules (ice-9 rdelim))
(use-modules (ice-9 binary-ports))
(use-modules (ice-9 format))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 readline))


(define debug #f)
(define pc 0)
(define vmem (list))
(define regv (vector 0 0 0 0 0 0 0 0))
(define stack (list))

(define (status inst msg val)
  (cond (debug
	 (display inst) (display "   pc: ") (display pc)
	 (display "   pc2: ") (display (* 2 pc))
	 (newline)
	 (display "    ")
	 (display msg) (display ": ") (display val)
	 
	 (newline)
	 (display "       Stack: ") (display stack)
	 (newline)
	 (display "       Reg:") (display regv)
	 (newline))))

(define (mem-test)
  (set! vmem (list))
  (set! vmem
    (call-with-input-file "challenge.bin" ;; "save.bin"
      (lambda (p)
	(let f ((x (get-bytevector-n p 2)))
	  (if (eof-object? x)
	      '()
	      (let ((op (bytevector-u16-ref x 0 (endianness little))))
		(cons op (f (get-bytevector-n p 2))))))))))

(define (write-mem)
  (let ((outmem vmem))
    (call-with-output-file "save.bin"
      (lambda (port)
	(while (not (null? outmem))
	  (let ((bv (make-bytevector 2)))
	    (bytevector-u16-set! bv 0 (car outmem) (endianness little))
	    (put-bytevector port bv 0 2)
	    )
	  (set! outmem (cdr outmem)))
	)))
  )


(define (run)
  ;;(copy-file "challenge.bin" "mem.bin")
  (mem-test)
  (set! pc -1)
  ;;(set! pc (- 1798 1)) ;; save file
  ;;(set! stack (list 0 101 13 4 32 2826 1 6124 16 6080))
  ;;(set! regv (vector 25975 25974 26006 0 101 0 0 0))
  (let ((debugGame #f)
	(debug-wmem #f)
	(debug-reg8 #f)
	(count 0)
	(step 0)
	(MAX 32768)
	(regMAX 32775)
	(callback 0)
	(reg 0)
	(nreg 0)
	(sto 0)
	(tmp 0)
	(pccount 0)
	(inputlst (list))

	(gmenu #f)
	(gmenu-input "")
	
	(halt #f)  
	(set #f)
	(push #f)
	(pop #f)
	(eq #f)
	(gt #f)
	(jmp #f)
	(jt #f)
	(jf #f)
	(add #f)
	(mult #f)
	(mod #f)
	(band #f)
	(bor #f)
	(bnot #f)
	(rmem #f)
	(wmem #f)
	(call #f)
	(ret #f)
	(out #f)
	(in #f)
	(noop #f))
    
    (while (not halt)
      (begin
	;; increment program counter
	(set! pc (+ pc 1))
	
	(let ((op (list-ref vmem pc)))
	  (cond ((and (>= op MAX)
		      (<= op regMAX))
		 (set! reg (modulo op MAX))
		 (set! op (vector-ref regv (modulo op MAX)))
		 (if (and (equal? reg 7)
			  debug)
		     (begin (display "\n**\n** Looking at reg 8! : ")
			    (display op)
			    (display " - pc: ")
			    (display pc)
			    ;;(newline)
			    ;;(display regv)
			    ;;(newline)
			    ;;(display stack)
			    (display "\n**\n")))
		 ))
	  (if (equal? pc 5491)
	      (vector-set! regv 0 6))
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Show the mysterious register 8
	  (cond (debug-reg8
		 (if (> (vector-ref regv 7) 0)
		     (begin
		       (display "\n**************\n* reg 8: ")
		       (display (vector-ref regv 7))
		       (display "\n**************\n")))))
	  
	  (cond
	   ;; run opcodes
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	   ;; set: 1 a b
	   (set
	    ;; set register <a> to the value of <b>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))

		  ((equal? step 1)
		   (vector-set! regv nreg op)
		   (status "set: 1 a b"
			   "reg value" (list nreg op))
		   (set! step 0)
		   (set! set #f))
		  )
	    )

	   ;; push: 2 a
	   (push
	    ;; push <a> onto the stack
	    (set! stack (cons op stack))
	    (status "push: 2 a"
		    "a" op)
	    (set! push #f)
	    )

	   ;; pop: 3 a
	   (pop
	    ;; remove the top element from the stack and write it into <a>; empty stack = error
	    (if (null? (car stack))
		(set! halt #t))
	    (vector-set! regv reg (car stack))
	    (status "pop: 3 a"
		    "reg value" (list reg (car stack)))
	    (set! stack (cdr stack))
	    (set! pop #f)
	    )

	   ;; eq: 4 a b c
	   (eq
	    ;; set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (if (equal? sto op)
		       (vector-set! regv nreg 1)
		       (vector-set! regv nreg 0))
		   (status "eq: 4 a b c"
			   "in b c" (list nreg sto op))
		   (set! step 0)
		   (set! eq #f)
		   ))
	    )

	   ;; gt: 5 a b c
	   (gt
	    ;; set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (if (> sto op)
		       (vector-set! regv nreg 1)
		       (vector-set! regv nreg 0))
		   (status "gt: 5 a b c"
			   "in b > c" (list nreg sto op))
		   (set! step 0)
		   (set! gt #f)
		   ))
	    )
	   
	   ;; jmp: 6 a
	   (jmp
	    ;;(seek port (* op 2) SEEK_SET)
	    
	    (status "jmp: 6 a"
		    ;;"mem address" (* op 2)
		    "mem address" op)
	    (set! jmp #f)
	    (set! pc (- op 1))
	    )

	   ;; jt: 7 a b
	   (jt
	    ;; if <a> is nonzero, jump to <b>
	    (set! jt #f)
	    (status "jt: 7 a b"
		    "true?" op)
	    (if (equal? op 0)
		(set! noop #t)
		(set! jmp #t))
	    )

	   ;; jf: 8 a b
	   (jf
	    ;; if <a> is zero, jump to <b>
	    (set! jf #f)
	    (status "jf: 8 a b"
		    "false?" op)
	    (if (equal? op 0)
		(set! jmp #t)
		(set! noop #t))
	    )
	   
	   ;; add: 9 a b c
	   (add
	    ;; assign into <a> the sum of <b> and <c> (modulo 32768)
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (vector-set! regv nreg (modulo (+ sto op) MAX))
		   (status "add: 9 a b c"
			   "in b+c" (list nreg sto op))
		   (set! step 0)
		   (set! add #f)
		   )))

	   ;; mult: 10 a b c
	   (mult
	    ;; store into <a> the product of <b> and <c> (modulo 32768)
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (vector-set! regv nreg (modulo (* sto op) MAX))
		   (status "mult: 10 a b c"
			   "in b*c" (list nreg sto op))
		   (set! step 0)
		   (set! mult #f)
		   ))
	    )

	   ;; mod: 11 a b c
	   (mod
	    ;; store into <a> the remainder of <b> divided by <c>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (vector-set! regv nreg (modulo sto op))
		   (status "mod: 11 a b c"
			   "in b mod c" (list nreg sto op))
		   (set! step 0)
		   (set! mod #f)
		   ))
	    )

	   ;; and: 12 a b c
	   (band
	    ;; stores into <a> the bitwise and of <b> and <c>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (vector-set! regv nreg (logand sto op))
		   (status "and: 12 a b c"
			   "in b and c" (list nreg sto op))
		   (set! step 0)
		   (set! band #f)
		   ))
	    )

	   ;; or: 13 a b c
	   (bor
	    ;; stores into <a> the bitwise or of <b> and <c>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  
		  ((equal? step 1)
		   (set! step 2)
		   (set! sto op))

		  ((equal? step 2)
		   (vector-set! regv nreg (logior sto op))
		   (status "or: 13 a b c"
			   "in b or c" (list nreg sto op))
		   (set! step 0)
		   (set! bor #f)
		   ))
	    )

	   ;; not: 14 a b
	   (bnot
	    ;; stores 15-bit bitwise inverse of <b> in <a>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  ((equal? step 1)
		   (vector-set! regv nreg (logxor op #x7fff))
		   (status "not: 14 a b"
			   "in b not b" (list nreg op (logxor op #x7fff)))
		   (set! step 0)
		   (set! bnot #f)
		   ))
	    )

	   ;; rmem: 15 a b
	   (rmem
	    ;; read memory at address <b> and write it to <a>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! nreg reg))
		  ((equal? step 1)
		   ;;(seek port (* op 2) SEEK_SET)
		   (set! callback pc)
		   (set! pc (- op 1)) ;; seek replacement
		   (set! step 2))
		  ((equal? step 2)
		   (vector-set! regv nreg op)
		   (set! pc callback)
		   (status "rmem: 15 a b"
			   "in value" (list nreg op))
		   ;;(seek port (* 2 (+ callback 1)) SEEK_SET)
		   (set! step 0)
		   (set! rmem #f)))
	    )

	   ;; wmem: 16 a b
	   (wmem
	    ;; write the value from <b> into memory at address <a>
	    (cond ((equal? step 0)
		   (set! step 1)
		   (set! sto op)
		   (if debug-wmem (display "\n\n* * wmem used\n\n"))
		   )
		  ((equal? step 1)
		   
		   ;;(seek port (* sto 2) SEEK_SET)
		   ;; (let ((bv (make-bytevector 2)))
		   ;;   (bytevector-u16-set! bv 0 op (endianness little))
		   ;;   ;;(put-bytevector port bv 0 2)
		   ;;   )
		   (list-set! vmem sto op)
		   (status "wmem: 16 a b"
			   "address value" (list sto op))
		   ;;(set! callback pc)
		   (set! step 2))
		  ((equal? step 2)
		   ;;(seek port (* 2 (+ callback 1)) SEEK_SET)
		   (set! pc (- pc 1))
		   (set! step 0)
		   (set! wmem #f)))
	    )

	   ;; call: 17 a
	   (call
	    ;; write the address of the next instruction to the stack and jump to <a>
	    ;;(seek port (* op 2) SEEK_SET)
	    (set! stack (cons (+ pc 1) stack))
	    (status "call: 17 a"
		    "a" op)
	    ;; (if (or (equal? pc 6055)
	    ;; 	    (equal? pc 6066))
	    ;; 	(set! pccount (+ 1 pccount))
	    ;; 	(begin
	    ;; 	  (display "pc jmp: ")
	    ;; 	  (display (list pc op))
	    ;; 	  (newline)
	    ;; 	  (display pccount)
	    ;; 	  (newline)))
	    (set! pc (- op 1))
	    (set! call #f)
	    )

	   ;; ret: 18
	   (ret
	    ;; remove the top element from the stack and jump to it; empty stack = halt
	    (cond ((null? (car stack))
		   (display "\n\nReturn failed. Empty stack. ")
		   (set! halt #t)))
	    ;;(seek port (* 2 (car stack)) SEEK_SET)
	    (status "ret: 18"
		    "address" (list (car stack) (* 2 (car stack))))
	    (set! pc (- (car stack) 1))
	    (set! stack (cdr stack))
	    (set! ret #f)
	    )
	   
	   ;; out: 19 a
	   (out
	    (if (> op MAX) (display "Error: modulo may need to be performed on char"))
	    (display (integer->char op))
	    (set! out #f)
	    )

	   ;; in: 20 a
	   (in
	    ;; read a character from the terminal and write its ascii code to <a>;
	    ;; it can be assumed that once input starts, it will continue until a newline is encountered;
	    (if (null? inputlst)

		(let ((input-user (readline "synacor-> ")))
		  (cond ((equal? input-user "gmenu")
			 (set! callback pc)
			 (set! in #f)
			 (set! gmenu #t)
			 )
			(else
			 (set! pc (- pc 1))
			 (set! inputlst (append
				(map (lambda (x) (char->integer x))
				     (string->list input-user))
				(list 10)))
			 )))
		(begin
		  (status "in: 20 a"
			  reg regv)
		  (vector-set! regv reg (car inputlst))
		  (set! inputlst (cdr inputlst))
		  ;;(if (not debug-reg8) (set! debug-reg8 #t))
		  (set! in #f)))
	    
	    (cond (debugGame
		   (display "pc:    ") (display pc) (newline)
		   (display "reg:   ") (display reg) (newline)
		   (display "reg:   ") (display regv) (newline)
		   (display "stack: ") (display stack) (newline)))
		  
	    ;;(set! in #f)
	    )

	   ;; Game Menu - hacks!
	   (gmenu
	    (display "\n\n== Welcome to Game Menu ==\n")

	    (cond ((equal? step 0)
		   (display "   options : (1) Save\n")
		   (display "           : (2) Inspect registers and stack\n")
		   (display "           : (3) Change register 8\n")
		   (display "           : (5) Exit\n")
		   
		   (set! gmenu-input (string->number (readline "game menu-> ")))
		   (set! step 1))
		  
		  ((equal? step 1)
		   (case gmenu-input
		     ((1)
		      (display "Saving state to save.bin ...\n")
		      (write-mem)
		      (display "Saved!\n\n")
		      (display "Callback pc: ")
		      (display callback)
		      (newline)
		      (display "\nstack: ")
		      (display stack)
		      (newline)
		      (display "\nregisters: ")
		      (display regv)
		      (newline)

		      (readline "return to menu->")
		      (set! step 0)
		      )
		     ((2)
		      (display "Callback pc: ")
		      (display callback)
		      (newline)
		      (display "\nstack: ")
		      (display stack)
		      (newline)
		      (display "\nregisters: ")
		      (display regv)
		      (newline)

		      (readline "return to menu->")
		      (set! step 0)
		      )
		     ((3)
		      (vector-set! regv 0 6)
		      (vector-set! regv 7 (string->number (readline "enter new value-> ")))
		      (set! step 0)
		      )
		     ((5)
		      (set! step 0)
		      (set! pc (- callback 1))
		      (set! gmenu #f)
		      (set! in #t))
		     (else
		      (display "Unknown option.\n")
		      (set! tmp (readline "return to menu->"))
		      (set! step 0)))
		   ))
	    )

	   ;; noop: 21
	   (noop
	    (set! noop #f)
	    (status "noop: 21"
		    "noop" 0)
	    )

	   ;; test for opcodes
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   
	   ;; halt: 0
	   ((equal? op 0)
	    (set! halt #t)
	    (newline)
	    (display "halting ") (display pc))

	   ;; set: 1 a b
	   ((equal? op 1)
	    (set! set #t))

	   ;; push: 2 a
	   ((equal? op 2)
	    (set! push #t))

	   ;; pop: 3 a
	   ((equal? op 3)
	    (set! pop #t))
	   
	   ;; eq: 4 a b c
	   ((equal? op 4)
	    (set! eq #t))

	   ;; gt: 5 a b c
	   ((equal? op 5)
	    (set! gt #t))

	   ;; jmp: 6 a
	   ((equal? op 6)
	    ;; jump to <a>
	    (set! jmp #t))

	   ;; jt: 7 a b
	   ((equal? op 7)
	    (set! jt #t))

	   ;; jf: 8 a b
	   ((equal? op 8)
	    (set! jf #t))

	   ;; add: 9 a b c
	   ((equal? op 9)
	    (set! add #t))

	   ;; mult: 10 a b c
	   ((equal? op 10)
	    (set! mult #t))

	   ;; mod: 11 a b c
	   ((equal? op 11)
	    (set! mod #t))

	   ;; and: 12 a b c
	   ((equal? op 12)
	    (set! band #t))

	   ;; or: 13 a b c
	   ((equal? op 13)
	    (set! bor #t))

	   ;; not: 14 a b
	   ((equal? op 14)
	    (set! bnot #t))

	   ;; rmem: 15 a b
	   ((equal? op 15)
	    (set! rmem #t))

	   ;; wmem: 16 a b
	   ((equal? op 16)
	    (set! wmem #t))

	   ;; call: 17 a
	   ((equal? op 17)
	    (set! call #t))

	   ;; ret: 18
	   ((equal? op 18)
	    (set! ret #t))
	   
	   ;; out: 19 a
	   ((equal? op 19)
	    (set! out #t))

	   ;; in: 20 a
	   ((equal? op 20)
	    (set! in #t))
	   
	   ;; noop: 21
	   ((equal? op 21)
	    ;;noop
	    (cond (debug
		   (display "\nnoop-a ")
		   (display "  pc: ")
		   (display pc)
		   (newline)))
	    )
	   ))
	
	(set! count (+ count 1))
	)) ;; end while
    (newline)
    (display count) (display " iterations") (newline)
    ) ;; end let
  )
