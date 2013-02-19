# lispenport

It's like a davenport, which is like a couch, but with a lisp!


## Use it

		erl
		> c(lispeport).
		> lispenport:repl().

		lispenport> (+ 2 2)
		4
		lispenport> (define square (lambda (x) (* x x)))
		#Fun<lispenport.3.86113503>
		lispenport> (square 244)
		59536
		lispenport> (square (/ (* 8 8) (+ 1 3)))
		256.0
		lispenport> quit
		bye
		ok
		>
