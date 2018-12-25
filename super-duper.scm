(define super-duper 
    (lambda (source count)
        (cond 
            ;If source is null then return empty list
            ((null? source) '())
            ;If source is not a list then return the source
            ((not (list? source)) source)
            ;If source is a single element list then duplicate that element count times and return it
            ((null? (cdr source)) (make-list count (car source)))
            (else 
                ;Otherwise, recurse with first element of the source and duplicate, then combine with
                ;rest of source passed into recursive function
                (append (make-list count (super-duper (car source) count)) (super-duper (cdr source) count))
            )
        )
    )
)

(define test-super-duper
    (lambda ()
        (display "input: 123 1\nexpected output: 123\nactual output:   ")
        (display (super-duper 123 1))
        (display "\n")
        (display "input: 123 2\nexpected output: 123\nactual output:   ")
        (display (super-duper 123 2))
        (display "\n")
        (display "input: '() 1\nexpected output: ()\nactual output:   ")
        (display (super-duper '() 1))
        (display "\n")
        (display "input: '() 2\nexpected output: ()\nactual output:   ")
        (display (super-duper '() 2))
        (display "\n")
        (display "input: '(x) 1\nexpected output: (x)\nactual output:   ")
        (display (super-duper '(x) 1))
        (display "\n")
        (display "input: '(x) 2\nexpected output: (x x)\nactual output:   ")
        (display (super-duper '(x) 2))
        (display "\n")
        (display "input: '(x y) 1\nexpected output: (x y)\nactual output:   ")
        (display (super-duper '(x y) 1))
        (display "\n")
        (display "input: '(x y) 2\nexpected output: (x x y y)\nactual output:   ")
        (display (super-duper '(x y) 2))
        (display "\n")
        (display "input: '((a b) y) 3\nexpected output: ((a a a b b b) (a a a b b b) (a a a b b b) y y y)\nactual output:   ")
        (display (super-duper '((a b) y) 3))
        (display "\n")
        "Test Finished!"
    )
)
