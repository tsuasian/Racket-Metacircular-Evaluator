#lang racket
(require rackunit)
(require "lazy-mceval.rkt")

(provide lazy-tests
         hybrid-tests)

(define (test-mceval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mceval exp (setup-environment))))

(define (test-mceval-sequence . exps)
  (let ((env (setup-environment)))
    (define (loop exps)
      (if (null? (cdr exps))
          (mceval (car exps) env)
          (begin (mceval (car exps) env)
                 (loop (cdr exps)))))
    (loop exps)))

(define (test-mceval-exception exp)
  (mceval exp (setup-environment)))

(define lazy-tests
  (test-suite
   "Problem 7: Extending the lazy evaluator"
   (test-suite
    "Primitives"
    (test-case "Implement +"     (check-equal? (test-mceval '(+ 4 5)) 9))
    (test-case "Implement -"     (check-equal? (test-mceval '(- 4 5)) -1))
    (test-case "Implement *"     (check-equal? (test-mceval '(* 4 5)) 20))
    (test-case "Implement /"     (check-equal? (test-mceval '(/ 8 4)) 2))
    (test-case "Implement <"     (check-equal? (test-mceval '(< 4 4)) #f))
    (test-case "Implement <="    (check-equal? (test-mceval '(<= 4 4)) #t))
    (test-case "Implement ="     (check-equal? (test-mceval '(= 4 4)) #t))
    (test-case "Implement >="    (check-equal? (test-mceval '(>= 4 4)) #t))
    (test-case "Implement >"     (check-equal? (test-mceval '(> 4 4)) #f))
    (test-case "Implement error" (check-exn (regexp "^Metacircular Interpreter Aborted$")
                                            (lambda () (test-mceval-exception '(error))))))

   (test-suite
    "and"
    (test-case "(and)"
               (check-equal? (test-mceval '(and)) #t))
    
    (test-case "(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1)))"
                (check-equal? (test-mceval '(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1))))
                              #t))
    
    (test-case "(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1)))"
               (check-equal? (test-mceval '(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1))))
                             #f))
    
    (test-case "(and 1 2 'c '(f g)))"
               (check-equal? (test-mceval '(and 1 2 'c '(f g)))
                             '(f g)))
    
    (test-case "(and ((lambda (x) x) false) (error))"
               (check-equal? (test-mceval '(and ((lambda (x) x) false) (error)))
                             #f)))

   (test-suite
    "or"
    (test-case "(or)"
               (check-equal? (test-mceval '(or))
                             #f))

    (test-case "(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1)))"
               (check-equal? (test-mceval '(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1))))
                             #t))
    
    (test-case "(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1)))"
               (check-equal? (test-mceval '(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1))))
                             #t))
    
    (test-case "(or false false false)"
               (check-equal? (test-mceval '(or false false false))
                             #f))
    
    (test-case "(or ((lambda (x) x) true) (error))"
               (check-equal? (test-mceval '(or ((lambda (x) x) true) (error)))
                             #t)))))

(define hybrid-tests
  (test-suite
   "Problem 8: A hybrid evaluator"

   (test-case "Evaluate non-delayed arguments"
              (check-exn (regexp "^/: division by zero$")
                         (lambda () (test-mceval-sequence '(define (try a b) (if (= a 0) 1 b))
                                                          '(try 0 (/ 1 0))))))

   (test-case "Don't evaluate delayed arguments"
              (check-equal? (test-mceval-sequence '(define (try a (delayed b)) (if (= a 0) 1 b))
                                                  '(try 0 (/ 1 0)))
                            1))

   (test-case "Bind delayed arguments"
              (check-equal? (test-mceval-sequence '(define (lazy-add (delayed a) (delayed b)) (+ a b))
                                                  '(lazy-add 2 3))
                            5))))
