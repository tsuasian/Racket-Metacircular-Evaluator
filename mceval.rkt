#lang racket/base
(require (except-in racket force delay))
(require racket/mpair)

;;;;Ported to Racket by Geoffrey Mainland <mainland@drexel.edu>

;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;SECTION 4.1.1

(define (mceval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((and? exp) (process-and (rest-exps exp) env))
        ((or? exp) (process-or (rest-exps exp) env))
        ((let? exp) (process-let exp env))
        ((delay? exp) (mceval (delay->lambda exp) env))
        ((stream-cons? exp) (mceval (process-stream-cons exp) env))
        ((stream? exp) (mceval (process-stream exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mceval (cond->if exp) env))
        ((application? exp)
         (mcapply (mceval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mcapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mceval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; AND FUNCTION
(define (process-and exp env)
  [cond
    [(no-operands? exp) #t]
    [(last-exp? exp) (mceval (first-exp exp) env)]
    [(false? (mceval (first-exp exp) env)) #f]
    [else (process-and (rest-exps exp) env)]]
  )

;; OR FUNCTION

(define (process-or exp env)
  [cond
    [(no-operands? exp) #f]
    [(last-exp? exp) (mceval (first-exp exp) env)]
    [(true? (mceval (first-exp exp) env)) #t]
    [else (process-or (rest-exps exp) env)]]
  )
      
;; LET FUNCTION

(define (let-bindings exp)
  (cadr exp))

(define (let-vars exp) 
  (map car (let-bindings exp)))
 
(define (let-vals exp)
  (map cadr (let-bindings exp)))

(define (let-body exp)
  (cddr exp))

(define (process-let exp env)
  (eval-sequence (let-body exp)
                 (extend-environment
                  (let-vars exp)
                  (list-of-values (let-vals exp) env)
                  env)))

;; DELAY FUNCTION
(define (delay-body exp)
  (cdr exp))

(define (delay->lambda exp)
  (list 'memo-proc (make-lambda '() (delay-body exp))))
  
;; FORCE FUNCTION defined as a top-level definition

;; STREAM FUNCTIONS, cons as special form, rest are top level defs

(define (make-delay e)
  (list 'delay e))

(define (process-stream-cons exp)
  (list 'cons (make-delay (cadr exp)) (make-delay (caddr exp))))

; p.6 (stream expr...)
(define (stream-body exp)
  (cdr exp))
  
(define (process-stream exp)
  (cond
    [(and (eq? (first-exp exp) 'stream) (eq? (rest-exps exp) '())) 'empty-stream]
    [(eq? (first-exp exp) 'stream) (process-stream (rest-exps exp))]
    [(last-exp? exp) (list 'stream-cons (first-exp exp) 'empty-stream)]
    [else (list 'stream-cons (first-exp exp) (process-stream (rest-exps exp)))]))

;; given
  
(define (eval-if exp env)
  (if (true? (mceval (if-predicate exp) env))
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mceval (first-exp exps) env))
        (else (mceval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mceval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp)  true)
        ((string? exp)  true)
        ((char? exp)    true)
        ((boolean? exp) true)
        (else           false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag) 
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
 
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (let? exp) (tagged-list? exp 'let))
(define (delay? exp) (tagged-list? exp 'delay))
(define (stream-cons? exp) (tagged-list? exp 'stream-cons))
(define (stream? exp) (tagged-list? exp 'stream))
(define (and-body exp) (cddr exp))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env))) 
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'null null initial-env)
    (eval-definition '(define (force obj)
                        (obj)) initial-env)
    (eval-definition '(define (not x)
                        (if x false true)) initial-env)
    (eval-definition '(define (memo-proc proc)
                        (let ((already-run? false)
                              (result null))
                          (lambda ()
                            (if (not already-run?)
                                (begin (set! result (proc))
                                       (set! already-run? true)
                                       result)
                                result)))) initial-env)
    (define-variable! 'empty-stream '() initial-env)
    (eval-definition '(define (stream-empty? s)
                        (if (eq? s '())
                            #t
                            #f)) initial-env)
    (eval-definition '(define (stream-first s)
                       (force (car s))) initial-env)
    (eval-definition '(define (stream-rest s)
                       (force (cdr s))) initial-env)
    initial-env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'eq?   eq?)
        (list '+     +)
        (list '-     -)
        (list '*     *)
        (list '/     /)
        (list '<     <)
        (list '<=    <=)
        (list '=     =)
        (list '>=    >=)
        (list '>     >)
        (list 'error (lambda () (error "Metacircular Interpreter Aborted")))
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (top-mceval exp)
  (let ((val (mceval exp (setup-environment))))
    (user-print val)))

(define the-global-environment (setup-environment))

(define input-prompt "> ")

(define (driver-loop)
  (display input-prompt)
  (when (with-handlers
            ([exn:fail? (lambda (exn)
                          (display "Error: ")
                          (display (exn-message exn))
                          (newline)
                          #t)])
          (let ([input (read)])
            (if (eof-object? input)
                (begin
                  (newline)
                  #f)
                (let ([output (mceval input the-global-environment)])
                  (user-print output)
                  (newline)
                  #t))))
    (driver-loop)))

(define (main . argv)
  (driver-loop))

(provide mceval
         setup-environment
         main)

