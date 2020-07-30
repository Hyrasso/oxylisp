
;; Conditionals

(define (not obj) (if obj #f #t))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
      (begin result1 result2 ...))
    ((cond (test => result))
      (let ((temp test))
        (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
      (let ((temp test))
        (if temp
            (result temp)
            (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
      (let ((temp test))
        (if temp
            temp
            (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
      (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)clause1 clause2 ...)
      (if test
        (begin result1 result2 ...)
        (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...) clauses ...)
      (let ((atom-key (key ...))) (case atom-key clauses ...)))
    ((case key (else => result))
      (result key))
    ((case key(else result1 result2 ...))
      (begin result1 result2 ...))
    ((case key((atoms ...) => result))
      (if (memv key ’(atoms ...))(result key)))
    ((case key((atoms ...) => result)clause clauses ...)
      (if (memv key ’(atoms ...))(result key)(case key clause clauses ...)))
    ((case key((atoms ...) result1 result2 ...))
      (if (memv key ’(atoms ...))(begin result1 result2 ...)))
    ((case key((atoms ...) result1 result2 ...)clause clauses ...)
      (if (memv key ’(atoms ...))(begin result1 result2 ...)(case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
      (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
      (let ((x test1))(if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
      (if test
        (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
      (if (not test)
        (begin result1 result2 ...)))))

;; Bindings


; (define-syntax let (syntax-rules ()((let ((name val) ...) body1 body2 ...) '((lambda (name ...) body1 body2 ...) val ...))))

(define-syntax let 
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
      ((letrec ((tag (lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))

(define <undefined> '(It is an error to access me))
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
      (letrec 'generatetempnames
        (var1 ...)
        ()
        ((var1 init1) ...)
        body ...))
    ((letrec 'generatetempnames () (temp1 ...) ((var1 init1) ...) body ...)
      (let ((var1 <undefined>) ...)
        (let ((temp1 init1) ...)
          (set! var1 temp1) ...
          body ...)))
    ((letrec 'generatetempnames (x y ...) (temp ...) ((var1 init1) ...) body ...)
      (letrec 'generatetempnames 
        (y ...)
        (newtemp temp ...)
        ((var1 init1) ...)
        body ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
      (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
      (let ((name1 val1))
        (let* ((name2 val2) ...)
          body1 body2 ...)))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
      (let ((var1 <undefined>) ...)
        (set! var1 init1)
        ...
        (let () body1 body2 ...)))))

; (define-syntax begin
;   (syntax-rules ()
;     ((begin exp ...)
;       ((lambda () exp ...)))))

(define-syntax do
  (syntax-rules (step)
    ((do ((var init step ...) ...) (test expr ...) command ...)
    (letrec ((loop
      (lambda (var ...)
        (if test
          (begin (if #f #f) expr ...)
          (begin 
            command ... 
            (loop (do step var step ...) ...))))))
      (loop init ...)))
    ((do step x)
      x)
    ((do step x y)
      y)))

; pairs stuff

; car
; cdr

; comp
(define = equal?)
(define (>= a b)
  (or
    (> a b)
    (= a b)))

(define (< a b) 
  (not (>= a b)))

; Numbers
(define (zero? n) (equal? n 0))

(define - (lambda (a b) (+ a (* -1 b))))

; hacks
(define (make-vector x) (quote x))

(define (null? e) (equal? e '()))

(define (list . a) a)

(define cons list)

; futures

; (define-syntax delay
;   (syntax-rules ()
;     ((delay expression)
;       (lambda () expression))))

; (define-syntax force
;   (syntax-rules ()
;   ((force promise)
;     (promise))))

(define (force promise)
  (if (promise-done? promise)
    (promise-value promise)
    (let ((promise* ((promise-value promise))))
    (unless (promise-done? promise)(promise-update! promise* promise))
    (force promise))))
(define promise-done?(lambda (x) (car (car x))))(define promise-value(lambda (x) ((car (cdr (car x))))))(define promise-update!(lambda (new old)(set-car! (car old) (promise-done? new))(set-cdr! (car old) (promise-value new))(set-car! new (car old))))
(define-syntax delay(syntax-rules ()((delay expression)(make-promise #t (lambda () expression)))))
(define make-promise(lambda (done? proc)(list (cons done? proc))))
