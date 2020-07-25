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


(define-syntax let 
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...)val ...))
    ((let tag ((name val) ...) body1 body2 ...)
      ((letrec ((tag (lambda (name ...) body1 body2 ...)))
        tag)
      val ...))))


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