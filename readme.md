# Lisp interpreter 

Implementation from http://norvig.com/lispy.html

Example of impl in rust: https://stopa.io/post/222

Scheme : https://en.wikipedia.org/wiki/Scheme_(programming_language)  
https://small.r7rs.org/attachment/r7rs.pdf

Scheme book: https://www.scheme.com/tspl4/

Grammar: https://www.scheme.com/tspl4/grammar.html#./grammar:h0

forms: https://www.scheme.com/tspl4/summary.html#./summary:h0

lisp in lisp, metacircular thingy : https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1

# TODOs

- [x] : Tail recurseion 
    - [ ] and everything in there: http://norvig.com/lispy2.html
- [x] a repl
    - better error handling to not be kicked out all the time
- [x] executing from files
    - [ ] maybe some load macro
- [ ] refactor: how does interpreter, env and eval relates to each other code wise  
    improve exp generation from string, clumsy (exp_from_token seems a bit weird and not in the right place)
- [ ] : explore the use of Rc::clone each time eval needs to be passed as ref is called, might not be necessary

## interpreter.rs
- [x] define what is environment (hashmap string -> Exp)
- [x] how to define procedure/user defined procedure (lambda) in the environmnet, they are 'tokens' : build an env when run with params updated with arguments value 
- [x] Maybe put seperation between tokens and ast types (symbol, lambda, values)
- [x] what does eval do : takes Exp returns Exp
- [x] what does sould eval do : takes Exp returns Result< Exp>
    - [ ] : better errors, giving actual info
- implement Fundamental forms: ✓ define, ✓ lambda, ✓ quote, ✓ if, define-syntax, let-syntax, letrec-syntax, syntax-rules, ✓ set!  
    load derived form from file?
- [ ] : syntax stuff (define-syntax, syntax-rules)
    It seems like the scoping rules for syntax rules are the same as for lambda, they should be called the same way lambda are (pointing the env to where it was at definition time)
    Syntactic things args are not evaluated before evaluating the syntax, syntax returns code to be executed
    Lambda args are evaluated and then the lambda code is evaluated
- implement some std funct (maths, comp is a good start), see for things with side effects (print, i/o)  
    std funct needs either a new exp type or maybe they could be implemented alongside keywords like if, define, ... 
    - maths (+-/*)
    - type comp number? procedure? symbol?
    - comp (><=)
    - bool (not and or)
    - array/vector/linkedlist manipulation (can use default exp::list, can be mixed type but is not linked list)  
        lambda calculus way to implement pair : https://en.wikipedia.org/wiki/Cons (from then we can build linked list and binary trees and probably more)
    - stdin/stdout (print, fopen, write, read) (needs some string/char and array type)

## integration tests suite for basic lisp programs
- tests  
    https://github.com/ashinn/chibi-scheme/tree/master/tests  
    http://git.savannah.gnu.org/cgit/guile.git/tree/test-suite/tests
- learn some lisp
- separate test files/ or split interpreter and env, interpreter is getting very confusing

## bugs
- strange recursion causing stack overflow: probably because building a list like
    ((... (1 2) adding 0) for 0) each 0) call 0)
    ```
    (define foo (lambda x (foo x 0)))
    (foo 1 2)
    ```
    Having the list on the heap would be a solution, this code is also data thing is weird 