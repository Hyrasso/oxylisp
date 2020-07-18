# Lisp interpreter 

Implementation from http://norvig.com/lispy.html

Example of impl in rust: https://stopa.io/post/222

# TODOs

- [x] a repl
    - better error handling to not be quicked out all the time
- [ ] executing from files
- [ ] refactor: how does interpreter, env and eval relates to each other code wise  
    improve exp generation from string, clumsy (exp_from_token seems a bit weird and not in the right place)

## interpreter.rs
- [x] define what is environment (hashmap string -> Exp)
- [x] how to define procedure/user defined procedure (lambda) in the environmnet, they are 'tokens' : build an env when run with params updated with arguments value 
- [x] Maybe put seperation between tokens and ast types (symbol, lambda, values)
- [x] what does eval do : takes Exp returns Exp
- [ ] what does sould eval do : takes Exp returns Result< Exp>
- implement lambda, set, quote
- implement some std funct (maths, comp is a good start), see for things with side effects (print, i/o)  
    Should std function have a different Exp variant than runtime created lambda? relates to lambda implementation iguess
    - maths (+-/*)
    - comp (><=)
    - bool (not and or)
    - begin (for sequential evaluation)
    - array/vector/linkedlist manipulation (can use default exp::list, but can be mixed type)
    - stdin/stdout (print, fopen, write, read) (needs some string/char and array type)

## parser.rs
- read prog from stdin or file
- function from string buffer to expr

## integration tests suite for basic lisp programs
- tests
- learn some lisp (closure?)
- separate test files/ or split interpreter and env 