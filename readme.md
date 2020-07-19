# Lisp interpreter 

Implementation from http://norvig.com/lispy.html

Example of impl in rust: https://stopa.io/post/222

Seems like it is not exactly mit scheme?

Sceme book: https://www.scheme.com/tspl4/

Grammar: https://www.scheme.com/tspl4/grammar.html#./grammar:h0

forms: https://www.scheme.com/tspl4/summary.html#./summary:h0

# TODOs

- [ ] : everything in there: http://norvig.com/lispy2.html
- [x] a repl
    - better error handling to not be kicked out all the time
- [ ] executing from files, maybe some load macro
- [ ] refactor: how does interpreter, env and eval relates to each other code wise  
    improve exp generation from string, clumsy (exp_from_token seems a bit weird and not in the right place)
- [ ] : explore the use of Rc::clone each time eval needs to be passed as ref is called, might not be necessary
- [ ] : implementation of std 'helpers' functions from the keywords and std functions implemented in Rust

## interpreter.rs
- [x] define what is environment (hashmap string -> Exp)
- [x] how to define procedure/user defined procedure (lambda) in the environmnet, they are 'tokens' : build an env when run with params updated with arguments value 
- [x] Maybe put seperation between tokens and ast types (symbol, lambda, values)
- [x] what does eval do : takes Exp returns Exp
- [x] what does sould eval do : takes Exp returns Result< Exp>
    - [ ] : better errors, giving actual info
- implement keywords set!, quote, begin ...
- implement some std funct (maths, comp is a good start), see for things with side effects (print, i/o)  
    std funct needs either a new exp type or maybe they could be implemented alongside keywords like if, define, ... 
    - maths (+-/*)
    - comp (><=)
    - bool (not and or)
    - array/vector/linkedlist manipulation (can use default exp::list, but can be mixed type)
    - stdin/stdout (print, fopen, write, read) (needs some string/char and array type)

## integration tests suite for basic lisp programs
- tests
- learn some lisp
- separate test files/ or split interpreter and env, interpreter is getting very confusing
