# Lisp interpreter 

Implementation from http://norvig.com/lispy.html

# TODOs

## interpreter.rs
- [ ] define what is environment (hashmap str -> Exp ?/tree/struct with parent env)
- [x] how to define procedure/user defined procedure (lambda) in the environmnet, they are 'tokens' : build an env when run with params updated with arguments value 
- Maybe put seperation between tokens and ast types (symbol, lambda, values)
- [x] what does eval do : takes Exp returns Exp
- implement some std funct (maths is a good start), see for things with side effects (print, i/o)

## parser.rs
- read prog from stdin or file
- function from string buffer to expr

## integration tests suite for basic lisp programs
- learn some lisp