(*let test str =
    let res = Lexer.lex str
    printfn "%A" res

test "(set foo 1)"
test "123 4.5 -6 \"abc\" @ , ,@ #t #false #\"a b\" 00 "

let test2 str =
    let res = Lexer.lex str
    printfn "%A" res
    let res' = Parser.parseExprs res
    printfn "%A" res'

test2 "(set foo 1)"
test2 """
(def fact [n (int)]
    (if (= n 0) then
        1
    else
        (* n (fact (- n 1)))))
(print (fact 5))
"""
test2 """
(global assert
(macro _ (*body)
    `(progn
        (set expression ,(car *body))
        (if (not (eval expression))
            (then (throw ((NSException alloc)
                            initWithName:"NuAssertionFailure"
                            reason:,(*body stringValue)
                            userInfo:nil)))))))
"""
*)

let run str =
    let res = Lexer.lex str
    //printfn "%A" res
    let res' = Parser.parseExprs res
    //printfn "%A" res'
    ignore <| List.map Eval.eval res'

Builtins.buildBuiltins()

run """
(global greeting "Hello, world!")
(print greeting)

(macro function (name args &body)
    `(global ,name (do ,args ,@body)))

(function nil? (value)
    ((eq nil value) or: (eq '() value)))

(macro if (value then &else)
    `(cond
        (,value ,@(cdr then))
        (true ,@(cond
            ((nil? else) nil)
            (true (cdr (car else)))))))

(if (eq 1 1)
    (then (print 1)))

(if (eq 1 2)
    (then (print 1))
    (else (print 2)))

(function + (&rest)
    (if (eq rest '()) 
        (then nil)
        (else
            (if (eq (cdr rest) '())
                (then (car rest))
                (else
                    (let [first rest' second] 
                        (set first (car rest))
                        (set rest' (cdr rest))
                        (set second (car rest'))
                        (apply + (concat (list (first +: second)) (cdr rest')))))))))

(print (list "===" (+ 1 2 3 4)))
(print (list "===" (* 1 2 3 4)))

(print function)
(global foo
    (do (x)
        (print x)
        (print (+ x 1))))
(foo 1)
(let ((a 1)
      (b 2))
    (print (+ a b)))
(print (list nil true false 'nil))

(print (nil class-name))
(print ((nil class) class-name))
(print ((nil class) new))
(print (nil equals: 1))
(print (nil equals: nil))
(print (nil equals: ((nil class) new)))
(print (1 equals: 1))

(cond
    (false (print 1))
    (nil (print 2))
    ((eq 1 1) (print 3)))

(global fact
    (do (n (int))
        (cond
            ((eq n 0) 1)
            (true (* n (fact (- n 1)))))))

(print (fact 5))

(function add (a b)
    (+ a b))
(print add)
(print (add 1 2))

(print "\n\n")

(let ((a 1))
    (print a)
    (set a 2)
    (print a))

(print (1.2 sign))
(print (1.2 +: (1 float)))
(print (-1.2 round-as: #zero))

(print ("a.b.c" split-on: "."))
(print ("a.b.c" split-on: "." count: 2))

(class Point is Object
    (ivar
        (x (int) #access)
        (y (int) #access))
    
    (+ (Point) x: (int) y: (int) is
        ((Point new) initWithX: x andY: y))

    (- (Point) initWithX: x (int) andY: y (int) is
        (set @x x)
        (set @y y)
        this)
    
    (- (string) string is
        (+ "(" @x ", " @y ")")))

(let [(p (Point x: 1 y: 2))]
    (print Point)
    (print (Point class-name))
    (print p))
"""