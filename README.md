##### **tmplang.**

*because only the temporary survives*

##### What is this?

...

##### Other names considered:

* TheShitLisp
* *I Can't Believe It's Not Scheme*
* γ-tho

##### Tutorial

This example demonstrates every single feature of the language. Plus modulo, which it doesn't actually have yet.

```
(let message-0 "is even"
  (let message-1 "is odd"
    (let test-vals (list 0 1 2 3 4 5)
      (let demo-function
        (function-def input-values
          (let first-in-list (first (deref input-values))
            (let rest-of-list (rest (deref input-values))
              (let first-msg
                (if (= (% (deref first-in-list) 2) 0) (deref message-0) (deref message-1))
                (if (= (deref rest-of-list) ())
                  (deref first-msg)
                  (let rest-msgs (function-call demo-function rest-of-list)
                    (concat (list (deref first-msg)) (deref rest-msgs))))))))
        (function-call demo-function test-vals)))))

output: ("is even", "is odd", "is even", "is odd", "is even", "is odd")

```

##### Frequently Asked Questions

> Q: Your list semantics are all screwed up. explicit symbol derefs are weird. you didn't implement macros. you're totally missing the point of LISP!

That wasn't a question.

##### TODO

* Finish the lexer and parser. Currently this is just an AST and interpreter
* Add a REPL?

