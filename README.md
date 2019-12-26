##### **tmplang.**

*because only the temporary survives*

##### What is this?

...

##### Other names considered:

* TheShitLisp
* *I Can't Believe It's Not Scheme*
* γ-tho

##### Tutorial

This example demonstrates every single feature of the language.

```
(let message-0 "zero"
  (let message-1 "not zero"
    (let test-vals (list 0 1 2 0 4 5)
      (let demo-function
        (function-def input-values
          (let first-in-list (first (list input-values))
            (let rest-of-list (rest (list input-values))
              (let first-msg
                (if (= (list first-in-list 0))
                  (list message-0)
                  (list message-1))
                (if (= (list rest-of-list ()))
                  first-msg
                  (let rest-msgs (function-call demo-function rest-of-list)
                    (concat (list first-msg rest-msgs))))))))
        (function-call demo-function test-vals)))))

output: ("zero", "not zero", "not zero", "zero", "not zero", "not zero")
```

##### Frequently Asked Questions

> Q: Your list semantics are all screwed up. variable is a dumb name for a
>    bound symbol. you didn't implement macros. you're missing the point of LISP.

That wasn't a question.

##### TODO

`()`


