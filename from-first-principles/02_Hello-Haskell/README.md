# Hello Haskell!

This chapter is largely just about syntax and using the REPL, which I already know. One key take-away is below.

### Weak Head Normal Form

Recall our definition of *normal form* in [Chapter 1](../1_Lambda-Calculus). Due to Haskell's laziness, it does **not** evaluate everything to normal form by default, and instead evaluates to *weak head normal form* (WHNF). Details to follow in later chapters. As an example,
```haskell
(\f -> (1, 2 + f)) 2
```
reduces to
```haskell
(1, 2 + 2)
```
in WHNF.
