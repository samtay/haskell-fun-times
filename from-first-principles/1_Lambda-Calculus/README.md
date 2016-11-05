# All You Need Is Lambda

The labmda calulus is a model of computation devised in the 1930s by Alonzo Church. All functional
programming languages are based on the lambda calculus, but Haskell is *pure* and consequentially
*referentially transparent*.

**pure**: all features of the language are translatable into lambda expressions 

**referentially transparent**: the same function, given the same values to evaluate, will always
return the same result

### Lambda Stuffs

I am already familiar with the lambda calculus and its method of computation. On with the equivalence
exercises.

1. *λxy.xz*
 - [ ] *λxz.xz*
 - [x] *λmn.mz*
 - [ ] *λz(λx.xz)*

2. *λxy.xxy*
 - [ ] *λmn.mnp*
 - [ ] *λx(λy.xy)*
 - [x] *λa(λb.aab)*

3. *λxyz.zx*
 - [ ] *λx.(λy.(λz.z))*
 - [x] *λtos.st*
 - [ ] *λmnp.mn*

### Evaluation is Simplification

For our purposes, normal form is *beta normal form*, which is when an expression cannot be beta reduced any further. **A fully executed Haskell program is equivalent to a lambda expression evaluated into beta normal form**.

To be precise, for a function such as division `d`, `d(100, 50)` is not yet in normal form, it is simply saturated (applied). The normal form is `2`. Analogously, for the function `(/) :: Fractional a => a -> a -> a`, `100/5` is `(/)` applied to `100` and `5`. The function is only evaluated to `2` when the program is executed.

### Combinators

A combinator is a lambda term with no free variables. Combinators,
as the name suggests, serve only to *combine* the arguments they are
given.

### Divergence

Sometimes reducible lambda terms cannot reduce to beta normal form. Instead of converging, they diverge. Once such example is *(λx.xx)(λx.xx)*.

## Exercises

#### Determine if each of the following are combinators

1. Yes - *λx.xxx*
2. No - *λxy.zx*
3. Yes - *λxyz.xy(zx)*
4. Yes - *λxyz.xy(zxy)*
5. No - *λxy.xy(zxy)*

#### Determine if each of the following can be reduced to a normal form or if they diverge.

1. Converge - *λx.xxx*
2. Diverge - *(λz.zz)(λy.yy)*
3. Converge - *(λx.xxx)z*

#### Evaluate (that is, beta reduce) each of the following expressions to normal form.

1. *(λabc.cba)zz(λwv.w) = (λwv.w)zz = z*
2. *(λx.λy.xyy)(λa.a)b = (λa.a)bb = bb*
3. *(λy.y)(λx.xx)(λz.zq) = (λx.xx)(λz.zq) = (λz.zq)(λz.zq) = (λz.zq)q = qq*
4. *(λz.z)(λz.zz)(λz.zy) = (λz.zy)(λz.zy) = (λz.zy)y = yy*
5. *(λx.λy.xyy)(λy.y)y = (λy.(λy.y)yy)y = yy*
6. *(λa.aa)(λb.ba)c = (λb.ba)(λb.ba)c = (λb.ba)ac = aac*
7. *(λxyz.xz(yz))(λx.z)(λx.a) = (λxyz.xz(yz))(λ_.z)(λ_.a) = λz.(λ_.z)z((λ_.a)z)*
   *= λz.za*

Damn! I screwed up alpha equivalence. The correct answer to (7) is *λz1.za*, which means the *z* in the body is a free variable. Of course, this would have been clear if I had renamed the initial *xyz* arguments to *jki*. Good exercises.
