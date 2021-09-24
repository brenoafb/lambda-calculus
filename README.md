# Lambda Calculus in Haskell

```
stack build

stack run

> x
x
x

> ((lambda x. x) x)
((λ x . x) x)
x

> ((lambda x. x) (lambda x. x))
((λ x . x) (λ x . x))
(λ x . x)
```
