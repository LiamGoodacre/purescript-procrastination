Example project using a constraint to denote deferred computations.

I introduce a type class `Defer`; used to mark arguments which should be left unevaluated until needed.

Similar to types like `=> T` in Scala.

The example scenario is that of `findMap`:

```purescript
findMap :: forall f a b. Foldable f => (a -> Maybe b) -> f a -> Maybe b
```

This function applies a testing function `a -> Maybe b` to each `a` in an `f a`.
The result is the first value constructed with `Just`, else `Nothing`.

Ideally we would like the computation to stop when we hit the first `Just`. But with the way things currently are, it doesn't.

I demonstrate a change to `Semigroup` and `First` which allows this behaviour.

