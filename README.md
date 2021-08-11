# purescript-typelevel-rows

Sometimes, you want to change a `Record rows` to `Record rows2` or a `Variant rows` to `Variant rows2` where `rows2` is some variation of `rows`:
- `Const Int`: `(a :: String, b :: Int)` becomes `(a :: Boolean, b :: Boolean)`
- `Wrap Array`: `(a :: String, b :: Int)` becomes `(a :: Array String, b :: Array Int)`

This library does not handle creating such records (use [purescript-heterogenous](https://github.com/natefaubion/purescript-heterogeneous) for that) or variants, but is intended to be used alongside of such libraries.

Rather, this library provides two type classes that work in tandem with one another to compute a derivation of an input row as an output row:
```purescript
data MapRowTagKind

class MapRow :: forall k1 k2. MapRowTagKind -> Row k1 -> Row k2 -> Constraint
class MapRow tag fromRow toRow | tag fromRow -> toRow

class MapRowTag :: forall k1 k2. MapRowTagKind -> Symbol -> k1 -> Symbol -> k2 -> Constraint
class MapRowTag tag fromSym fromK toSym toK | tag fromSym fromK -> toSym toK
```

With these two type classes, we can do the following
```purescript
test
  ::       { a ::       Int, b ::       Int, c ::       String }
  -> Proxy { a :: Array Int, b :: Array Int, c :: Array String }
test rec =  wrapInArray rec

wrapInArray
  :: forall inputRows outputRows
   . MapRow (Wrap Array) inputRows outputRows
  => { | inputRows }
  -> Proxy { | outputRows }
wrapInArray _ = Proxy

-- data Wrap f
foreign import data Wrap :: forall k1 k2. (k1 -> k2) -> MapRowTagKind
instance MapRowTag (Wrap f) fromSym fromA fromSym (f fromA)
```

See the [./test/Test/Rows.purs](./test/Test/Rows.purs) for some other examples.

Two or more transformations can be done via the `Compose` MapRowTagKind.

Note: this library has a ceiling in terms of what it can do. For anything more complicated (at the cost of efficiency in the constraint solver), consider [`purescript-typelevel-eval`](https://github.com/natefaubion/purescript-typelevel-eval).
