module Rows.Map where

import Prim.Row as Row
import Prim.RowList as RL
import Type.Proxy (Proxy)

-- | Indicates an entity used as the tag parameter for the `MapRowTag` class.
data MapRowTagKind

-- | Used in `MapRow` to determine how to transform the input row's symbol and/or kind
-- | to the output row's symbol and/or kind.
class MapRowTag :: forall k1 k2. MapRowTagKind -> Symbol -> k1 -> Symbol -> k2 -> Constraint
class MapRowTag tag fromSym fromK toSym toK | tag fromSym fromK -> toSym toK

-- | Tag for `MapRow` that works only on homogenous rows.
-- |
-- | - `FromTo String Int` - `(a :: String, b :: String)` -> `(a :: Int, b :: Int)`
foreign import data FromTo :: forall k1 k2. k1 -> k2 -> MapRowTagKind
-- data FromTo from to
instance MapRowTag (FromTo a b) sym a sym b

-- | Tag for `MapRow` that works on homogenous and heterogenous rows.
-- |
-- | - `Const Boolean` - `(a :: from1, b :: from2)` -> `(a :: Boolean, b :: Boolean)`
foreign import data Const :: forall k. k -> MapRowTagKind
-- data Const to
instance MapRowTag (Const to) sym from sym to

-- | Tag for `MapRow` that works on homogenous and heterogenous rows.
-- |
-- | - `Wrap Array` - `(a :: from1, b :: from2)` -> `(a :: Array from1, b :: Array from2)`
foreign import data Wrap :: forall k1 k2. (k1 -> k2) -> MapRowTagKind
-- data Wrap f
instance MapRowTag (Wrap f) sym a sym (f a)

-- | Tag for `MapRow` that works on homogenous and heterogenous rows
-- | (as long as `f` is the outmost kind in each label-kind association).
-- |
-- | - `Unwrap Array` - `(a :: Array from1, b :: Array from2)` -> `(a :: from1, b :: from2)`
foreign import data Unwrap :: forall k1 k2. (k1 -> k2) -> MapRowTagKind
-- data Unwrap f
instance MapRowTag (Unwrap f) sym (f a) sym a

-- | Tag for `MapRow` that works only on homogenous rows.
-- |
-- | - `Swap Either` - `(a :: Either Int String, b :: Either Int String)` -> `(a :: Either String Int, b :: Either String Int)`
foreign import data Swap :: forall k1 k2 k3. (k1 -> k2 -> k3) -> MapRowTagKind
-- data Swap bifunctor
instance MapRowTag (Swap bifunctor) sym (bifunctor a b) sym (bifunctor b a)

-- | Tag for `MapRow` that works on homogenous and heterogenous rows.
-- |
-- | - `ToSym` - `(a :: from1, b :: from2)` -> `(a :: "a", b :: "b")`
foreign import data ToSym :: MapRowTagKind
-- data ToSym
instance MapRowTag ToSym sym a sym sym

-- | Tag for `MapRow` that works on homogenous and heterogenous rows.
-- |
-- | - `ToProxy` - `(a :: from1, b :: from2)` -> `(a :: Proxy "a", b :: Proxy "b")`
foreign import data ToProxy :: MapRowTagKind
-- data ToProxy
instance MapRowTag ToProxy sym a sym (Proxy sym)

-- | Combinator tag for `MapRow`. Does the rightmost tag's transformation
-- | first and then does the leftmost transformation.
-- |
-- | - `(Compose (Wrap Array) (Wrap Maybe))` - `(a :: Int` -> `(a :: Array (Maybe Int))`
foreign import data Compose :: MapRowTagKind -> MapRowTagKind -> MapRowTagKind
-- data Compose g f
instance
  ( MapRowTag f symA a symB b
  , MapRowTag g symB b symC c
  ) => MapRowTag (Compose g f) symA a symC c


-- | Works in tandem with the `MapRowTag` MapRowTagKind class to compute a variation
-- | of the given row. See each `MapRowTagKind` tag to see how the tag
-- | modifies the row and whether that tag works on only homogenous rows
-- | or both homogenous and heterogenous rows.
class MapRow :: forall k1 k2. MapRowTagKind -> Row k1 -> Row k2 -> Constraint
class MapRow tag fromRow toRow | tag fromRow -> toRow

-- | Removes the need to include the `RowToList` constraint when using `MapRow`.
instance
  ( RL.RowToList fromRow rl
  , MapRow' tag rl toRow
  ) => MapRow tag fromRow toRow

-- | This is what implements the `MapRow` MapRowTagKind class functionality.
-- | `MapRow` exists so you don't need to add a `RowToList` constraint.
class MapRow' :: forall k1 k2. MapRowTagKind -> RL.RowList k1 -> Row k2 -> Constraint
class MapRow' tag rl toRow | tag rl -> toRow

instance MapRow' tag RL.Nil ()

-- 1. Recursively call `MapRow` on the tail of the row list to get `toTail`
-- 2. Use `MapRowTag`'s tag to determine what `toSym` and `toK` should be
-- 3. Cons that back on to `toTail` to get `toRow`
-- 4. Return `toRow` as the new row
else instance
  ( MapRow' tag rlFrom toTail
  , MapRowTag tag fromSym fromK toSym toK
  , Row.Cons toSym toK toTail toRow
  ) => MapRow' tag (RL.Cons fromSym fromK rlFrom) toRow
