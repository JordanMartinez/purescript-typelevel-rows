module Test.Rows where

import Prim.Symbol as Symbol
import Rows.Map (class MapRow, class MapRowTag, Compose, Const, FromTo, MapRowTagKind, Swap, ToProxy, ToSym, Unwrap, Wrap)
import Type.Proxy (Proxy(..))

data Either l r 
  = Left l 
  | Right r

data Maybe a
  = Nothing
  | Just a

testConst :: Proxy (a :: Int, b :: Int, c :: Int, d :: Int)
testConst = test heterogenous
  where
  test :: forall from to. MapRow (Const Int) from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Array Int, b :: String, c :: Number, d :: Proxy "d")
  heterogenous = Proxy

testFromTo :: Proxy (a :: Int, b :: Int, c :: Int, d :: Int)
testFromTo = test homogenous
  where
  test :: forall from to. MapRow (FromTo String Int) from to => Proxy from -> Proxy to
  test _ = Proxy

  homogenous :: Proxy (a :: String, b :: String, c :: String, d :: String)
  homogenous = Proxy

testWrap :: Proxy (a :: Array Int, b :: Array String, c :: Array Boolean, d :: Array Number)
testWrap = test heterogenous
  where
  test :: forall from to. MapRow (Wrap Array) from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Int, b :: String, c :: Boolean, d :: Number)
  heterogenous = Proxy

testUnwrap :: Proxy (a :: Int, b :: String, c :: Boolean, d :: Number)
testUnwrap = test heterogenous
  where
  test :: forall from to. MapRow (Unwrap Array) from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Array Int, b :: Array String, c :: Array Boolean, d :: Array Number)
  heterogenous = Proxy

testSwap :: Proxy (a :: Either Int String, b :: Either Int String, c :: Either Int String, d :: Either Int String)
testSwap = test homogenous
  where
  test :: forall from to. MapRow (Swap Either) from to => Proxy from -> Proxy to
  test _ = Proxy

  homogenous :: Proxy (a :: Either String Int, b :: Either String Int, c :: Either String Int, d :: Either String Int)
  homogenous = Proxy

testCompose :: Proxy (a :: Maybe (Array Int), b :: Maybe (Array String), c :: Maybe (Array Boolean), d :: Maybe (Array Number))
testCompose = test heterogenous
  where
  test :: forall from to. MapRow (Compose (Wrap Maybe) (Wrap Array)) from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Int, b :: String, c :: Boolean, d :: Number)
  heterogenous = Proxy

testToSym :: Proxy (a :: "a", b :: "b", c :: "c", d :: "d")
testToSym = test heterogenous
  where
  test :: forall from to. MapRow ToSym from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Array Int, b :: Array String, c :: Array Boolean, d :: Array Number)
  heterogenous = Proxy

testToProxy :: Proxy (a :: Proxy "a", b :: Proxy "b", c :: Proxy "c", d :: Proxy "d")
testToProxy = test heterogenous
  where
  test :: forall from to. MapRow ToProxy from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Array Int, b :: Array String, c :: Array Boolean, d :: Array Number)
  heterogenous = Proxy

-- This last test just shows how one could use `MapRow` to transform the labels in a row

-- | Appends "foo" to each label in a row
foreign import data AppendFoo :: MapRowTagKind

instance
  ( Symbol.Append symFrom "foo" symTo
  ) => MapRowTag AppendFoo symFrom sameKind symTo sameKind

testAppendFoo :: Proxy (afoo :: Int, bfoo :: String, cfoo :: Array Boolean, dfoo :: Proxy "d")
testAppendFoo = test heterogenous
  where
  test :: forall from to. MapRow AppendFoo from to => Proxy from -> Proxy to
  test _ = Proxy

  heterogenous :: Proxy (a :: Int, b :: String, c :: Array Boolean, d :: Proxy "d")
  heterogenous = Proxy
