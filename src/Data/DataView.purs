module Data.ArrayBuffer.DataView
  ( DataView
  , view
  , viewWhole
  , buffer
  , byteSize
  , byteLength
  , byteOffset
  , byteSize'
  , byteLength'
  , byteOffset'
  , getInt8
  , setInt8
  ) where

import Control.Monad.Eff (Eff)
import Data.ArrayBuffer (ARRAY_BUFFER, ArrayBuffer)
import Data.Function ((>>>))
import Data.Function.Uncurried (Fn3, Fn4, Fn5, runFn3, runFn4, runFn5)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Typelevel.Num (class Add, class Lt, class LtEq, class Nat, class Pos, D0, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Unit (Unit)

type Endianness = Boolean

-- | 1st Type - type-level natural number representing a size of the underlying `ArrayBuffer`
-- | 2nd Type - type-level natural number representing an offset in the underlying `ArrayBuffer`
-- | 2nd Type - type-level natural number representing a length of the `DataView`
foreign import data DataView :: Type -> Type -> Type -> Type

instance showDataView :: (Nat s, Nat o, Nat l) => Show (DataView s o l) where
  show dv = "DataView[" <> show (byteSize   dv) <>
                   ", " <> show (byteOffset dv) <>
                   ", " <> show (byteLength dv) <>
                   "]"

foreign import viewWhole :: ∀ s. ArrayBuffer s -> DataView s D0 s

foreign import _view :: ∀ s o l. Fn3 Int Int (ArrayBuffer s) (DataView s o l)

view :: ∀ s o l e. Nat o => Pos l => Add o l e => Lt o l => LtEq e s =>
                  o -> l -> ArrayBuffer s -> DataView s o l
view o l b = runFn3 _view (toInt o) (toInt l) b

-- | `ArrayBuffer` being mapped by the view.
foreign import buffer :: ∀ s o l. DataView s o l -> ArrayBuffer s

-- | Represents the size of the underlying `ArrayBuffer`
byteSize :: ∀ s o l. Nat s => DataView s o l -> Int
byteSize = byteSize' >>> toInt

byteSize' :: ∀ s o l. Nat s => DataView s o l -> s
byteSize' _ = undefined

-- | Represents the offset of this view from the start of its `ArrayBuffer`
byteOffset :: ∀ s o l. Nat o => DataView s o l -> Int
byteOffset = byteOffset' >>> toInt

byteOffset' :: ∀ s o l. Nat o => DataView s o l -> o
byteOffset' _ = undefined

-- | Represents the length of this view
byteLength :: ∀ s o l. Nat l => DataView s o l -> Int
byteLength = byteLength' >>> toInt

byteLength' :: ∀ s o l. Nat l => DataView s o l -> l
byteLength' _ = undefined

foreign import get :: ∀ e s o l r.
                      Fn4 String Int Endianness (DataView s o l) (Eff (arrayBuffer :: ARRAY_BUFFER | e) r)

foreign import set :: ∀ e s o l r.
                      Fn5 String Int r Endianness (DataView s o l) (Eff (arrayBuffer :: ARRAY_BUFFER | e) Unit)

-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: ∀ e s o l i .
           Nat i => Lt i l =>
           i -> DataView s o l -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Int
getInt8 offset = runFn4 get "Int8" (toInt offset) false

-- | Store int8 value at a certain index in a `DataView`.
setInt8 :: ∀ e s o l i .
           Nat i => Lt i l =>
           i -> Int -> DataView s o l -> Eff (arrayBuffer :: ARRAY_BUFFER | e) Unit
setInt8 offset i = runFn5 set "Int8" (toInt offset) i false
