module Data.ArrayBuffer
  ( ARRAY_BUFFER
  , ArrayBuffer
  , mkArrayBuffer
  , byteLength
  , slice
  , isView
  ) where

import Control.Monad.Eff (kind Effect, Eff)
import Data.Function ((>>>))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Typelevel.Num (class GtEq, class Nat, class Sub, toInt)
import Data.Typelevel.Undefined (undefined)

-- | Represents the length of an ArrayBuffer in bytes
byteLength :: ∀ n. Nat n => ArrayBuffer n -> Int
byteLength = byteLength' >>> toInt

byteLength' :: ∀ n. Nat n => ArrayBuffer n -> n
byteLength' _ = undefined

-- | Returns true if ArrayBuffer is one of the views, such as typed array objects or a DataView
-- | false otherwise.
foreign import isView :: ∀ n. ArrayBuffer n -> Boolean

-- | Creates a new ArrayBuffer of a given length
mkArrayBuffer :: ∀ e n. Nat n => n -> Eff (arrayBuffer :: ARRAY_BUFFER | e) (ArrayBuffer n)
mkArrayBuffer n = _arrayBuffer (toInt n)

-- | Creates a new ArrayBuffer with a copy of the bytes of the given arrayBuffer
-- | between beginByte (inclusive) and endByte (exclusive).
-- | Changes to the original arrayBuffer do not affect the copy returned by slice
slice :: ∀ e beginByte endByte size newSize.
         Nat beginByte =>
         Nat endByte =>
         GtEq endByte beginByte =>
         Sub endByte beginByte newSize =>
           beginByte ->
           endByte ->
           ArrayBuffer size ->
           Eff (arrayBuffer :: ARRAY_BUFFER | e) (ArrayBuffer newSize)
slice b e ab = runFn3 _slice (toInt b) (toInt e) ab


foreign import data ARRAY_BUFFER :: Effect

foreign import data ArrayBuffer :: Type -> Type

instance showArrayBuffer :: Nat n => Show (ArrayBuffer n) where
  show ab = "ArrayBuffer[" <> show (byteLength ab) <> "]"

foreign import _arrayBuffer :: ∀ e n. Int -> Eff (arrayBuffer :: ARRAY_BUFFER | e) (ArrayBuffer n)

foreign import _slice :: ∀ e size newSize .
                         Fn3 Int Int (ArrayBuffer size) (Eff (arrayBuffer :: ARRAY_BUFFER | e) (ArrayBuffer newSize))
