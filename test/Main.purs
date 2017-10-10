module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.ArrayBuffer (ARRAY_BUFFER, byteLength, isView, mkArrayBuffer, slice)
import Data.ArrayBuffer.DataView (getInt16be, getInt16le, getInt8, setInt16be, setInt16le, setInt8, view, viewWhole)
import Data.Typelevel.Num (d0, d1, d2, d20, d3, d4, d40, d42)
import Test.Unit (suite, test)
import Test.Unit.Assert (assertFalse, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: ∀ e. Eff ( console     :: CONSOLE
                 , testOutput  :: TESTOUTPUT
                 , avar        :: AVAR
                 , random      :: RANDOM
                 , arrayBuffer :: ARRAY_BUFFER
                 | e
                 ) Unit
main = runTest do
  suite "ArrayBuffer" do

    test "byteLength" do
      arrayBuffer <- liftEff $ mkArrayBuffer d42
      byteLength arrayBuffer `equal` 42

    test "isView" do
      arrayBuffer <- liftEff $ mkArrayBuffer d42
      assertFalse "ArrayBuffer is not expected to have a view" $
        isView arrayBuffer

    test "slice" do
      arrayBuffer <- liftEff $ mkArrayBuffer d42
      sliced <- liftEff $ slice d20 d40 arrayBuffer
      20 `equal` byteLength sliced

  suite "DataView" do

    test "viewWhole" do
      arrayBuffer <- liftEff $ mkArrayBuffer d2
      "DataView[2, 0, 2]" `equal` show (viewWhole arrayBuffer)

    test "view" do
      arrayBuffer <- liftEff $ mkArrayBuffer d3
      "DataView[3, 1, 2]" `equal` show (view d1 d2 arrayBuffer)

    test "setInt8 / getInt8" do
      arrayBuffer <- liftEff $ mkArrayBuffer d3
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt8 d1 42 dataView
      b1 <- liftEff $ getInt8 d0 dataView
      b2 <- liftEff $ getInt8 d1 dataView
      b3 <- liftEff $ getInt8 d2 dataView
      [0, 42, 0] `equal` [b1, b2, b3]

    test "setInt16be / getInt16be" do
      arrayBuffer <- liftEff $ mkArrayBuffer d4
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt16be d1 4080 dataView  -- | ____ 1111 1111 ____ = 4080
      w1 <- liftEff $ getInt16be d0 dataView -- | 0000 1111 ____ ____ = 15
      w2 <- liftEff $ getInt16be d2 dataView -- | ---- ---- 1111 0000 = -4096
      [15, -4096] `equal` [w1, w2]

    test "setInt16le / getInt16le" do
      arrayBuffer <- liftEff $ mkArrayBuffer d4
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt16le d1 4080 dataView  -- | ____ 1111 1111 ____ = 4080
      w1 <- liftEff $ getInt16le d0 dataView -- | 0000 1111 ____ ____ = -4096 in le
      w2 <- liftEff $ getInt16le d2 dataView -- | ____ ____ 1111 0000 = 15 in le
      [-4096, 15] `equal` [w1, w2]
