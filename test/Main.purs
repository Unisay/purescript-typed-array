module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.ArrayBuffer (ARRAY_BUFFER, byteLength, isView, mkArrayBuffer, slice)
import Data.ArrayBuffer.DataView (getInt8, setInt8, view, viewWhole)
import Data.Typelevel.Num (d0, d1, d2, d20, d3, d40, d42)
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
      liftEff $ setInt8 d1 42 (viewWhole arrayBuffer)
      let dataView = viewWhole arrayBuffer
      r1 <- liftEff $ getInt8 d0 dataView
      r2 <- liftEff $ getInt8 d1 dataView
      r3 <- liftEff $ getInt8 d2 dataView
      [0, 42, 0] `equal` [r1, r2, r3]
