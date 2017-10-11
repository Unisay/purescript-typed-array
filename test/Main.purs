module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.ArrayBuffer (ARRAY_BUFFER, byteLength, isView, mkArrayBuffer, slice)
import Data.ArrayBuffer.DataView (getInt16be, getInt16le, getInt32be, getInt32le, getInt8, getUint8, setInt16be, setInt16le, setInt32be, setInt32le, setInt8, setUint8, view, viewWhole)
import Data.Typelevel.Num (d0, d1, d2, d20, d3, d4, d40, d42, d6, d7)
import Data.UInt (fromInt)
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
      "DataView[0, 2]" `equal` show (viewWhole arrayBuffer)

    test "view" do
      arrayBuffer <- liftEff $ mkArrayBuffer d3
      "DataView[1, 2]" `equal` show (view d1 d2 arrayBuffer)

    -- TODO: test int8 overflow

    test "setInt8 / getInt8" do
      arrayBuffer <- liftEff $ mkArrayBuffer d3
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt8 d1 127 dataView
      b1 <- liftEff $ getInt8 d0 dataView
      b2 <- liftEff $ getInt8 d1 dataView
      b3 <- liftEff $ getInt8 d2 dataView
      [0, 127, 0] `equal` [b1, b2, b3]

    test "setUint8 / getUint8" do
      arrayBuffer <- liftEff $ mkArrayBuffer d3
      let dataView = viewWhole arrayBuffer
      liftEff $ setUint8 d1 (fromInt 250) dataView
      b1 <- liftEff $ getUint8 d0 dataView
      b2 <- liftEff $ getUint8 d1 dataView
      b3 <- liftEff $ getUint8 d2 dataView
      [fromInt 0, fromInt 250, fromInt 0] `equal` [b1, b2, b3]

-- 0---------1---------2---------3---------4
--  ____ ____ 0000 1111 1111 0000 ____ ____ = 4080
--  0000 0000 0000 1111 ____ ____ ____ ____ = 15
--  ____ ____ ____ ____ 1111 0000 0000 0000 = -4096
    test "setInt16be / getInt16be" do
      arrayBuffer <- liftEff $ mkArrayBuffer d4
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt16be d1 4080 dataView
      w1 <- liftEff $ getInt16be d0 dataView
      w2 <- liftEff $ getInt16be d2 dataView
      [15, -4096] `equal` [w1, w2]

-- 0---------1---------2---------3---------4
--  ____ ____ 0000 1111 1111 0000 ____ ____ = 4080
--  0000 0000 0000 1111 ____ ____ ____ ____ = -4096 in le
--  ____ ____ ____ ____ 1111 0000 0000 0000 = 15 in le
    test "setInt16le / getInt16le" do
      arrayBuffer <- liftEff $ mkArrayBuffer d4
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt16le d1 4080 dataView
      w1 <- liftEff $ getInt16le d0 dataView
      w2 <- liftEff $ getInt16le d2 dataView
      [-4096, 15] `equal` [w1, w2]

-- 0---------1---------2---------3---------4---------5---------6---------7
--  ____ ____ 0000 1000 0000 0100 0000 0010 0000 0001 ____ ____ ____ ____ = 134480385
--  0000 0000 0000 1000 0000 0100 0000 0010 ____ ____ ____ ____ ____ ____ = 525314
--  ____ ____ ____ ____ 0000 0100 0000 0010 0000 0001 0000 0000 ____ ____ = 67240192
    test "setInt32be / getInt32be" do
      arrayBuffer <- liftEff $ mkArrayBuffer d7
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt32be d1 134480385 dataView
      w1 <- liftEff $ getInt32be d0 dataView
      w2 <- liftEff $ getInt32be d2 dataView
      [525314, 67240192] `equal` [w1, w2]

-- 0---------1---------2---------3---------4---------5---------6---------7
--  ____ ____ 0000 1000 0000 0100 0000 0010 0000 0001 ____ ____ ____ ____ = 134480385
--  0000 0000 0000 1000 0000 0100 0000 0010 ____ ____ ____ ____ ____ ____ = 67240192 in le
--  ____ ____ ____ ____ 0000 0100 0000 0010 0000 0001 0000 0000 ____ ____ = 525314 in le
    test "setInt32le / getInt32le" do
      arrayBuffer <- liftEff $ mkArrayBuffer d6
      let dataView = viewWhole arrayBuffer
      liftEff $ setInt32le d1 134480385 dataView
      w1 <- liftEff $ getInt32le d0 dataView
      w2 <- liftEff $ getInt32le d2 dataView
      [67240192, 525314] `equal` [w1, w2]
