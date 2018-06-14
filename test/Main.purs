module Test.Main where

import Prelude
import Data.ArrayBuffer (byteLength, isView, mkArrayBuffer, slice)
import Data.ArrayBuffer.DataView as V
import Data.Typelevel.Num (d0, d1, d2, d20, d3, d4, d40, d42, d6, d7)
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Unit (suite, test)
import Test.Unit.Assert (assertFalse, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "ArrayBuffer" do

    test "byteLength" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d42
      byteLength arrayBuffer `equal` 42

    test "isView" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d42
      assertFalse "ArrayBuffer is not expected to have a view" $
        isView arrayBuffer

    test "slice" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d42
      sliced <- liftEffect $ slice d20 d40 arrayBuffer
      20 `equal` byteLength sliced

  suite "DataView" do

    test "viewWhole" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d2
      "DataView[0, 2]" `equal` show (V.viewWhole arrayBuffer)

    test "view" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d3
      "DataView[1, 2]" `equal` show (V.view d1 d2 arrayBuffer)

    -- TODO: test int8 overflow

    test "setInt8 / getInt8" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d3
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setInt8 d1 127 dataView
      b1 <- liftEffect $ V.getInt8 d0 dataView
      b2 <- liftEffect $ V.getInt8 d1 dataView
      b3 <- liftEffect $ V.getInt8 d2 dataView
      [0, 127, 0] `equal` [b1, b2, b3]

    test "setUint8 / getUint8" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d3
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setUint8 d1 (fromInt 250) dataView
      b1 <- liftEffect $ V.getUint8 d0 dataView
      b2 <- liftEffect $ V.getUint8 d1 dataView
      b3 <- liftEffect $ V.getUint8 d2 dataView
      [fromInt 0, fromInt 250, fromInt 0] `equal` [b1, b2, b3]

-- 0---------1---------2---------3---------4
--  ____ ____ 0000 1111 1111 0000 ____ ____ = 4080
--  0000 0000 0000 1111 ____ ____ ____ ____ = 15
--  ____ ____ ____ ____ 1111 0000 0000 0000 = -4096
    test "setInt16be / getInt16be" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d4
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setInt16be d1 4080 dataView
      w1 <- liftEffect $ V.getInt16be d0 dataView
      w2 <- liftEffect $ V.getInt16be d2 dataView
      [15, -4096] `equal` [w1, w2]

-- 0---------1---------2---------3---------4
--  ____ ____ 0000 1111 1111 0000 ____ ____ = 4080
--  0000 0000 0000 1111 ____ ____ ____ ____ = -4096 in le
--  ____ ____ ____ ____ 1111 0000 0000 0000 = 15 in le
    test "setInt16le / getInt16le" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d4
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setInt16le d1 4080 dataView
      w1 <- liftEffect $ V.getInt16le d0 dataView
      w2 <- liftEffect $ V.getInt16le d2 dataView
      [-4096, 15] `equal` [w1, w2]

-- 0---------1---------2---------3---------4
--  ____ ____ 0000 1111 1111 0000 ____ ____ = 4080
--  0000 0000 0000 1111 ____ ____ ____ ____ = 15
--  ____ ____ ____ ____ 1111 0000 0000 0000 = 61440
    test "setUint16be / getUint16be" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d4
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setUint16be d1 (fromInt 4080) dataView
      w1 <- liftEffect $ V.getUint16be d0 dataView
      w2 <- liftEffect $ V.getUint16be d2 dataView
      [fromInt 15, fromInt 61440] `equal` [w1, w2]

-- 0---------1---------2---------3---------4
--  ____ ____ 0000 1111 1111 0000 ____ ____ = 4080
--  0000 0000 0000 1111 ____ ____ ____ ____ = 61440 in le
--  ____ ____ ____ ____ 1111 0000 0000 0000 = 15 in le
    test "setUint16le / getUint16le" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d4
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setUint16le d1 (fromInt 4080) dataView
      w1 <- liftEffect $ V.getUint16le d0 dataView
      w2 <- liftEffect $ V.getUint16le d2 dataView
      [fromInt 61440, fromInt 15] `equal` [w1, w2]

-- 0---------1---------2---------3---------4---------5---------6---------7
--  ____ ____ 0000 1000 0000 0100 0000 0010 0000 0001 ____ ____ ____ ____ = 134480385
--  0000 0000 0000 1000 0000 0100 0000 0010 ____ ____ ____ ____ ____ ____ = 525314
--  ____ ____ ____ ____ 0000 0100 0000 0010 0000 0001 0000 0000 ____ ____ = 67240192
    test "setInt32be / getInt32be" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d7
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setInt32be d1 134480385 dataView
      w1 <- liftEffect $ V.getInt32be d0 dataView
      w2 <- liftEffect $ V.getInt32be d2 dataView
      [525314, 67240192] `equal` [w1, w2]

-- 0---------1---------2---------3---------4---------5---------6---------7
--  ____ ____ 0000 1000 0000 0100 0000 0010 0000 0001 ____ ____ ____ ____ = 134480385
--  0000 0000 0000 1000 0000 0100 0000 0010 ____ ____ ____ ____ ____ ____ = 67240192 in le
--  ____ ____ ____ ____ 0000 0100 0000 0010 0000 0001 0000 0000 ____ ____ = 525314 in le
    test "setInt32le / getInt32le" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d6
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setInt32le d1 134480385 dataView
      w1 <- liftEffect $ V.getInt32le d0 dataView
      w2 <- liftEffect $ V.getInt32le d2 dataView
      [67240192, 525314] `equal` [w1, w2]

-- 0---------1---------2---------3---------4---------5---------6---------7
--  ____ ____ 0000 1000 0000 0100 0000 0010 0000 0001 ____ ____ ____ ____ = 134480385
--  0000 0000 0000 1000 0000 0100 0000 0010 ____ ____ ____ ____ ____ ____ = 525314
--  ____ ____ ____ ____ 0000 0100 0000 0010 0000 0001 0000 0000 ____ ____ = 67240192
    test "setUint32be / getUint32be" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d7
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setUint32be d1 (fromInt 134480385) dataView
      w1 <- liftEffect $ V.getUint32be d0 dataView
      w2 <- liftEffect $ V.getUint32be d2 dataView
      [fromInt 525314, fromInt 67240192] `equal` [w1, w2]

-- 0---------1---------2---------3---------4---------5---------6---------7
--  ____ ____ 0000 1000 0000 0100 0000 0010 0000 0001 ____ ____ ____ ____ = 134480385
--  0000 0000 0000 1000 0000 0100 0000 0010 ____ ____ ____ ____ ____ ____ = 67240192 in le
--  ____ ____ ____ ____ 0000 0100 0000 0010 0000 0001 0000 0000 ____ ____ = 525314 in le
    test "setUint32le / getUint32le" do
      arrayBuffer <- liftEffect $ mkArrayBuffer d6
      let dataView = V.viewWhole arrayBuffer
      liftEffect $ V.setUint32le d1 (fromInt 134480385) dataView
      w1 <- liftEffect $ V.getUint32le d0 dataView
      w2 <- liftEffect $ V.getUint32le d2 dataView
      [fromInt 67240192, fromInt 525314] `equal` [w1, w2]
