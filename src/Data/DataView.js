"use strict";

exports.viewWhole = function(arrayBuffer) {
  return new DataView(arrayBuffer);
}

exports._view = function(offset, length, arrayBuffer) {
  return new DataView(arrayBuffer, offset, length);
}

exports.buffer = function(dataView) {
  return dataView.buffer;
}

exports.get = function(type, offset, endianness, dataView) {
  return function() {
    return dataView["get" + type].call(dataView, offset, endianness);
  };
}

exports.set = function(type, offset, value, endianness, dataView) {
  return function() {
    return dataView["set" + type].call(dataView, offset, value, endianness);
  };
}
