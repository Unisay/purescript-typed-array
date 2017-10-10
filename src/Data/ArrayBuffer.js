"use strict";

exports._arrayBuffer = function(n) {
  return function() {
    return new ArrayBuffer(n);
  };
}

exports._slice = function(beginByte, endByte, arrayBuffer) {
  return function(){
    return arrayBuffer.slice(beginByte, endByte);
  };
}

exports.isView = function(arrayBuffer) {
  return arrayBuffer.isView;
}
