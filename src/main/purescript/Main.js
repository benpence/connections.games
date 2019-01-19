"use strict";

// module Main

exports.currentPath = function() {
  if (typeof window !== 'undefined') {
    return window.location.pathname;
  }
};
