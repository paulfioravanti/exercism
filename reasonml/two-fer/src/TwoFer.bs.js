// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_Option = require("bs-platform/lib/js/belt_Option.js");

function twoFer(name) {
  var companion = Belt_Option.getWithDefault(name, "you");
  return "One for " + (companion + ", one for me.");
}

exports.twoFer = twoFer;
/* No side effect */
