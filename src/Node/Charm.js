'use strict';

var Charm = require('charm');

exports._init = function (args) {
  if (args.length === 0) {
    return Charm(process);
  }
  return Charm.apply(Charm, [process]);
}

exports._reset = function (charm) {
    return function () {
        charm.reset();
        return {};
    }
}

exports._destroy = function (charm) {
  return function () {
    charm.destroy();
    return {};
  }
}

exports._end = function (charm) {
  return function () {
    charm.end();
    return {};
  }
}


exports._write = function (string, charm) {
  return function () {
    charm.write(string);
    return {};
  }
}

exports._setPosition = function (x, y, charm) {
  return function () {
    charm.position(x, y);
    return {};
  }
}

exports._getPosition = function (cb, charm) {
  return function () {
    charm.position(function (x, y) {
      cb(x)();
    })
  }
}

exports._move = function (x, y, charm) {
    return function () {
        charm.move(x, y);
        return {};
    }
}

exports._up = function (y, charm) {
  return function () {
    charm.up(y);
    return {};
  }
}

exports._down = function (y, charm) {
  return function () {
    charm.down(y);
    return {};
  }
}

exports._left = function (x, charm) {
  return function () {
    charm.left(x);
    return {};
  }
}

exports._right = function (x, charm) {
  return function () {
    charm.right(x);
    return {};
  }
}

exports._push = function (w, charm) {
  return function () {
    charm.push(w);
    return {};
  }
}

exports._pop = function (w, charm) {
  return function () {
    charm.push(w);
    return {};
  }
}

exports._erase = function (string, charm) {
  return function () {
    charm.erase(string);
    return {};
  }
}

exports._delete = function (mode, n, charm) {
  return function () {
    charm.delete(mode, n);
    return {};
  }
}

exports._insert = function (mode, n, charm) {
  return function () {
    charm.insert(mode, n);
    return {};
  }
}

exports._display = function (attr, charm) {
  return function () {
    charm.display(attr);
    return {};
  }
}

exports._foreground = function (color, charm) {
  return function () {
    if (!isNaN(color)) color = parseInt(color);
    charm.foreground(color);
    return {};
  }
}

exports._background = function (color, charm) {
  return function () {
    if (!isNaN(color)) color = parseInt(color);
    charm.background(color);
    return {};
  }
}

exports._cursor = function (visible, charm) {
  return function () {
    charm.cursor(visible);
    return {};
  }
}

// @TODO removeAllListeners
// @TODO on
