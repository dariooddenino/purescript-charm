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


exports._write = function (charm, string) {
  return function () {
    charm.write(string);
    return {};
  }
}

exports._setPosition = function (charm, x, y) {
  return function () {
    charm.position(x, y);
    return {};
  }
}

// Questa e' async!
exports._getPosition = function (charm, cb) {
  return function () {
    // @ TODO
  }
}

exports._move = function (charm, x, y) {
    return function () {
        charm.move(x, y);
        return {};
    }
}

exports._up = function (charm, y) {
  return function () {
    charm.up(y);
    return {};
  }
}

exports._down = function (charm, y) {
  return function () {
    charm.down(y);
    return {};
  }
}

exports._left = function (charm, x) {
  return function () {
    charm.left(x);
    return {};
  }
}

exports._right = function (charm, x) {
  return function () {
    charm.right(x);
    return {};
  }
}

// exports._push @TODO
// exports._pop @TODO

exports._erase = function (charm, string) {
  return function () {
    charm.erase(string);
    return {};
  }
}

exports._delete = function (charm, mode, n) {
  return function () {
    charm.delete(mode, n);
    return {};
  }
}

exports._insert = function (charm, mode, n) {
  return function () {
    charm.insert(mode, n);
    return {};
  }
}

exports._display = function (charm, attr) {
  return function () {
    charm.display(attr);
    return {};
  }
}

exports._foreground = function (charm, color) {
  return function () {
    if (!isNaN(color)) color = parseInt(color);
    charm.foreground(color);
    return {};
  }
}

exports._background = function (charm, color) {
  return function () {
    if (!isNaN(color)) color = parseInt(color);
    charm.background(color);
    return {};
  }
}

exports._cursor = function (charm, visible) {
  return function () {
    charm.cursor(visbile);
    return {};
  }
}

// @TODO removeAllListeners
// @TODO on
