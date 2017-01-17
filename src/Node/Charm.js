'use strict';

var Charm = require('charm')

exports._init = Charm(process);

exports._reset = function (charm) {
    return function () {
        charm.reset();
        return {};
    }
}

exports._move = function (charm, x, y) {
    return function () {
        charm.move(x, y);
        return {};
    }
}

exports._background = function (charm, color) {
    return function () {
        charm.background(color);
        return {};
    }
}

exports._foreground = function (charm, color) {
    return function () {
        charm.foreground(color);
        return {};
    }
}

exports._write = function (charm, string) {
    return function () {
        charm.write(string);
        return {};
    }
}