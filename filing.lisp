#!/usr/bin/sbcl --script

(load "~/.sbclrc")

(require :quicklisp)
(ql:quickload "cl-charms")



(load "src/package")
(load "src/curses")

(require :sick-filing)

(sick-filing:main)
