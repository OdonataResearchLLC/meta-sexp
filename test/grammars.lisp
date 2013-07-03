#|

 META Rules Unit Tests

 Copyright (c) 2013, Thomas M. Hermann
 All rights reserved.

 Permission is hereby granted, free  of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction,  including without limitation the rights
 to use, copy, modify,  merge,  publish,  distribute,  sublicense, and/or sell
 copies of the  Software,  and  to  permit  persons  to  whom  the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and  this  permission  notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED  "AS IS",  WITHOUT  WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT  NOT  LIMITED  TO  THE  WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT  HOLDERS  BE  LIABLE  FOR  ANY  CLAIM,  DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

|#

(in-package :meta-sexp-test)

;;; Utilities

(meta-sexp:defrule fixed-width-8? () ()
  (:fixed-width 8
   (:* (:type meta-sexp:space?))
   (:rule meta-sexp:float?)))

(meta-sexp:defrule fixed-width-10? () ()
  (:fixed-width 10
   (:* (:type meta-sexp:space?))
   (:rule meta-sexp:float?)))

;;; Tests

(define-test fixed-width
  "Test using the fixed-width grammar."
  (let ((ctx (meta-sexp:create-parser-context
              "1.23456712.3456789123.4567")))
    (assert-float-equal 1.234567 (fixed-width-8? ctx))
    (assert-eq 8 (meta-sexp::parser-context-cursor ctx))
    (assert-float-equal 12.345679 (fixed-width-10? ctx))
    (assert-eq 18 (meta-sexp::parser-context-cursor ctx))
    (assert-float-equal 123.4567 (fixed-width-8? ctx))
    (assert-eq 26 (meta-sexp::parser-context-cursor ctx)))
  ;; Embedded spaces
  (let ((ctx (meta-sexp:create-parser-context
              " 6.167E-04 4.552E-04-9.643E-04-3.370E-03")))
    (assert-float-equal  6.167E-04 (fixed-width-10? ctx))
    (assert-eq 10 (meta-sexp::parser-context-cursor ctx))
    (assert-float-equal  4.552E-04 (fixed-width-10? ctx))
    (assert-eq 20 (meta-sexp::parser-context-cursor ctx))
    (assert-float-equal -9.643E-04 (fixed-width-10? ctx))
    (assert-eq 30 (meta-sexp::parser-context-cursor ctx))
    (assert-float-equal -3.370E-03 (fixed-width-10? ctx))
    (assert-eq 40 (meta-sexp::parser-context-cursor ctx))))
