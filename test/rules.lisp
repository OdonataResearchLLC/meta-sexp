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

;;; Utitlities

(defun test-float? (text)
  "Return the floating point number parsed from the text."
  (meta-sexp:float? (meta-sexp:create-parser-context text)))

(defun error-float? (exact text &optional epsilon)
  "Return the float error in number of epsilons."
  (/ (relative-error exact (test-float? text))
     (or epsilon single-float-epsilon)))

;;; Parse floating point numbers

(define-test float?
  "Test parsing basic floating point notation."
  (loop
   for (fp text) in
   '(( 0.123456789  ".123456789")
     (-0.123456789 "-.123456789")
     ( 1.23456789  "1.23456789")
     (-1.23456789 "-1.23456789")
     ( 12.3456789  "12.3456789")
     (-12.3456789 "-12.3456789")
     ( 123.456789  "123.456789")
     (-123.456789 "-123.456789")
     ( 1234.56789  "1234.56789")
     (-1234.56789 "-1234.56789")
     ( 12345.6789  "12345.6789")
     (-12345.6789 "-12345.6789")
     ( 123456.789  "123456.789")
     (-123456.789 "-123456.789")
     ( 1234567.89  "1234567.89")
     (-1234567.89 "-1234567.89")
     ( 12345678.9  "12345678.9")
     (-12345678.9 "-12345678.9"))
   do
   (assert-float-equal
    fp (test-float? text)
    text
    (error-float? fp text))))

(define-test scientific-float?
  "Test parsing scientific notation."
  (loop
   with fp+ =  1234.5678
   with fp- = -1234.5678
   for (text+ text-) in
   '((".12345678E+04" "-.12345678E+04")
     ("1.2345678E+03" "-1.2345678E+03")
     ("12.345678E+02" "-12.345678E+02")
     ("123.45678E+01" "-123.45678E+01")
     ("1234.5678E+00" "-1234.5678E+00")
     ("12345.678E-01" "-12345.678E-01")
     ("123456.78E-02" "-123456.78E-02")
     ("1234567.8E-03" "-1234567.8E-03")
     ("12345678E-04"  "-12345678E-04"))
   do
   (assert-float-equal
    fp+ (test-float? text+)
    text+
    (error-float? fp+ text+))
   (assert-float-equal
    fp- (test-float? text-)
    text-
    (error-float? fp- text-))))

(define-test single-float?
  "Test parsing single float scientific notation."
  (loop
   with fp+ =  1234.5678
   with fp- = -1234.5678
   for (text+ text-) in
   '((".12345678F+04" "-.12345678F+04")
     ("1.2345678F+03" "-1.2345678F+03")
     ("12.345678F+02" "-12.345678F+02")
     ("123.45678F+01" "-123.45678F+01")
     ("1234.5678F+00" "-1234.5678F+00")
     ("12345.678F-01" "-12345.678F-01")
     ("123456.78F-02" "-123456.78F-02")
     ("1234567.8F-03" "-1234567.8F-03")
     ("12345678F-04"  "-12345678F-04"))
   do
   (assert-float-equal
    fp+ (test-float? text+)
    text+
    (error-float? fp+ text+))
   (assert-float-equal
    fp- (test-float? text-)
    text-
    (error-float? fp- text-))))

(define-test double-float?
  "Test parsing double float scientific notation."
  (loop
   with fp+ =  1234.56789D0
   with fp- = -1234.56789D0
   for (text+ text-) in
   '((".123456789D+04" "-.123456789D+04")
     ("1.23456789D+03" "-1.23456789D+03")
     ("12.3456789D+02" "-12.3456789D+02")
     ("123.456789D+01" "-123.456789D+01")
     ("1234.56789D+00" "-1234.56789D+00")
     ("12345.6789D-01" "-12345.6789D-01")
     ("123456.789D-02" "-123456.789D-02")
     ("1234567.89D-03" "-1234567.89D-03")
     ("123456789D-04"  "-123456789D-04"))
   do
   (assert-float-equal
    fp+ (test-float? text+)
    text+
    (error-float? fp+ text+ double-float-epsilon))
   (assert-float-equal
    fp- (test-float? text-)
    text-
    (error-float? fp- text- double-float-epsilon))))
