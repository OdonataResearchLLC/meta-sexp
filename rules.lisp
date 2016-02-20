#|

 Common META Rules

 Copyright (c) 2010-2016, Thomas M. Hermann

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

(in-package :meta-sexp)

(defrule sign? (&aux (sign 1)) ()
  (:? (:or (:and "-" (:assign sign -1)) "+"))
  (:return sign))

;;; Parse an integer
(defrule integer? (&aux (sign 1) digit (num 0)) ()
  (:assign sign (:rule sign?))
  (:+ (:assign digit (:type digit?))
      (:assign num (+ (* num 10)
                      (- (char-code digit)
                         #.(char-code #\0)))))
  (:return (* sign num)))

;;; Parse a floating point number
(defrule decimal? (&aux (decimal 0) (dexp 0) digit) ()
  (:+ (:assign digit (:type digit?))
      (:assign decimal (+ (* decimal 10)
                          (- (char-code digit) #.(char-code #\0))))
      (:assign dexp (1- dexp)))
  (:return decimal dexp))

(defrule exponent? (&aux sign float-format exponent) ()
  (:or
   (:and (:or "e" "E") (:assign float-format *read-default-float-format*))
   (:and (:or "s" "S") (:assign float-format 'short-float))
   (:and (:or "f" "F") (:assign float-format 'single-float))
   (:and (:or "d" "D") (:assign float-format 'double-float))
   (:and (:or "l" "L") (:assign float-format 'long-float)))
  (:assign sign (:rule sign?))
  (:assign exponent (:rule decimal?))
  (:return (* sign exponent) float-format))

;;; [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent]
(defrule float-token-a? (&aux icomp iexp fcomp fexp exponent format) ()
  (:? (:assign (icomp iexp) (:rule decimal?)))
  "."
  (:assign (fcomp fexp) (:rule decimal?))
  (:? (:assign (exponent format) (:rule exponent?)))
  (:return (or icomp 0) (1+ (or iexp 0))
           (or fcomp 0) (or fexp 0)
           (or exponent 0) (or format *read-default-float-format*)))

;;; [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
(defrule float-token-b? (&aux icomp iexp fcomp fexp exponent format) ()
  (:assign (icomp iexp) (:rule decimal?))
  (:? (:and "." (:? (:assign (fcomp fexp) (:rule decimal?)))))
  (:assign (exponent format) (:rule exponent?))
  (:return (or icomp 0)  (1+ (or iexp 0))
           (or fcomp 0) (or fexp 0)
           (or exponent 0) (or format *read-default-float-format*)))

(defrule float? (&aux sign icomp iexp fcomp fexp exponent format) ()
  (:assign sign (:rule sign?))
  (:or
   (:assign (icomp iexp fcomp fexp exponent format)
            (:rule float-token-a?))
   (:assign (icomp iexp fcomp fexp exponent format)
            (:rule float-token-b?)))
  (:return (let ((ten (coerce 10 format)))
             (* sign
                (+ icomp (* fcomp (expt ten fexp)))
                (expt ten exponent)))))

(defrule number? () ()
  ;; NOTE : It is important to read the float first
  (:rule (or float? integer?)))

;;; Line rules

(defrule blank-line? () ()
  (:* (:type white-space?))
  (:type newline?))

(defrule skip-line? () ()
  (:* (:type (or white-space? graphic?)))
  (:type newline?))

(defrule echo-skip-line? (&aux chr (line (make-char-accum))) ()
  (:*
   (:assign chr (:type (or white-space? graphic?)))
   (:char-push chr line))
  (:type newline?)
  (:return line))

(defrule eol? () ()
  (:* (:type white-space?))
  (:type newline?))

;;; End of file

(defrule eof? () ()
  (:* (:type (or white-space? newline?)))
  (:eof))
