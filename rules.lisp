#|

 Common META Rules

 Copyright (c) 2010, Thomas M. Hermann
 All rights reserved.

 Redistribution and  use  in  source  and  binary  forms, with or without
 modification, are permitted  provided  that the following conditions are
 met:

   o  Redistributions of  source  code  must  retain  the above copyright
      notice, this list of conditions and the following disclaimer.
   o  Redistributions in binary  form  must reproduce the above copyright
      notice, this list of  conditions  and  the  following disclaimer in
      the  documentation  and/or   other   materials  provided  with  the
      distribution.
   o  The names of the contributors may not be used to endorse or promote
      products derived from this software without  specific prior written
      permission.

 THIS SOFTWARE IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS AND CONTRIBUTORS
 "AS IS"  AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT LIMITED TO,
 PROCUREMENT OF  SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE, DATA, OR
 PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER  CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER  IN  CONTRACT,  STRICT  LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR  OTHERWISE)  ARISING  IN  ANY  WAY  OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

;;; NOTE : It is important to read the float first
(defrule number? () ()
  (:rule (or float? integer?)))

;;; Skip a line
(defrule skip-line? () ()
  (:* (:type (or white-space? graphic?)))
  (:type newline?))

;;; End of file
(defrule eof? () ()
  (:* (:type (or white-space? newline?)))
  (:eof))

;;; End of line
(defrule eol? () ()
  (:* (:type white-space?))
  (:type newline?))
