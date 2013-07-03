#|

 Extension Grammars

 Copyright (c) 2013, Thomas M. Hermann

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

(defmethod transform-grammar
    (ret ctx (in-meta (eql t)) (directive (eql :fixed-width)) &optional args)
  "\(:FIXED-WIDTH WIDTH FORM)

Apply the rule to a fixed width field.)"
  (with-unique-names (start end subctx val)
    `(let* ((,start (parser-context-cursor ,ctx))
            (,end (+ ,(first args) ,start)))
       (unless (< (parser-context-size ,ctx) ,end) 
         (let* ((,subctx
                 (create-parser-context
                  (subseq (parser-context-data ,ctx) ,start ,end)))
                (,val
                 ,(transform-grammar ret subctx t :and (rest args))))
           (when ,val
             (setf (parser-context-cursor ,ctx) ,end))
           ,val)))))
