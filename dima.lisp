(defpackage dima
  (:use :cl)
  (:export

   ;; for --map and the other anaphoric ones
   it
   it-index

   ;; for subp-with
   exit
   success
   failure
   stdout
   stderr

   nope
   pr
   prl
   while
   sha1
   vector-to-list
   json-switch
   -drop
   -take
   -contains-p
   --each
   --find
   --filter
   --remove
   --keep
   -map
   --map
   --map-indexed
   --partition; named `--separate' in Emacs Lisp
   thread-first
   thread-last
   f-join
   f-read
   f-read-bytes
   f-write
   f-mkdir
   f-exists-p
   f-expand
   concat
   s-truncate
   s-blank-p
   s-lines
   s-join
   s-chop-prefix
   s-chop-suffix
   s-starts-with-p
   s-ends-with-p
   s-replace
   s-trim
   s-upcase
   s-downcase
   subp-with
   slynk-kill-all-workers))
(in-package dima)

;;; misc

(defmacro nope (&rest body)
  "Ignore the passed body and do not evaluate anything."
  (declare (ignore body))
  nil)

(defun pr (&rest args)
  "Print ARGS readably, separated by spaces and followed by a newline.
  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.
  This is what `print' should have been. "
  (format t "~{~S~^ ~}~%" args)
  (finish-output)
  (first args))

(defmacro prl (&rest args)
  "Print ARGS labeled and readably.
  Each argument form will be printed, then evaluated and the result printed.
  One final newline will be printed after everything.
  Returns the last result.
  Examples:
    (let ((i 1)
          (l (list 1 2 3)))
      (prl i (second l)))
    ; =>
    i 1
    (second l) 2
  "
  `(prog1
       (progn ,@(mapcar (lambda (arg) `(pr ',arg ,arg)) args))
     (terpri)
     (finish-output)))

(defmacro while (test &body body)
  "If TEST yields non-nil, eval BODY... and repeat.

The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.

This can be used to break out of the while early.

    (return-from nil)

The returned value is always nil."
  `(do () ((not ,test))
     ,@body))

(defun sha1 (string)
  "Return a hex like string for STRING for SHA1."
  ;; (sha1 "hi")
  ;; (sha1 "⟰")
  (let ((digester (ironclad:make-digest :sha1)))
    (ironclad:update-digest digester
                            (sb-ext:string-to-octets string :external-format :UTF-8))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digester))))

(defun vector-to-list (vector)
  "Return VECTOR as list."
  (coerce vector 'list))

(defun json-switch (string-or-hash-table &optional prettyp)
  "Convert from or to string and hash table.

Note that this returns a vector if the JSON uses [].

Access via (access:access hash-table \"foo\")."
  (if (stringp string-or-hash-table)
      (shasht:read-json string-or-hash-table)
      (shasht:write-json* string-or-hash-table
                          :pretty prettyp
                          :stream nil)))

;;; dash.el

(defun -drop (n list)
  "Return the tail (not a COPY) of LIST without the first N items.

Return nil if LIST contains N items or fewer.
Return LIST if N is zero or less.

For another variant, see also `-drop-last'."
  (cond
    ((<= n 0) list)
    ((null list) nil)
    (t (-drop (1- n) (cdr list)))))

(defun -take (n list)
  "Return a copy of the first N items in LIST.
Return a copy of LIST if it contains N items or fewer.
Return nil if N is zero or less.

See also: `-take-last'."
  (cond
    ((<= n 0) list)
    ((null list) nil)
    (t (-drop (1- n) (cdr list)))))

(defun -contains-p (list element)
  "Return non-nil if LIST contains ELEMENT.

The test for equality is done with `equal' which works for string equality."
  (member element list :test #'equal))

(defmacro --each (list &rest body)
  "Evaluate BODY for each element of LIST and return nil.
Each element of LIST in turn is bound to `it' and its index  
within LIST to `it-index' before evaluating FORM.

Note that the arguments are switched compared to Emacs Lisp."
  `(let ((it-index 0))
     (dolist (it ,list)
       ,@body
       (incf it-index))))

(defmacro --find (form list)
  "Return the first item in LIST for which FORM evals to non-nil.
Return nil if no such element is found.  
Each element of LIST in turn is bound to `it' before evaluating FORM."
  `(loop for it in ,list
         thereis (and ,form it)))

(defmacro --filter (form list)
  "Return a new list of the items in LIST for which FORM evals to non-nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-filter'.
For the opposite operation, see also `--remove'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (push it ,r)))
       (nreverse ,r))))

(defmacro --remove (form list)
  "Return a new list of the items in LIST for which FORM evals to nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating FORM.
This is the anaphoric counterpart to `-remove'.
For the opposite operation, see also `--filter'."
  `(--filter (not ,form) ,list))

(defmacro --keep (form list)
  "Eval FORM for each item in LIST and return the non-nil results.

Like `--filter', but returns the non-nil results of FORM instead
of the corresponding elements of LIST.  Each element of LIST in
turn is bound to `it' and its index within LIST to `it-index'
before evaluating FORM.

This is the anaphoric counterpart to `-keep'."
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (--each ,list (let ((,m ,form)) (when ,m (push ,m ,r))))
       (nreverse ,r))))

(defun -map (fn list)
  "Apply FN to each item in LIST and return the list of results.

This function's anaphoric counterpart is `--map'."
  (mapcar fn list))

(defmacro --map (form list)
  "Eval FORM for each item in LIST and return the list of results.
Each element of LIST in turn is bound to `it' before evaluating
FORM.
This is the anaphoric counterpart to `-map'."
  `(mapcar
    (lambda (it) ,form)
    ,list))

(defmacro --map-indexed (form list)
  "Eval FORM for each item in LIST and return the list of results.
Each element of LIST in turn is bound to `it' and `it-index'
before evaluating FORM.
This is the anaphoric counterpart to `-map'."
  `(loop for it-index from 0
         for it in ,list
         collect ,form))

;; private
(defmacro !cons (car cdr)
  "Destructive: Set CDR to the cons of CAR and CDR."
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro --partition (form list)
  "Split LIST into two sublists based on whether items satisfy FORM.

Returns a list with 2 elements: first, the elements that satisfy FORM, second, the others
that do not.

`it' and `it-index' are set for each list iteration.

Note that this is named `--separate' in Emacs Lisp."
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each ,list (if ,form (!cons it ,y) (!cons it ,n)))
       (list (nreverse ,y) (nreverse ,n)))))

;;; thread macros

(defmacro thread-first (&rest forms)
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  `(arrow-macros:-> ,@forms))

(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  `(arrow-macros:->> ,@forms))

;;; strings

(defun concat (&rest strings)
  "Return a new string with all STRINGS concatenated."
  (apply #'str:concat strings))

(defun s-truncate (len s &optional (ellipsis "..."))
  "If S is longer than LEN, cut it down and add ELLIPSIS to the end.

The resulting string, including ellipsis, will be LEN characters long, 
but at least as long as ELLIPSIS."
  ;; (s-truncate 2 "hello")
  ;; (s-truncate 4 "hello")
  (if (> (length s) len)
      (format nil "~a~a"
              (subseq s 0 (max 0 (- len (length ellipsis))))
              ellipsis)
      s))

(defun s-blank-p (string)
  "Return true if STRING is nil or the empty string."
  (or (null string) (string= "" string)))

(defun s-lines (string)
  "Return a list of strings by splitting STRING on newline characters."
  (str:lines string))

(defun s-join (separator strings)
  "Return a new string with STRINGS joined with SEPARATOR.

SEPARATOR can be a string or the keyword :newline."
  (when (eq :newline separator)
    (setq separator "
"))
  (str:join separator strings))

(defun s-chop-prefix (prefix s)
  "Return a new string with PREFIX removed from S."
  (if (str:starts-with-p prefix s)
      (str:substring (length prefix) t s)
      s))

(defun s-chop-suffix (suffix s)
  "Return a new string with SUFFIX removed from S."
  (if (str:ends-with-p suffix s)
      (str:substring 0 (- (length suffix)) s)
      s))

(defun s-starts-with-p (prefix s)
  "Return true if S start with PREFIX."
  (str:starts-with-p prefix s))

(defun s-ends-with-p (suffix s)
  "Return true if S ends with SUFFIX."
  (str:ends-with-p suffix s))

(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (str:replace-all old new s))

(defun s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (str:trim s))

(defun s-upcase (s)
  "Return S in upper case."
  (str:upcase s))

(defun s-downcase (s)
  "Return S in lower case."
  (str:downcase s))

;;; files

(defun f-join (&rest paths)
  "Return a string of all PATHS joined via \"/\".

This implementation is not as smart as the one in f.el, but I think
for my use cases this is fine."
  ;; (f-join "foo" "bar/")
  ;; (f-join "foo" "bar/" "1.lisp")
  ;; (f-join "/oeu" "1.lisp")
  ;; (f-join "/foo/")
  (str:join
   "/"
   (--map-indexed
    (cond
      ((= 1 (length paths))
       (first paths))
      
      ((= 0 it-index)
       (s-chop-suffix "/" it))

      ((= (1- (length paths)) it-index)
       (s-chop-prefix"/" it))
      
      (t
       (thread-last it
                    (s-chop-suffix "/")
                    (s-chop-prefix "/"))))
    paths)))

(defun f-read (path)
  "Return the file content of PATH."
  (uiop:read-file-string path))

(defun f-read-bytes (path)
  "Return binary data as a byte vector from PATH."
  (with-open-file (stream path
                          :element-type '(unsigned-byte 8)
                          :direction :input)
    (let* ((length (file-length stream))
           (bytes (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun f-write (path content)
  "Write the CONTENT string to the file under PATH.

If the file already exists, it will be overwritten."
  (with-open-file
      (stream path
              :direction :output
              :if-exists :supersede)
    (write-string content stream)))

(defun f-mkdir (path)
  "Create the directory PATH and all missing parent directories.

Return true if the creation succeeded and nil otherwise."
  (unless (s-ends-with-p "/" path)
    (setq path (concat path "/")))
  (second (multiple-value-list (ensure-directories-exist path))))

(defun f-exists-p (path)
  "Return true when path exists."
  (not (null (probe-file path))))

(defun f-expand (path)
  "Return the expanded string path which expands ~."
  (if (string= "~" path)
      ;; handle case which would otherwise crash
      (s-chop-suffix "/" (uiop:native-namestring (truename "~/")))
      (uiop:native-namestring (truename path))))

;;; shell

(defmacro subp-with (command-list &rest body)
  "COMMAND is a list of strings which can have spaces in it.

Anaphoric bindings provided:
  exit: the exit code of the process
  success: t if process exited with exit code 0
  failure: t if process did not invoke or exited with a nonzero code
  stdout: output of stdout
  stderr: output of stderr"
  `(multiple-value-bind (output error-output exit-code)
       (uiop:run-program ,command-list
                         :ignore-error-status t
                         :output :string
                         :error-output :string)
     (let* ((exit exit-code)
            (success (= 0 exit-code))
            (failure (not success))
            (stdout output)
            (stderr error-output))
       (declare (ignorable exit success failure stdout stderr))
       ,@body)))

;;; sly/slynk

(defun slynk-kill-all-workers ()
  "Kill all workers."
  (mapc (lambda (thread)
          (when (string=
                 "slynk-worker"
                 (funcall (find-symbol "THREAD-NAME" 'slynk-backend) thread))
            (funcall (find-symbol "KILL-THREAD" 'slynk-backend) thread)))
        (funcall (find-symbol "ALL-THREADS" 'slynk-backend))))
