;;; skewer-stylus.el --- Skewer support for live interaction with stylus -*- lexical-binding: t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Kristoffer Levin Hansen <kristoffer@levinhansen.dk>
;; Keywords: languages, tools
;; Version: "1.0.0"
;; URL: https://github.com/theunbound/skewer-stylus
;; Package-Requires: ((emacs "27.1") (skewer-mode "1.8.0"))

;;; Commentary:

;; This minor mode provides functionality for Stylus like plain skewer
;; does for JavaScript, or skewer-less for Less.

;; * C-x C-e -- ‘skewer-stylus-send-declaration’
;; * C-M-x   -- ‘skewer-stylus-send-rule’
;; * C-x C-k -- ‘skewer-stylus-eval-block’

;; Customization variables ‘skewer-stylus-block-start-regexp’ and
;; ‘skewer-stylus-block-end-regexp’ contain lists of rexexps that
;; identify the opening respective closing of a stylus block. By
;; default, they contain expressions that match opening respective
;; closing html style tags.

;; There is no support for Stylus funcitons or mixins. If you who are
;; reading the comments in the source files want to contribute code to
;; add that support, it would certainly be appreciated.

;; As the Stylus documentation says, “What if everything was
;; optional?” It makes parsing more difficult, is what. We try our
;; best to deal with the various ways that Stylus has of deliniating
;; rules, but the primary focus has been to support the style that has
;; neither semicolons nor curly-braces. The more complicated the
;; coding style of what you feed skewer-stylus, the more likely it is
;; to become confused. As always, there are no guarantees.

;; As the license file point out...

;;; Code:
(require 'skewer-css)

(define-error 'skewer-stylus-not-selector-error
  "Could not find extent of selector"
  'scan-error )

(defgroup skewer-stylus nil
  "Live updating of skewer styleheets."
  :group 'skewer )

(defcustom skewer-stylus-block-end-regexps
  '( "</[[:space:]]*style[^>]*>" )
  "A list of regular expressions that can identify the end of a \
block of stylus code in a buffer with mixed languages."
  :group 'skewer-stylus
  :type '(repeat string) )

(defcustom skewer-stylus-block-start-regexps
  '( "<[[:space:]]*style[^>]*>" )
  "A list of regular expressions that can identify the start of a \
block of stylus code in a buffer with mixed languages."
  :type '(repeat string) )

(defun skewer-stylus-regexp-or-block-end (regexp)
  "Add to REGEXP or-clauses for the block end regexps from the \
customization variable ‘skewer-stylus-block-end-regexps’.

If REGEXP is nil, return just the regexps in
‘skewer-stylus-block-end-regexps’ joined by or clauses."
  
  (string-join (if regexp
                   (nconc (list regexp) skewer-stylus-block-end-regexps)
                 skewer-stylus-block-end-regexps )
               "\\|" ) )

(defun skewer-stylus-regexp-or-block-start (regexp)
  "Add to REGEXP or-clauses for the block start regexps from the \
customization variable ‘skewer-stylus-block-start-regexps’.

If REGEXP is nil, return just the regexps in
‘skewer-stylus-block-start-regexps’ joined by or clauses."
  
  (string-join (if regexp
                   (nconc (list regexp) skewer-stylus-block-start-regexps)
                 skewer-stylus-block-start-regexps )
               "\\|" ) )

(defun skewer-stylus-blank-line-p ()
  "Test if the line at point contains only whitespace.

Returns as ‘string-match’."

  (save-match-data
    (string-match "^[[:space:]]*\n$" (thing-at-point 'line t)) ) )

(defun skewer-stylus-indent-level ()
  "Counts either tabs or spaces at the start of the current line.

If the current line is blank, return the indent level of the next
non-blank line. If there are no further non-blank lines, return
0."
  
  (save-excursion
    (cond
     ;; the indent level of a blank line will be the indent level of
     ;; the next non-blank line
     ((skewer-stylus-blank-line-p)
      (if (not (= 0 (forward-line 1)))
          0
        (skewer-stylus-indent-level) ) )
     (t
      (while
          (when (re-search-backward
                 (skewer-stylus-regexp-or-block-start "[^\\]\n") nil :move )
            (goto-char (match-end 0))
            (skewer-stylus-escape-enclosing :backward ?\( ?\[ ) ) )
      (if (memql (char-after) '( ?\C-i ?  ))
          (progn
            (re-search-forward (concat (char-to-string (char-after)) "+" )
                               (line-end-position) t)
            (- (match-end 0) (match-beginning 0)) )
        0 ) ) ) ) )


(defun skewer-stylus-rule-body-limits ()
  "Return the start and end position of the body of the rule that the point is \
currently in, as a 2-element list.

Declarations will be gathered with
‘skewer-stylus-declaration-limits'. The body will be deliniated
either by lines with a lower indentation than the one that point
is currently in, or a set of curly braces, whichever is
encountered first when scanning backwards from the current point."

  (save-excursion
    ;; locate limits of current declaration
    (let ((entry-end (cadr (cl-mapcar #'funcall
                                      '(goto-char identity)
                                      (skewer-stylus-declaration-limits) )))
          (target-indent (skewer-stylus-indent-level)) )
      
      (cl-labels
          ((body-inner
            ()
            (cond
             ((char-equal (or (char-before) ?\C-@ ) ?\} )
              (backward-list)
              (goto-char (car (skewer-stylus-declaration-limits)))
              (body-inner) )
             ;; step into previous declaration
             ((not (or (looking-back (skewer-stylus-regexp-or-block-start nil)
                                     nil )
                       (ignore-error beginning-of-buffer
                         (backward-char)
                         t ) ))
              ;; ...if there is one
              (end-rule-from-here) )
             ;; declaration was delimited by a {: eval to this {}
             ;; group, and surrounding spaces as appropriate
             ((char-equal (char-after) ?\{ )
              (list (if (looking-back "\n[[:space:]]*" nil)
                        (save-excursion
                          (while (looking-back "\n[[:space:]]*" nil)
                            (goto-char (match-beginning 0)) )
                          (1+ (point)) )
                      (point) )
                    (progn
                      (forward-list)
                      (when (looking-at "[[:space:]]*\n")
                        (goto-char (match-end 0)) )
                      (point) ) ) )
             ;; lower indentation: step forward and find the end
             ((< (skewer-stylus-indent-level) target-indent)
              (forward-char)
              (end-rule-from-here) )
             ;; keep looking
             (t
              (goto-char (car (skewer-stylus-declaration-limits)))
              (body-inner) ) ) )
           
           (end-rule-from-here
            ()
            (list
             (point)
             (progn
               (goto-char entry-end)
               (while
                   (and
                    (prog1
                        (>= (skewer-stylus-indent-level)
                            target-indent )
                      (when (char-equal (or (char-after) ?\C-@ ) ?\{ )
                        ;; skip over {}-groups, so long as
                        ;; the indentation looks right
                        (forward-list) ) )
                    (not
                     (= (point)
                        (goto-char
                         (cadr (skewer-stylus-declaration-limits)) ) ) ) ) )
               (point) ) ) ) )
        
        (body-inner) ) ) ) )


(defun skewer-stylus-escape-enclosing (&optional direction &rest paren-start-chars)
  "Escape comments, strings and, optionally, paren-groups, and eval to nil if \
point was not moved.

DIRECTION may be :backward or :forward. nil is equvalent to
:backward and non-nil is equivalent to :forward.
PAREN-START-CHARS should be the chars that open the paren-groups
to escape from. eg.  \\?{ \\?( .

‘syntax-ppss’ is used for finding whether a position is within a
comment, string or paren, and their start points."
  
  (let ((forwards (if (memq direction '(:backward :backwards)) nil direction)))
    (pcase (syntax-ppss)
      ;; syntax-ppss says we're in a commment or string
      ((seq _ _ _ _ comment _ _ _ (and (pred identity) start))
       (let ((end (save-excursion
                    (goto-char start)
                    (if comment
                        (search-forward "*/")
                      (re-search-forward
                       (concat "[^\\]"
                               (regexp-quote
                                (char-to-string (char-after)) ) ) ) ) )))
         (unless (memql (point) (list start end))
           (goto-char (if forwards end start)) ) ) )
      ;; syntax-ppss says we're in a parens. it might not
      ;; be the right type...
      ((and (guard paren-start-chars)
            (seq _ (pred identity) _ _ _ _ _ _ _ open-list) )
       (cl-labels ((is-paren (x)
                             ;; we get a list of positions,
                             ;; so check the char at this
                             ;; position
                             (memql (char-after x) paren-start-chars) )
                   (slice (x)
                          ;; find the longest sublist that
                          ;; contains only the positions of
                          ;; regular parentheses
                          (setq x (cl-member-if #'is-paren x))
                          (if (cl-every #'is-paren x)
                              x
                            (slice
                             (cl-member-if-not #'is-paren x) ) ) ) )
         (let ((first-par (car-safe (slice open-list))))
           (when first-par
             (goto-char first-par)
             (when forwards (forward-list) ) )
           first-par ) ) ) ) ) )

(defun skewer-stylus-declaration-limits ()
  "Return the beginning and end position of the declaration that \
the point is currently within, as a 2-element list.

A declaration extends from from the first instance of either a
newline, semicolon or curly bracket ( { or } ) encountered before
point to the first instance of same encountered after point, with
some exceptions:
- Escaped newlines (newlines preceeded by a backslash) will not
  delimit declarations.
- Newlines within parentheses ( ( ) ) will be skipped."
  
  (let ((delimiter (rx (any ?\; ?\{ ?\} ?\C-j ))))
    (save-excursion
      (list
       (progn
         (while
             (and (re-search-backward
                   (skewer-stylus-regexp-or-block-start delimiter)
                   nil 'move )
                  ;; include preceeding blank lines
                  (or (save-match-data (skewer-stylus-blank-line-p))
                      (save-match-data
                        (skewer-stylus-escape-enclosing :backward ?\( ) )
                      ;; don't get stuck between a semicolon and a newline
                      (save-match-data
                        (looking-at ";[[:space:]]*$") )
                      ;; keep going if the newline is escaped
                      (and (char-equal (char-after) ?\C-j )
                           (char-equal (char-before) ?\\ ) )
                      (progn
                        (goto-char (match-end 0))
                        nil ) ) ) )
         (point) )

       (progn
         (while
             (or (and (skewer-stylus-blank-line-p)
                      (= 0 (forward-line)) )
                 (when (re-search-forward
                        (skewer-stylus-regexp-or-block-end delimiter)
                        nil 'move )
                   (cond
                    ((save-match-data
                       (string-match "\n$" (match-string-no-properties 0)) )
                     (looking-back "\\\\\n" nil) ) ;escaped newline, try again
                    ((save-match-data
                       (string-match ";$" (match-string-no-properties 0)) )
                     (save-match-data
                       ;; also include remaining line after ; if empty
                       (when (looking-at "[[:space:]]*\n")
                         (goto-char (match-end 0)) )
                       nil ) )      ;don't leave the or list
                    (t (goto-char (match-beginning 0))
                       nil ) )      ;don't leave the or list
                   )
                 (skewer-stylus-escape-enclosing :forward ?\( ) ) )
         (point) ) ) ) ) )


(defun skewer-stylus-selector-limits ()
  "Return the start and emd point of the selector that the point \
is currently within as a 2-element list.

The last line of a selector can contain any text, so long as it
is followed by lines of greater indentation, or the same
indentation but starting with an opening curly brace. Preceeding
lines are included if they have the same indentation level as the
start line, and look like a selector, that is no space seperated
character blocks, or a closing comma (see [Stylus/selectors]), or
if they are empty. If this process comes across a line that has a
closing curly brace, it will consider the character just after it
the first character of the selector.

[Stylus/selectors](URL ‘https://stylus-lang.com/docs/selectors.html’)"
  (pcase
      (save-excursion
        ;; escape comments, string and then square brackets
        (skewer-stylus-escape-enclosing :backward ?\( ?\[ )
        (let ((target-indent (skewer-stylus-indent-level)))
          (reverse
           (list
            (catch 'wall
              (while
                  (or (cond ((and (not (char-equal (or (char-before) ?\C-@ ) ?\\ ))
                                  (char-equal (char-after) ?\C-j ) )
                             (forward-char) ) ;returns nil: next check
                            ((re-search-forward
                              (skewer-stylus-regexp-or-block-end "{\\|[^\\]\n")
                              nil 'move )
                             nil )            ;next check
                            (t
                             (throw 'wall (point)) ) ) ;end of buffer: stop.
                      (save-match-data
                        ;; escape comments or strings.
                        ;; nil if nothing to escape: next check
                        ;; non-nil if point moved: start over
                        (skewer-stylus-escape-enclosing :forward) )
                      (when (save-match-data
                              (string-match (skewer-stylus-regexp-or-block-end nil)
                                            (match-string 0) ) )
                        ;; block end pattern: stand at start...
                        (goto-char (match-beginning 0))
                        (throw 'wall (point)) ) ;and stop
                      ;; escape parenthetical groups. if we were in
                      ;; one, start over
                      (skewer-stylus-escape-enclosing :forward ?\( ?\[ )
                      (when (char-equal (char-before) ?\{ )
                        ;; stand before opening { and stop
                        (backward-char) ) ;evals to nil
                      (and (char-equal (char-before) ?\C-j )
                           (pcase (skewer-stylus-indent-level)
                             ((pred (> target-indent))
                              ;; can't have been a valid selector, then
                              (signal 'skewer-stylus-not-selector-error (list (point))) )
                             ((pred (= target-indent))
                              ;; if the rule after the selector begins
                              ;; with a {, stop here by evaluating to
                              ;; nil. otherwise, the selector
                              ;; continues.
                              (not (looking-at "[[:space:]\n]*{")) )
                             ((pred (< target-indent))
                              ;; selector ends, eval to nil
                              nil ) ) ) ) )
              (point) )

            ;; now search backward for selector start
            (cl-labels
                ((goto-opener
                  ()
                  (while
                      (and
                       ;; initial delimiter might be { or newline or script start
                       (re-search-backward
                        (skewer-stylus-regexp-or-block-start "[{}\n]")
                        nil :move) ;nil if stop
                       ;; escape comments, strings and []- and ()-blocks
                       (save-match-data
                         (if
                             (or
                              ;; if we escaped something...
                              (skewer-stylus-escape-enclosing :backward ?\[ ?\( )
                              ;; or we're at an escaped newline...
                              (and (char-equal (or (char-before) ?\C-@ ) ?\\ )
                                   (char-equal (char-after) ?\C-j ) ) )
                             t          ;start over
                           ;; else stand after the delimiter
                           (goto-char (match-end 0))
                           nil ) ) ) )  ;and end the loop
                  (point) )
                 
                 (preceeding-line-start-if-valid
                  ()
                  (save-excursion
                    (when
                        (and
                         (when (char-equal (or (char-before) ?\C-@ ) ?\C-j )
                           (backward-char)
                           t )
                         (string-match
                          (rx bol
                              (* space)
                              (? (+ (not space))
                                 (? space (* anything) ?\, )
                                 (* space) )
                              eol )
                          (let ((potential-string (buffer-substring-no-properties
                                                   (point)
                                                   (goto-opener) )))
                            (while       ;remove strings, comments and parentheses
                                (string-match
                                 (rx (eval
                                      (nconc
                                       '(or
                                         (seq ?\\ ?  )          ;escaped newline
                                         (seq "/*" (*? anything) "*/" ) ) ;comments
                                       (mapcar (lambda (esc-del) ;strings
                                                 `(seq ,esc-del
                                                       (or ,esc-del
                                                           (seq (*? anything)
                                                                (not ?\\ )
                                                                ,esc-del ) ) ) )
                                               '( ?\" ?\' ) )
                                       (mapcar (lambda (del-set) ;parenthetical grups
                                                 `(seq ,(car del-set)
                                                       (*? (not ,(car del-set)))
                                                       ,(cadr del-set) ) )
                                               '(( ?\{ ?\} )
                                                 ( ?\( ?\) )
                                                 ( ?\[ ?\] ) ) ) ) ))
                                 potential-string )
                              (setq potential-string
                                    (replace-match "" nil nil
                                                   potential-string )) )
                            potential-string ) )
                         (= (skewer-stylus-indent-level) target-indent) )
                      (point) ) ) ) )
                        
              (when (char-equal (or (char-before) ?\C-@ ) ?\C-j )
                (backward-char) )
              (unless (string-match
                       ;; the last line of the selector only needs to
                       ;; contain a non-space character
                       "[^[:space:]\n]"
                       (buffer-substring-no-properties (point)
                                                       (goto-opener) ) )
                (signal 'skewer-stylus-not-selector-error (list (point))) )
              
              (while
                  (not (= (point)
                          (goto-char
                           (or (preceeding-line-start-if-valid)
                               (point) ) ) ) ) )
              (point) ) ) ) ) )

    ;; pcase:
    ((seq (and selector-start
               (pred (< (point))) ))
     ;; point was not inside a selector
     (signal 'skewer-stylus-not-selector-error (list (point) selector-start)) )
    (range range) ) )

        
(defun skewer-stylus-wrap-in-selectors (center &optional ranges)
  "Return the string CENTER prepended with the set of selectors \
active at point, or the selectors given in RANGES, and appended \
with closing braces for any opening braces associated with \
those selectors."

  (setq ranges (cond ((not ranges)
                      (skewer-stylus-gather-ranges) )
                     (t ranges) ))
  (let* (;(ranges (skewer-stylus-gather-ranges))
         (openers (skewer-stylus-selector-ranges-braces-list ranges))
         (closers (replace-regexp-in-string "{"
                                            "}"
                                            (apply #'concat openers) )) )
    (concat
     (mapconcat
      (lambda (el)
        (concat (apply #'buffer-substring-no-properties el)
                (pop openers) ) )
      ranges
      "" )
     center
     closers ) ) )


(defun skewer-stylus-extract-declaration ()
  "Return the declaration on the line at point, along with the \
set of selectors that apply to it.

The returned string should be ready to be processed by the stylus
executable."
  
  (skewer-stylus-wrap-in-selectors
   (apply #'buffer-substring-no-properties
          (skewer-stylus-declaration-limits) ) ) )


(defun skewer-stylus-extract-rule ()
  "Return the rule at point, along with the set of selectors \
that apply to it.

The returned string should be ready to be processed by the stylus
executable."
  
  (skewer-stylus-wrap-in-selectors
   (apply #'buffer-substring-no-properties
          (skewer-stylus-rule-body-limits) ) ) )

  
(defun skewer-stylus-gather-ranges ()
  "Return a list of lists of start and end points for the \
selectors that apply to the rule at point, as found by
‘skewer-stylus-selector-limits’.

See ‘skewer-stylus-rule-body-limits’ for information on how the
nearest applicable selector is located at each step."
  (save-excursion
    (let ((ratchet most-positive-fixnum))
      (reverse
       (cl-loop
        for range
        = (let ((this-range
                 (progn
                   (goto-char (car (skewer-stylus-rule-body-limits)))
                   (while (memql (char-before) '( ?\C-j ?\{ ?   ))
                     (backward-char) )
                   (ignore-error skewer-stylus-not-selector-error
                     (skewer-stylus-selector-limits) ) ) ))
            (when this-range
              (goto-char (car this-range))
              (when (> ratchet (setq ratchet (skewer-stylus-indent-level)))
                (when (char-equal (or (char-before) ?\C-@ ) ?\} )
                  (backward-char) )
                this-range ) ) )
        while range
        collect range ) ) ) ) )
    

(defun skewer-stylus-selector-ranges-braces-list (selector-ranges)
  "Given a list of lists of start and end points for selectors \
SELECTOR-RANGES, such as is returned by \
‘skewer-stylus-gather-ranges’, return a list of the same length \
containing an opening curly brace '{' if the selector is followed \
by one in the buffer, otherwise nil."
  (mapcar
   (lambda (el)
     (save-excursion
       (goto-char (cadr el))
       (when (char-equal (char-after) ?\{ )
         (forward-char)
         (when (looking-back "\\(?:\n[[:space:]]*\\)?{" nil)
           (buffer-substring-no-properties (match-beginning 0)
                                         (match-end 0) ) ) ) ) )
   selector-ranges ) )

(defun skewer-stylus-eval (string)
  "Evaluate STRING with Stylus, and send the result with ‘skewer-css’.

The stylus command must be available in emacs’ environment. It is
called with ‘call-process-region’, and the result is held in a
temporary buffer, which is sent to skewer with
‘skewer-css-eval-buffer’."

  (let ((stylus-buffer (get-buffer-create "**Stylus**")))
    (print (= 0 (call-process-region string nil "stylus" nil stylus-buffer)))
    (with-current-buffer stylus-buffer
      (skewer-css-mode)
      (skewer-css-eval-buffer)
      (kill-buffer) ) ) )

(defun skewer-stylus-enter-rule-body ()
  "Move the point inside the corresponding rule body if the point is \
in a selector.

If the point isn't in a selector, it is not moved.

Returns new point if point was moved, otherwise nil."

  (ignore-error skewer-stylus-not-selector-error
    (goto-char (cadr (skewer-stylus-selector-limits)))) )

(defun skewer-stylus-send-rule-for-body-func (body-func)
  "Send the rule body found by BODY-FUNC, along with selectors that apply \
at point, through skewer.

BODY-FUND should eval to a 2-element list containing the first
and last buffer posiitons of the rule body to be sent. The list
of active selectors is worked out after BODY-FUNC is evaluated,
meaning that if BODY-FUNC moves point, the new point is taken as
the basis for finding active selectors.

The regions being sent are marked with ‘skewer-flash-region’."
  
  (let* ((body (funcall body-func))
         (ranges (skewer-stylus-gather-ranges)) )

    (mapc (lambda (r) (apply #'skewer-flash-region r))
          (cons body ranges) )
    (sit-for .1)                        ;wait for overlay to happen
    (skewer-stylus-eval (skewer-stylus-wrap-in-selectors
                         (apply #'buffer-substring-no-properties body)
                         ranges)) ) )

(defun skewer-stylus-send-rule ()
  "Evaluate the entire rule around point, and send it through skewer.

If point is in a selector, the current rule is everything that
that selector applies to. Otherwise, the rule around point is
everything at the same indentation level as the line at point or
higher, or the {}-group that point is within, whichever is found
first moving backwards from point."

(interactive)
  (save-excursion
    (skewer-stylus-send-rule-for-body-func
     (lambda ()
       (skewer-stylus-enter-rule-body)
       (skewer-stylus-rule-body-limits) ) ) ) )

(defun skewer-stylus-send-declaration ()
  "Evaluate the declaration at point, and send it and its relevant \
selectors through skewer.

Declaratios are delimited either by unescaped newlines,
semicolons or curly braces, except if they occur within strings,
commments or parentheses ( () )."

  (interactive)
  (skewer-stylus-send-rule-for-body-func #'skewer-stylus-declaration-limits) )

(defun skewer-stylus-eval-block ()
  "Evaluate the entire stylus block at point, and send through skewer.

The current block begins either after the nearst text block that
matches one of the regexps in ‘skewer-stylus-block-start-regexps’
or the beginning of buffer, and ends with the nearest text block
that matches one of the regexps in
‘skewer-stylus-block-end-regexps’ or the end of the buffer."

  (interactive)
  (save-excursion
    (skewer-stylus-eval (buffer-substring-no-properties
                         (if (re-search-backward
                              (skewer-stylus-regexp-or-block-start nil)
                              nil t )
                             (goto-char (match-end 0))
                           (buffer-end -1) )
                         (if (re-search-backward
                              (skewer-stylus-regexp-or-block-end nil)
                              nil t )
                             (match-beginning 0)
                           (buffer-end 1) ) )) ) )

(defvar skewer-stylus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") 'skewer-stylus-send-declaration)
    (define-key map (kbd "C-M-x") 'skewer-stylus-send-rule)
    (define-key map (kbd "C-c C-k") 'skewer-stylus-eval-block)
    (define-key map (kbd "C-c C-c") 'skewer-css-clear-all)
    map )
  "Keymap for skewer-stylus-mode." )

;;;###autoload
(define-minor-mode skewer-stylus-mode
  "Minor mode for interactively loading new Stylus rules."
  :lighter " skewer-stylus"
  :keymap skewer-stylus-mode-map
  :group 'skewer )

(provide 'skewer-stylus)

;;; skewer-stylus.el ends here
