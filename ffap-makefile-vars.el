;;; ffap-makefile-vars.el --- find file with makefile variables expanded

;; Copyright 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 5
;; Keywords: files, ffap, make
;; URL: http://user42.tuxfamily.org/ffap-makefile-vars/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-makefile-vars.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffap-makefile-vars.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code lets M-x ffap expand makefile macros $(FOO) in a
;; filename, like
;;
;;     PREFIX = /usr
;;
;;     $(PREFIX)/share/foo
;;
;; With point on the "$(PREFIX)/share/foo" an M-x ffap expands to offer
;; "/usr/share/foo".  This is good for constructed filenames in makefiles.
;;
;; Macros are expanded from the definitions in the file and also from
;; `process-environment' like "make" does.  There's no support for the
;; various smart expansions GNU make can do though.

;;; Install:

;; Put ffap-makefile-vars.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-makefile-vars))
;;
;; There's an autoload cookie below for this, if you know how to use
;; `update-file-autoloads' and friends.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - cooperate better with other advice on ffap-string-at-point
;; Version 3 - undo defadvice on unload-feature
;; Version 4 - express dependency on 'advice
;; Version 5 - new email

;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-makefile-vars))

;; Explicit dependency on advice.el since
;; `ffap-makefile-vars-unload-function' needs `ad-find-advice' macro when
;; running not byte compiled, and that macro is not autoloaded.
(require 'advice)


;;----------------------------------------------------------------------------
;; `replace-regexp-in-string' compatibility

;; [same in man-completion.el]
;;
(eval-and-compile ;; quieten emacs byte compiler
  ;; no `eval-when-compile' on this fboundp because in xemacs 21.4.22
  ;; easy-mmode.el (which is define-minor-mode etc) rudely defines a
  ;; replace-regexp-in-string, so a compile-time test is unreliable
  (if (fboundp 'replace-regexp-in-string)
      ;; emacs (21 up)
      (defalias 'ffap-makefile-vars--replace-regexp-in-string
        'replace-regexp-in-string)

    ;; xemacs21
    (defun ffap-makefile-vars--replace-regexp-in-string
      (regexp rep string fixedcase literal)
      "`replace-regexp-in-string' made available in xemacs.
The FIXEDCASE argument is ignored, case is always fixed."
      (replace-in-string string regexp rep literal))))

;;----------------------------------------------------------------------------

(defun ffap-makefile-vars-find (name)
  "Return the value of makefile macro/variable NAME.
A definition like \"FOO = xyz\" is sought in the current buffer.
If there's none the return is nil.  If there's more than one
definition the last is used, the same as \"make\" does.

Backslashed newlines for multi-line values are recognised.  Those
backslashes and newlines are collapsed out of the return, the
same as \"make\" does when it uses the value."

  ;; The first part of the regexp doesn't match a preceding backslashed
  ;; newline, ie. it's a non-backslash and a newline, or start of buffer and
  ;; a newline, or just start of buffer.  This protects against a "NAME="
  ;; within some unrelated multi-line value etc.  The rest is similar to a
  ;; comment with `makefile-macroassign-regex' in the way it matches
  ;; backslashes for multi-line values.
  ;;
  (save-excursion
    (goto-char (point-max))
    
    (and (let ((case-fold-search nil))
           (re-search-backward
            (concat "\\(?:\\(?:[^\\]\\|\\`\\)\n\\|\\`\\) *"
                    (regexp-quote name)
                    "[ \t*:+!?]*=[ \t]*\\(\\(.*\\\\\n\\)*.*\\)")
            nil t))
         (ffap-makefile-vars--replace-regexp-in-string
          "\\\\\n" "" (match-string 1) t t)))) ;; fixedcase and literal

(defun ffap-makefile-vars-substitute (str)
  "Return STR with makefile vars like $(FOO) substituted to their values.
Variable definitions are sought in the current buffer or in
`process-environment' and definitions are expanded recursively.

Variable definitions in the buffer are preferred over environment
values if both exist.  The \"make -e\" option reverses that, but
there's no equivalent here yet.

Values from the environment are treated the same as from the
buffer, so further $(BAR) forms are expanded recursively in them
too.  This is what \"make\" does, though it'd be unusual to have
$(BAR) forms in an environment variable.

Circular definitions like \"FOO=$(FOO)\" are noticed and
currently are left unexpanded.  Perhaps that will change (an
error, a warning, a special return ...).

Single-letter forms $X are not expanded, since for ffap they're
more likely to be a shell style variable name, and are a bit
unusual in makefiles anyway.  Perhaps this will change, but for
now write $(X) instead.

See `substitute-in-file-name' for similar expansion of shell
style environment variables."

  (let ((case-fold-search nil)
        (in-progress (list (cons nil str)))
        var-value
        circular
        name elem)
    (while str
      (let ((start 0))
        (while (string-match "\
\\(?:\\`\\|[^$]\\)\
\\(?:\\$\\$\\)*\
\\(\\$(\\([A-Za-z_][A-Za-z_0-9]*\\))\\)" str start)
          (setq name (match-string 2 str))
          (if (assoc name in-progress)
              (add-to-list 'circular name))

          (if (member name circular)
              ;; circular definition, skip
              (setq start (match-end 1))

            (if (setq elem (assoc name var-value))
                ;; have value for NAME
                (setq str (concat (substring str 0 (match-beginning 1))
                                  (cdr elem)
                                  (substring str (match-end 0))))

              ;; find NAME and recursively expand
              (push (cons name str) in-progress)
              (setq str (or (ffap-makefile-vars-find name)
                            (getenv name)
                            "")))
            (setq start 0))))

      (push (cons (caar in-progress) str) var-value)
      (setq str (cdr (pop in-progress))))

    (ffap-makefile-vars--replace-regexp-in-string
     "\\$\\$" "$" (cdar var-value) t t)))

;; This hack is applied to `ffap-string-at-point' instead of
;; `ffap-file-at-point' because the default `file' chars in
;; `ffap-string-at-point-mode-alist' don't include ( or ) so for instance on
;; "$(HOME)/foo.txt" it otherwise gives just "$".
;;
;; Run ad-do-it with "()" added, and check that the only "(" chars are "$(".
;; If there's any which are not "$(" then go a plain ad-do-it instead.  In
;; either case `ffap-makefile-vars-substitute' is applied to the return.
;;
(defadvice ffap-string-at-point (around ffap-makefile-vars activate)
  "Expand makefile forms $(FOO) in filenames."

  (let ((ffap-string-at-point-mode-alist
         (let ((elem (assq 'file ffap-string-at-point-mode-alist)))
           (cons (cons (car elem)
                       (cons (concat (cadr elem) "()/")
                             (cddr elem)))
                 ffap-string-at-point-mode-alist))))
    ad-do-it)
  (if ad-return-value
      ;; "(" other than "$(" is no good
      ;; ")" without "(" is no good
      (if (or (string-match "\\(\\`\\|[^$]\\)(" ad-return-value)
              (string-match "\\`[^(]*)" ad-return-value))
          ad-do-it))
  (if ad-return-value
      (setq ad-return-value
            (setq ffap-string-at-point
                  (ffap-makefile-vars-substitute ad-return-value)))))

(defun ffap-makefile-vars-unload-function ()
  "Remove defadvice from function `ffap-string-at-point'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-makefile-vars)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-makefile-vars)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: makefile makefiles usr filenames xyz Backslashed unexpanded
;; LocalWords: foo vars

(provide 'ffap-makefile-vars)

;;; ffap-makefile-vars.el ends here
