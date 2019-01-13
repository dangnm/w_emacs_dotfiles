;;; save-sexp.el --- save variables in files using setter forms like `setq'

;; Copyright (C) 2010-2013  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100902
;; Version: 0.2.0
;; Homepage: https://github.com/tarsius/save-sexp
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Persistently save the value of variables by replacing the textual
;; representations of the setter S-expression (e.g. `setq') used to
;; set it's value in some file.

;; This is similar to how Custom saves options and faces; but instead
;; of using exactly one instance of `custom-set-variables' to set the
;; values of all customized variables, this package saves each
;; variable separatly using an instance of some form that sets the
;; value of a variable, like e.g. `setq', `defvar' or `defconst'.

;;   (save-sexp-save-setq "~/file.el" 'variable)

;; This removes all top-level (setq variable ...) forms and inserts a
;; new `setq' form where the first match was removed, setting VARIABLE
;; to it's current value.

;; `save-sexp-save' can be used to replace any setter of the form:

;;   (SETTER '?VARIABLE VALUE [DOC-STRING])

;; See `save-sexp-save's doc-string for how ways to control
;; intentation etc. `save-sexp-save-setq' and similar functions can
;; also be used interactively.

;;; Todo:

;; - write more tests
;; - also remove different forms setting the same variable

;;; Code:

(require 'cl-lib)
(require 'find-func)

(defvar recentf-exclude)
(declare-function recentf-expand-file-name "recentf" (name))

(defconst save-sexp-use-current-value
  (make-symbol "save-sexp-use-current-value"))

(cl-defun save-sexp-save-setq
  (file-or-buffer variable
                  &optional wrap printer (value nil svalue))
  "Save the value of VARIABLE using a `setq' form.
Insert the `setq' form into FILE-OR-BUFFER.  If FILE-OR-BUFFER
is a file name then also save the file; otherwise it has to be a
buffer, nil for the current buffer.

With a prefix argument prompt for the value to save, otherwise
save the current value.

See `save-sexp-save' for details on the optional arguments."
  (interactive (save-sexp-read-args))
  (save-sexp-save file-or-buffer 'setq variable nil
                  wrap printer nil
                  (if svalue value save-sexp-use-current-value)))

(cl-defun save-sexp-save-defvar
  (file-or-buffer variable
                  &optional wrap printer doc-string (value nil svalue))
  "Save the value of VARIABLE using a `defvar' form.
Insert the `defvar' form into FILE-OR-BUFFER.  If FILE-OR-BUFFER
is a file name then also save the file; otherwise it has to be a
buffer, nil for the current buffer.

With a prefix argument prompt for the value to save, otherwise
save the current value.  Interactivley always save the doc-string.

See `save-sexp-save' for details on the optional arguments."
  (interactive (save-sexp-read-args t))
  (save-sexp-save file-or-buffer 'defvar variable nil
                  wrap printer
                  (or doc-string (called-interactively-p 'any))
                  (if svalue value save-sexp-use-current-value)))

(cl-defun save-sexp-save-defconst
  (file-or-buffer variable
                  &optional wrap printer doc-string (value nil svalue))
  "Save the value of VARIABLE using a `defconst' form.
Insert the `defconst' form into FILE-OR-BUFFER.  If FILE-OR-BUFFER
is a file name then also save the file; otherwise it has to be a
buffer, nil for the current buffer.

With a prefix argument prompt for the value to save, otherwise
save the current value.  Interactivley always save the doc-string.

See `save-sexp-save' for details on the optional arguments."
  (interactive (save-sexp-read-args t))
  (save-sexp-save file-or-buffer 'defconst variable nil
                  wrap printer
                  (or doc-string (called-interactively-p 'any))
                  (if svalue value save-sexp-use-current-value)))

(defun save-sexp-read-args (&optional use-default)
  (let* ((var (save-sexp-read-variable))
         f fob val)
    (when (and use-default
               (setq f (symbol-file var 'defvar))
               (string-match "\\.el\\(c\\)\\'" f))
      (setq f (find-library-name (substring f 0 (match-beginning 1)))))
    (setq fob (read-file-name "in file (empty for current buffer): "
                              (and f (file-name-directory f))
                              nil nil
                              (and f (file-name-nondirectory f))))
    (setq val save-sexp-use-current-value)
    (when (or current-prefix-arg (not (boundp var)))
      (setq val (read-from-minibuffer "using value: " nil nil t)))
    (list (if (eq fob "") nil fob) var val)))

(defun save-sexp-read-variable ()
  (let* ((enable-recursive-minibuffers t)
         (v (variable-at-point))
         (var (completing-read
               (if (symbolp v)
                   (format "Save variable (default %s): " v)
                 "Save variable: ")
               obarray
               (lambda (vv)
                 (or (get vv 'variable-documentation)
                     (and (boundp vv) (not (keywordp vv)))))
               t nil nil
               (if (symbolp v) (symbol-name v)))))
    (if (equal var "")
        (error "No variable selected")
      (intern var))))

(defmacro save-sexp-with-buffer (buffer &rest body)
  (declare (indent 1))
  (let ((old-mode (make-symbol "old-mode")))
    `(with-current-buffer
         (or ,buffer (current-buffer))
       (let ((standard-output (current-buffer))
             (inhibit-read-only t)
             ,old-mode)
         (unless (derived-mode-p 'emacs-lisp-mode)
           (setq ,old-mode major-mode)
           (emacs-lisp-mode))
         (prog1 (progn ,@body)
           (when ,old-mode
             (funcall ,old-mode)))))))

(defmacro save-sexp-with-file-or-buffer (file-or-buffer &rest body)
  (declare (indent 1))
  (let ((old-buffer (make-symbol "old-buffer"))
        (old-buffer-name (make-symbol "old-buffer-name"))
        (fob (make-symbol "file-or-buffer")))
    `(let ((,fob ,file-or-buffer))
       (if (or (not ,fob) (bufferp ,fob))
           (save-sexp-with-buffer ,fob
             ,@body)
         (let* ((recentf-exclude
                 (if recentf-mode
                     (cons (concat "\\`"
                                   (regexp-quote
                                    (recentf-expand-file-name ,fob))
                                   "\\'")
                           recentf-exclude)))
                (,old-buffer (find-buffer-visiting ,fob))
                ,old-buffer-name)
           (make-directory (file-name-directory ,fob) t)
           (save-sexp-with-buffer
               (let ((find-file-visit-truename t))
                 (or ,old-buffer (find-file-noselect ,fob)))
             (when ,old-buffer
               (setq ,old-buffer-name (buffer-file-name))
               (set-visited-file-name (file-chase-links ,fob)))
             (prog1 (progn ,@body)
               (let ((file-precious-flag t))
                 (save-buffer))
               (if ,old-buffer
                   (progn
                     (set-visited-file-name ,old-buffer-name)
                     (set-buffer-modified-p nil))
                 (kill-buffer (current-buffer))))))))))

(cl-defun save-sexp-save
  (file-or-buffer setter variable
                  &optional quote wrap printer doc-string
                  (value nil svalue))
  "Save the value of VARIABLE using a SETTER form.
Insert the SETTER form into FILE-OR-BUFFER.  If FILE-OR-BUFFER
is a file name then also save the file; otherwise it has to be a
buffer, nil for the current buffer.

The VARIABLE's value is quoted as needed and pretty-printed using
optional PRINTER or `pp-to-string' if that is nil.  If optional
QUOTE is non-nil a quote is inserted and expected before VARIABLE.

If optional WRAP is nil insert the value on a new line if and
only if the string contains at least one newline or if the
current line would become longer than `fill-column' when
inserting the string there.  If WRAP is t unconditionally insert
the string on a new line.  For any other non-nil value
unconditionally insert on the current line.

If optional VALUE is provided use that as VARIABLE's value;
otherwise it's current value.  If optional DOC-STRING is a string
insert that after the value; otherwise if it is nil don't insert
a doc-string; or if it is t insert it's current doc-string."
  (save-sexp-with-file-or-buffer file-or-buffer
    (save-sexp-prepare setter quote variable)
    (save-sexp-insert-value
     printer wrap
     (if (and svalue (not (eq value save-sexp-use-current-value)))
         value
       (symbol-value variable)))
    (when (eq doc-string t)
      (setq doc-string (documentation-property
                        variable 'variable-documentation t)))
    (when doc-string
      (princ "\n")
      (lisp-indent-line)
      (prin1 doc-string))))

(defun save-sexp-prepare (setter quote variable)
  "Prepare to set VARIABLE using SETTER.
Remove all matching S-expressions using `save-sexp-delete' from
the current buffer.  Then insert \"(SETTER VARIABLE)\" where the
first match was found.

When QUOTE is non-nil insert a quote before VARIABLE.  Point is
before the closing paren when this function returns.  The caller
should then insert VARIABLE's value and possibly it's doc-string
there."
  (let ((rep (save-sexp-delete
              (apply-partially 'save-sexp-search setter quote variable)))
        pos)
    (or (bobp)
        (and rep (not (car rep)))
        (princ "\n"))
    (princ (format "(%s %s%s" setter (if quote "'" "") variable))
    (setq pos (point))
    (princ ")\n")
    (or (eobp)
        (looking-at "\n")
        (and rep (not (cdr rep)))
        (insert "\n"))
    (goto-char pos)))

(defun save-sexp-search (setter quote variable)
  "Goto the next matching S-expression.
Find the next sexp of the form (SETTER VARIABLE ...) in the
current buffer, put point at position at the sexp's end and
return the position at it's the beginning.  If no matching
sexp is found don't touch point and return nil."
  (catch 'found
    (while t
      (while (forward-comment 1))
      (let ((beg (point))
            (sexp (condition-case nil
                      (read (current-buffer))
                    (end-of-file (throw 'found nil)))))
        (when (and (listp sexp)
                   (eq (car sexp) setter)
                   (eq (if quote (cadr (cadr sexp)) (cadr sexp)) variable))
          (forward-char)
          (throw 'found beg))))))

(defun save-sexp-delete (locator)
  "Remove matching S-expressions from the current buffer.
Remove all top-level sexps for which function LOCATOR returns
non-nil from the current buffer and move point to the end of the
first match.

If no match is found move point near the end of the buffer.  More
precisely if the buffer ends with a commend and/or a `provide'
form move before that, otherwise the very end of the buffer."
  (goto-char (point-min))
  (unless (eobp)
    (save-excursion (forward-sexp (buffer-size))))
  (let (first ret before after beg)
    (while (setq beg (funcall locator))
      (setq before (save-excursion (goto-char beg)
                                   (looking-back "\n\n" nil))
            after  (looking-at "\n"))
      (when (and before (not first))
        (cl-decf beg))
      (when (and after (or before (not first)))
        (forward-char))
      (delete-region beg (point))
      (unless first
        (setq first (point) ret (cons before after))))
    (if first
        (goto-char first)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (let ((pos (point)))
        (while (forward-comment -1)
          (or (bobp) (backward-char))
          (setq pos (point)))
        (goto-char pos)
        (when (save-excursion
                (backward-sexp)
                (eq (ignore-errors (car-safe (read (current-buffer))))
                    'provide))
          (backward-sexp)
          (or (bobp) (backward-char)))))
    ret))

(defun save-sexp-insert-value (printer wrap object)
  "Insert the pretty-printed representation of OBJECT using PRINTER.
Function PRINTER is called with OBJECT as only argument and has
to return it's pretty-printed representation as a string, which
is then inserted into the current buffer.

If WRAP is nil insert the string on a new line if and only if the
string contains at least one newline or if the current line would
become longer than `fill-column' when inserting the string there.
If WRAP is t unconditionally insert the string on a new line.
For any other non-nil value unconditionally insert on the current
line.

Finally indent the inserted text."
  (let ((string (funcall (or printer 'pp-to-string) object)))
    (when (string-match "[ \t\n\r]+\\'" string)
      (setq string (replace-match "" t t string)))
    (if (or (eq wrap t)
            (and (not wrap)
                 (or (string-match "\n" string)
                     (> (- (point) (line-beginning-position))
                        fill-column))))
        (insert "\n")
      (insert " "))
    (or (keywordp object)
        (booleanp object)
        (and (not (symbolp object))
             (not (listp object)))
        (insert "'"))
    (insert string)
    (let ((beg (scan-sexps (point) -1)))
      (indent-region beg (point))
      (whitespace-cleanup-region beg (point))
      (save-excursion (goto-char beg) (read (current-buffer))))))

(provide 'save-sexp)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; save-sexp.el ends here
