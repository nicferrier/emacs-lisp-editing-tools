;;; lisp-editing.el --- lisp editing tools

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier(defun lisp-reinsert-as-pp () <nferrier@ferrier.me.uk>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some lisp editing tools.

;;; Code:

(defun lisp-reinsert-as-pp ()
  "Read sexp at point, delete it and pretty print it back in."
  (interactive)
  (let* ((buf (current-buffer))
         (pp-sexp
          (replace-regexp-in-string
           "\\(\n\\)$"
           ""
           (with-temp-buffer
             (let ((bufname (buffer-name)))
               (pp-display-expression
                (with-current-buffer buf
                  (car
                   (read-from-string
                    (replace-regexp-in-string
                     "\\*\\(.*?\\)\\*\\(<[0-9]+>\\)* <[0-9:.]+>"
                     "\"\\&\""
                     (save-excursion
                       (buffer-substring-no-properties
                        (point)
                        (progn
                          (forward-sexp)
                          (point))))))))
                bufname)
               (buffer-substring (point-min) (point-max)))))))
    (kill-sexp)
    (insert pp-sexp)))

(provide 'lisp-editing)

;;; lisp-editing.el ends here
