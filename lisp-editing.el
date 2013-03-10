;;; lisp-editing.el --- lisp editing tools

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier  <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier  <nferrier@ferrier.me.uk>
;; Created: 18th August 2012
;; Keywords: lisp
;; Version: 0.0.5

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

(defun lisp-package-time-now ()
  "Produce the current time in a package friendly format.

If called interactively it inserts at point."
  (interactive)
  (let ((time-str
         (format-time-string
          (format
           "%%d%s %%B %%Y"
           (let ((day (string-to-number (format-time-string "%d"))))
             (if
              (or (and (<= 4 day) (<= day 20))
                  (and (<= 24 day) (<= day 30)))
              "th"
              ;; Else one of the others
              (elt ["st" "nd" "rd"] (- (% day 10) 1))))))))
    (if (called-interactively-p 'interactive)
        (insert time-str)
        time-str)))


(defun lisp-lexbind-modeline ()
  "Add LEX to the modeline."
  ;; not sure about this: mode-line-modes is a cust var
  ;; we're breaking customization by doing this
  (interactive)
  (add-to-list
   'mode-line-modes
   '(:eval
     (when lexical-binding "LEX"))))

(defun lisp-lexscratch (&optional other-window)
  "Make a lexical scratch buffer."
  (interactive "P")
  (let ((buf (get-buffer "*lexscratch*")))
    (unless buf
      (setq buf (get-buffer-create "*lexscratch*"))
      (with-current-buffer buf
        (lisp-interaction-mode)
        (setq lexical-binding t)
        (insert initial-scratch-message)))
    (if other-window
        (switch-to-buffer-other-window buf)
        (switch-to-buffer buf))))

;;;###autoload
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

;;;###autoload
(defun lisp-load-all (filename)
  "Load all the files in a package.

Takes a filename that specifies all the files in a package."
  (interactive "Gfile list:")
  (flet ((map-regex (buffer regex fn)
           "Map the REGEX over the BUFFER executing FN.

FN is called with the match-data of the regex.

Returns the results of the FN as a list."
           (with-current-buffer buffer
             (save-excursion
               (goto-char (point-min))
               (let (res)
                 (save-match-data
                   (while (re-search-forward regex nil t)
                     (let ((f (match-data)))
                       (setq res
                             (append res
                                     (list
                                      (save-match-data
                                        (funcall fn f))))))))
                 res)))))
    (map-regex
     (find-file-noselect
      (cond
        ((and filename
              (file-exists-p filename)
              (file-directory-p filename))
         (concat (file-name-as-directory filename) "build-parts.txt"))
        ((file-exists-p filename)
         filename)))
     "^\\(.*.el\\(\\.gz\\)*\\)$"
     (lambda (md)
       (let ((filename (match-string 0)))
         (message "loading %s" filename)
         (when (file-exists-p filename)
           (load-file filename)))))))


(provide 'lisp-editing)

;;; lisp-editing.el ends here
