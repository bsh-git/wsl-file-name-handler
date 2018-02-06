;;; wsl-file-name.el --- filename handler for "Windows Subsystem for Linux"  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Hiroyuki Bessho

;; Author: Hiroyuki Bessho <Bessho@Genetec.co.jp>
;; Keywords: files

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

;; 

;;; Code:


(defconst wsl-file-name-regexp "\\`/mnt/\\([[:alpha:]]\\)/"
  "regexp for windows file name seen from WSL.")

(defun wsl-file-name-to-w32 (name)
  "convert /mnt/x/foo to x:/foo"
  (if (string-match wsl-file-name-regexp name)
      (concat (match-string 1 name) ":/" (substring name (match-end 0)))
    name))

(defun wsl-file-name-handler (operation &rest args)
  (cond
   ((eq operation 'expand-file-name)
    (let ((name (car args))
	  (dir (and (cdr args) (cadr args)))
	  (inhibit-file-name-handlers (cons 'wsl-file-name-handler
					    (and (eq inhibit-file-name-operation operation)
						 inhibit-file-name-handlers))))
      (expand-file-name (wsl-file-name-to-w32 name)
			(and dir (wsl-file-name-to-w32 dir)))))
   ((eq operation 'unhandled-file-name-directory)
    (let ((name (car args)))
      (wsl-file-name-to-w32 name)))
   (t (let ((inhibit-file-name-handlers
	     (cons 'wsl-file-name-handler
		   (and (eq inhibit-file-name-operation operation)
			inhibit-file-name-handlers)))
	    (inhibit-file-name-operation operation))
	(apply operation args)))))




(defun wsl-enable-file-name-handler ()
  "Enable file name handler for WSL"
  (interactive)
  (let ((pair (assoc wsl-file-name-regexp file-name-handler-alist)))
    (if pair
	(setcdr pair 'wsl-file-name-handler)
      (push (cons wsl-file-name-regexp 'wsl-file-name-handler) file-name-handler-alist))))


(defun wsl-disable-file-name-handler ()
  "Disable file name handler for WSL"
  (interactive)
  (let ((pair (assoc wsl-file-name-regexp file-name-handler-alist)))
    (when pair
      (setq file-name-handler-alist (delq pair file-name-handler-alist)))))



(provide 'wsl-file-name)
;;; wsl-file-name.el ends here
