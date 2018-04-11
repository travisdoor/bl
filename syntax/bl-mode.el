;;; bl-mode-el -- Major mode for editing BL files

;; Author: Scott Andrew Borton <scott@pp.htv.fi>
;; Created: 25 Sep 2000
;; Keywords: BL major-mode

;; Copyright (C) 2000, 2003 Scott Andrew Borton <scott@pp.htv.fi>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;; 
;; This mode is an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar bl-mode-hook nil)
(defvar bl-mode-map
  (let ((bl-mode-map (make-keymap)))
    (define-key bl-mode-map "\C-j" 'newline-and-indent)
    bl-mode-map)
  "Keymap for BL major mode")

(add-to-list 'auto-mode-alist '("\\.bl\\'" . bl-mode))

(defconst bl-font-lock-keywords-1
  (list
   '("\\<\\(const\\|var\\|if\\|while\\|loop\\|break\\|continue\\|else\\|extern\\|module\\|public\\|struct\\|enum\\|fn\\|return\\)\\>" . font-lock-keyword-face)
   '("\\<\\(i8\\|i32\\|i64\\|u8\\|u32\\|u64\\|f32\\|f64\\|string\\|char\\|ptr\\)\\>" . font-lock-type-face)
   '("\\(\\s"\\w*\\s"\\)" . font-lock-string-face)
   '("\\<\\(\\+\\|-\\)?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
   '("\\<true\\|false\\|null\\>" . font-lock-constant-face))
  "Minimal highlighting expressions for BL mode.")

(defvar bl-font-lock-keywords bl-font-lock-keywords-1
  "Default highlighting expressions for BL mode.")

(defvar bl-mode-syntax-table
  (let ((bl-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" bl-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?/ ". 124b" bl-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" bl-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" bl-mode-syntax-table)
	bl-mode-syntax-table)
  "Syntax table for bl-mode")
  
(defun bl-mode ()
  (interactive)
  (setq tab-width 2)
  (kill-all-local-variables)
  (use-local-map bl-mode-map)
  (set-syntax-table bl-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(bl-font-lock-keywords))
  ;; Register our indentation function
  (setq major-mode 'bl-mode)
  (setq mode-name "BL")
  (run-hooks 'bl-mode-hook))

(provide 'bl-mode)

;;; bl-mode.el ends here
