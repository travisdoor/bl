;;; bl-mode-el -- Major mode for editing BL files

;; Author: Martin Dorazil
;; Created: 25 Sep 2018
;; Keywords: BL major-mode

;;; Code:
(defvar bl-mode-hook nil)
(defvar bl-mode-map
  (let ((bl-mode-map (make-keymap)))
    (define-key bl-mode-map "\C-j" 'newline-and-indent)
    bl-mode-map)
  "Keymap for BL major mode")

(add-to-list 'auto-mode-alist '("\\.bl\\'" . bl-mode))

(defconst bl-keywords
  '("const" "var" "if" "while" "loop" "break" "continue" "else" "extern"
    "module" "public" "struct" "enum" "fn" "return" "using" "cast"))

(defconst bl-types
  '("i8" "i16" "i32" "i64" "u8" "u16" "u32" "u64" "f32" "f64" "bool" "size_t" "void"
    "char" "string"))

(defconst bl-constants
  '("true" "false" "null"))

(defconst bl-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

(defun bl-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun bl-keywords-rx (keywords)
  "build keyword regexp"
  (bl-wrap-word-rx (regexp-opt keywords t)))

(defconst bl-font-lock-defaults
  `(
    ;; Keywords
    (,(bl-keywords-rx bl-keywords) 1 font-lock-keyword-face)

    ;; Types 
    (,(bl-keywords-rx bl-types) 1 font-lock-type-face)
    ("\\(struct \\)\\(\\w+\\)" 2 font-lock-type-face)

    ;; Constants
    (,(bl-keywords-rx bl-constants) 1 font-lock-constant-face)

    ;; Functions
    ("\\(\\w+\\)\\((\\)" 1 font-lock-function-name-face)

    ;; Modules
    ("\\(\\w+\\)\\(::\\)" 1 font-lock-reference-face)
    ("\\(using \\)\\(\\w+\\)" 2 font-lock-reference-face)
    ("\\(module \\)\\(\\w+\\)" 2 font-lock-reference-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)

    ;; Numbers
    (,(bl-wrap-word-rx bl-number-rx) . font-lock-constant-face)
    ))


(defvar bl-font-lock-keywords bl-font-lock-defaults
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
  (setq major-mode 'bl-mode)
  (setq mode-name "BL")
  (run-hooks 'bl-mode-hook))

(provide 'bl-mode)

;;; bl-mode.el ends here
