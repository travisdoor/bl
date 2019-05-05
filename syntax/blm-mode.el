;;; blm-mode-el -- Major mode for editing BLM files

;; Author: Martin Dorazil
;; Created: 25 Sep 2018
;; Keywords: BLM major-mode

;;; Code:
(defvar blm-mode-hook nil)
(defvar blm-mode-map
  (let ((blm-mode-map (make-keymap)))
    (define-key blm-mode-map "\C-j" 'newline-and-indent)
    blm-mode-map)
  "Keymap for BLM major mode")

(add-to-list 'auto-mode-alist '("\\.blm\\'" . blm-mode))

(defconst blm-keywords
  '("const"
    "br"
    "bitcast"
    "ptrtoint"
    "inttoptr"
    "noopcast"
    "zext"
    "sext"
    "trunc"
    "fptosi"
    "fptoui"
    "arrtoslice"
    "addrof"
    "elemptr"
    "memberptr"
    "unop"
    "unreachable"
    "typeinfer"
    "load"
    "decl"
    "declmember"
    "declvariant"
    "declref"
    "call"
    "validate_type"
    "ret"
    "store"
    "binop"
    "unreachable"
    "arg"
    "sizeof"
    "typeinfo"
    "compound"
    "vargs"
    "phi"
    "alignof")) 

(defconst blm-types
  '("s8" "s16" "s32" "s64" "u8" "u16" "u32" "u64" "f32" "f64" "bool" "usize" "void" "string"
    "type" "slice" "null_t"))

(defconst blm-constants
  '("true" "false" "null"))

(defconst blm-number-rx
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9" "."))))
       symbol-end)))

(defun blm-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun blm-keywords-rx (keywords)
  "build keyword regexp"
  (blm-wrap-word-rx (regexp-opt keywords t)))

(defconst blm-font-lock-defaults
  `(
    ;; Keywords
    (,(blm-keywords-rx blm-keywords) 1 font-lock-keyword-face)

    ;; Types 
    ("fn\(.*\) \\w+" . font-lock-type-face)
    ("struct{.*}" . font-lock-type-face)
    ("slice{.*}" . font-lock-type-face)
    ("enum{.*}" . font-lock-type-face)
    ("*.\\w+" . font-lock-type-face)
    (,(blm-keywords-rx blm-types) 1 font-lock-type-face)

    ;; Hash directives
    ("#\\w+" . font-lock-preprocessor-face)

    ;; Functions
    ("@\\w+" . font-lock-function-name-face)
    ("@\\.\\w+" . font-lock-builtin-face)

    ;; IDs 
    ("%\\w+" . font-lock-variable-name-face)
    ("$\\w+" . font-lock-reference-face)

    ;; Invalid
    ("<.*>" . font-lock-warning-face)

    ;; Analyze helper instruction
    ("'\\w+" . font-lock-builtin-face)

    ;; Chars 
    ("\\\\'.*\\\\'" . font-lock-string-face)

    ;; Strings
    ("\\\".*\\\"" . font-lock-string-face)
    ))


(defvar blm-font-lock-keywords blm-font-lock-defaults
  "Default highlighting expressions for BLM mode.")

(defvar blm-mode-syntax-table
  (let ((blm-mode-syntax-table (make-syntax-table)))
    
                                        ; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" blm-mode-syntax-table)
    
                                        ; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" blm-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" blm-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" blm-mode-syntax-table)
    blm-mode-syntax-table)
  "Syntax table for blm-mode")

(defun blm-mode ()
  (interactive)
  (setq tab-width 2)
  (kill-all-local-variables)
  (use-local-map blm-mode-map)
  (set-syntax-table blm-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(blm-font-lock-keywords))
  (setq major-mode 'blm-mode)
  (setq mode-name "BLM")
  (run-hooks 'blm-mode-hook))

(provide 'blm-mode)

;;; blm-mode.el ends here
