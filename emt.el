;;; emt.el  --- Tokenizing CJK words and symbols with macOS's built-in NLP tokenizer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Roife Wu

;; Author: Roife Wu <roifewu@gmail.com>
;; URL: https://github.com/cireu/jieba.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: chinese, cjk, tokenizer, macos, mac, natural language, segmentation

;; This file is NOT a part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; EMT stands for Emacs MacOS Tokenizer.
;; This package use macOS's built-in NLP tokenizer to tokenize and operate on
;; CJK words in Emacs.

;;; Code:

(eval-when-compile (require 'thingatpt))
(eval-when-compile (require 'subr-x))

;;; Customize

(defgroup emt ()
  "Tokenize CJK words with macOS's built-in NLP tokenizer."
  :group 'chinese
  :prefix "emt-")

(defcustom emt-use-cache t
  "Caches for results of tokenization if non-nil."
  :type 'boolean
  :group 'emt)

(defcustom emt--cache-lru-size 50
  "The size of LRU cache for tokenization results."
  :type 'integer
  :group 'emt)

(defcustom emt-lib-path (concat user-emacs-directory "modules/libEMT" module-file-suffix)
  "The path to the directory of dynamic library for emt."
  :type 'string
  :group 'emt)

;;; Export function

(defconst emt-version "1.0")

(defvar emt--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of the package.")

(defvar emt--cjk-char-regex-forward (format "\\W*\\(\\cc\\|\\cj\\|\\ch\\)")
  "Regex for CJK char.")

(defvar emt--cjk-char-regex-backward (format "\\(\\cc\\|\\cj\\|\\ch\\)\\W*")
  "Regex for CJK char.")

(defvar emt--lib-loaded nil
  "Whether dynamic module for emt is loaded.")

(defvar emt--cache-set (make-hash-table :test #'equal)
  "The hash table for caching tokenization results.")

(defvar emt--cache-lru-list nil
  "The LRU list for caching tokenization results.")

(defvar emt--lib-fns
  '(emt--do-split-helper
    emt--do-split-without-bounds-helper
    emt--word-at-point-or-forward-helper)
  "The list of functions in dynamic module.")

(defun emt--cache-get (key)
  "Get the value of KEY in cache."
  (setq key (string-trim (substring-no-properties key) "\\W*" "\\W*"))
  (let ((value (gethash key emt--cache-set)))
    (when value
      (setq emt--cache-lru-list (delete key emt--cache-lru-list))
      (push key emt--cache-lru-list))
    value))

(defun emt--cache-put (key value)
  "Put KEY and VALUE into cache."
  (setq key (string-trim (substring-no-properties key) "\\W*" "\\W*"))
  (puthash key value emt--cache-set)
  (push key emt--cache-lru-list)
  (when (> (length emt--cache-lru-list) emt--cache-lru-size)
    (setq emt--cache-lru-list (butlast emt--cache-lru-list))))

(defun emt--get-bounds-at-point (direction)
  "Get the bounds of the CJK string at point.

If DIRECTION is `'forward', return the bounds of the string forward.
If DIRECTION is `'backward', return the bounds of the string backward.
If DIRECTION is `'all', return the bounds of the string forward and backward."
  (let ((beg (point))
        (end (point)))
    (when (or (eq direction 'forward) (eq direction 'all))
      (when (looking-at emt--cjk-char-regex-forward)
        (save-excursion
          (forward-word)
          (setq end (point)))))
    (when (or (eq direction 'backward) (eq direction 'all))
      (when (looking-back emt--cjk-char-regex-backward nil)
        (save-excursion
          (backward-word)
          (setq beg (point)))))
    (cons beg end)))

(defun emt--word-at-point-helper (back)
  "Return the word at point.

If BACK is non-nil, return the word backward."
  (unless emt--lib-loaded (error "Dynamic module not loaded"))
  (let* ((bounds (emt--get-bounds-at-point 'all))
         (beg (car bounds))
         (end (cdr bounds))
         (index (- (point) beg))
         (wordAndRange (emt--word-at-point-or-forward-helper
                        (buffer-substring-no-properties beg end)
                        (if (and back (> index 0))
                            (1- index)
                          index)))
         (word-beg (cadr wordAndRange))
         (word-end (cddr wordAndRange)))
    (if (eq word-beg word-end)
        (word-at-point)
      (buffer-substring (+ beg word-beg) (+ beg word-end)))))

(defun emt--upperbound (pred vec)
  "Binary search to find the last element in VEC satisfying PRED."
  (if (null vec) nil
    (let ((start 0)
          (end (1- (length vec))))
      (while (< start end)
        (let ((mid (ash (+ start end 1) -1)))
          (if (funcall pred (elt vec mid))
              (setq start mid)
            (setq end (1- mid)))))
      (elt vec start))))

(defun emt--lowerbound (pred vec)
  "Binary search to find the first element in VEC satisfying PRED."
  (if (null vec) nil
    (let ((start 0)
          (end (1- (length vec))))
      (while (< start end)
        (let ((mid (ash (+ start end) -1)))
          (if (funcall pred (elt vec mid))
              (setq end mid)
            (setq start (1+ start)))))
      (elt vec end))))

(defun emt--move-by-word (direction)
  "Move point by word.

If DIRECTION is `'forward', move point forward by word.
If DIRECTION is `'backward', move point backward by word."
  (let* ((bounds-at-point (emt--get-bounds-at-point 'all))
         (beg (car bounds-at-point))
         (end (cdr bounds-at-point)))
    (if (eq beg end)
        (if (eq direction 'forward) (forward-word) (backward-word))
      (let* ((text (buffer-substring-no-properties (car bounds-at-point) (cdr bounds-at-point)))
             (results (emt-split text))
             (pos (- (point) beg))
             target-bound)
        (cond ((eq direction 'forward)
               (setq target-bound (emt--lowerbound (lambda (x) (> (cddr x) pos)) results))
               (if (and target-bound (> (cddr target-bound) pos))
                   (goto-char (+ beg (cddr target-bound)))
                 (forward-word)))
              (t (setq target-bound (emt--upperbound (lambda (x) (< (cadr x) pos)) results))
                 (if (and target-bound (< (cadr target-bound) pos))
                     (goto-char (+ beg (cadr target-bound)))
                   (backward-word)))))))
  t)

(defun emt-split (str)
  "Split STR into a list of words.

Return a list of cons, each of which has a word and its bound."
  (if emt--lib-loaded
      (if-let ((cached (and emt-use-cache (emt--cache-get str))))
          cached
          (let ((result (emt--do-split-helper str)))
            (when emt-use-cache (emt--cache-put str result))
            result))
    (error "Dynamic module not loaded")))

(defun emt-split-without-bounds (str)
  "Split STR into a list of words. Just return a list of word."
  (if emt--lib-loaded
      (if-let ((cached (and emt-use-cache (emt--cache-get str))))
          (mapcar #'car cached)
        (let ((result (emt--do-split-without-bounds-helper str)))
          (when emt-use-cache (emt--cache-put str (cons result nil)))
          result))
    (error "Dynamic module not loaded")))

;;;###autoload
(defun emt-word-at-point-or-forward ()
  "Return the word at point.

If current point is at bound of a word, return the one forward."
  (interactive)
  (emt--word-at-point-helper nil))

;;;###autoload
(defun emt-word-at-point-or-backward ()
  "Return the word at point.

If current point is at bound of a word, return the one backward."
  (interactive)
  (emt--word-at-point-helper t))

;;;###autoload
(defun emt-compiler-module (&optional path)
  "Compile dynamic module."
  (interactive)
  (unless (eq system-type 'darwin)
    (error "Only support macOS"))
  (unless module-file-suffix
    (error "Variable `module-file-suffix' is nil"))
  (unless (executable-find "swift")
    (error "Swift compiler not found"))
  (unless (file-directory-p (concat emt--root "module/"))
    (error "No module source found"))

  (setq path (or path emt-lib-path))
  (let ((default-directory (concat emt--root "module/")))
    (if (zerop (shell-command "swift build -c release"))
        (progn (message "Compile succeed!")
               (make-directory (file-name-directory path) t)
               (copy-file (concat emt--root "module/.build/release/libEMT" module-file-suffix)
                          path t))
      (error "Compile dynamic module failed"))))

;;;###autoload
(defun emt-forward-word (&optional arg)
  "CJK compatible version of `forward-word'.

Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((direction (if (< arg 0) 'backward 'forward)))
    (dotimes (_ (abs arg))
      (emt--move-by-word direction))))

;;;###autoload
(defun emt-backward-word (&optional arg)
  "CJK compatible version of `backward-word'.

Move point backward ARG words (forward if ARG is negative).
If ARG is omitted or nil, move point forward one word."
  (interactive "p")
  (setq arg (or arg 1))
  (emt-forward-word (- arg)))

;;;###autoload
(defun emt-kill-word (arg)
  "CJK compatible version of `kill-word'.

Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (emt-forward-word arg) (point))))

;;;###autoload
(defun emt-backward-kill-word (arg)
  "CJK compatible version of `backward-kill-word'.

Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (emt-kill-word (- arg)))

;;;###autoload
(defun emt-mark-word (&optional arg)
  "CJK compatible version of `mark-word'.

Set mark ARG words from point or move mark one word."
  (interactive "p")
  (set-mark (point))
  (emt-forward-word arg))

;;;###autoload
(defun emt-ensure ()
  "Load the dynamic library."
  (interactive)
  (unless emt--lib-loaded
    (if (not (file-exists-p emt-lib-path))
        (error "No compiled dynamic module found")
      (load-file emt-lib-path)
      (dolist (fn emt--lib-fns)
        (unless (fboundp fn)
          (error "No %s function found in dynamic module" fn)))
      (setq emt--lib-loaded t))))

;;; Minor mode

;;;###autoload
(defvar emt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word] #'emt-forward-word)
    (define-key map [remap backward-word] #'emt-backward-word)
    (define-key map [remap kill-word] #'emt-kill-word)
    (define-key map [remap backward-kill-word] #'emt-backward-kill-word)
    map))

;;;###autoload
(define-minor-mode emt-mode
  "Minor mode for tokenizing CJK words with macOS's built-in NLP tokenizer."
  :global t
  :keymap emt-mode-map
  :lighter "emt"
  (when emt-mode (emt-ensure)))

(provide 'emt)

;;; emt.el ends here
