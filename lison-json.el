;;; lison-json.el --- lison into json                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience
;; License: Apache-2.0

;;; Commentary:

;; Lison into json

;;; Code:

(require 'json)

(defun read-line () (read-string ""))

(defun get-stdin-buffer ()
  (with-current-buffer (get-buffer-create "*stdin*")
    (erase-buffer)
    (while (setq line (ignore-errors (read-line)))
      (insert line "\n"))
    (buffer-string)))

(defun lison-json-print (arg)
  "Return ARG as json string."
  (let ((json-encoding-pretty-print t))
    (json-encode arg)))

(defun batch-lison-json ()
  "Entrypoint of lison-json."
  (princ (lison-json-print (eval (read (get-stdin-buffer))))))

(provide 'lison-json)
;;; lison-json.el ends here
