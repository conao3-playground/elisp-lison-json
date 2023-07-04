;;; lisfy-json.el --- lisfy into json                      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience
;; License: Apache-2.0

;;; Commentary:

;; Lisfy into json

;;; Code:

(require 'json)

(defun read-line () (read-string ""))

(defun get-stdin-buffer ()
  (with-current-buffer (get-buffer-create "*stdin*")
    (erase-buffer)
    (while (setq line (ignore-errors (read-line)))
      (insert line "\n"))
    (buffer-string)))

(defun lisfy-json-print (arg)
  "Return ARG as json string."
  (let ((json-encoding-pretty-print t))
    (json-encode arg)))

(defun batch-lisfy-json ()
  "Entrypoint of lisfy-json."
  (princ (lisfy-json-print (eval (read (get-stdin-buffer))))))

(provide 'lisfy-json)
;;; lisfy-json.el ends here

