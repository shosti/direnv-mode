;;; direnv.el --- Direnv for Emacs -*- lexical-binding: t -*-

;; Copyright © 2017 Emanuel Evans

;; Author: Emanuel Evans <mail@emanuel.industries>
;; URL: http://github.com/shosti/direnv.el
;; Version: 0.0.1
;; Created: 26 March 2013
;; Keywords: workspace
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides Emacs integration with direnv

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'env)
(require 'json)
(require 'seq)

(defcustom direnv-command "direnv"
  "The direnv command to use.")

(defconst direnv-buffer-name "*direnv*")
(defconst direnv-process-name "direnv")

(defun direnv--get-process ()
  (get-process direnv-proc))

(defun direnv ()
  (interactive)
  (direnv--clean)
  (make-process :name direnv-process-name
                :buffer direnv-buffer-name
                :command (cons direnv-command '("export" "json"))
                :filter #'direnv--filter
                :sentinel #'direnv--sentinel
                :stderr "*direnv*"))

(defun direnv-allow ()
  (interactive)
  (direnv--clean)
  (if (eq (call-process direnv-command
                        nil
                        (list direnv-buffer-name t)
                        nil
                        "allow") 0)
      (message "direnv: allowed .envrc in %s" default-directory)
    (if (with-current-buffer direnv-buffer-name
          (save-excursion
            (goto-char (point-min))
            (search-forward ".envrc file not found" nil 'noerror)))
        (message ".envrc file not found")
      (error "Error calling `direnv allow', see buffer %s" direnv-buffer-name))))

(defun direnv--clean ()
  (when-let ((proc (direnv--get-process)))
    (kill-process proc))
  (with-current-buffer direnv-buffer-name
    (delete-region (point-min) (point-max))))

(defun direnv--filter (proc string)
  (when-let ((env
              (condition-case nil
                  (let ((json-key-type 'string))
                    (json-read-from-string string))
                (json-end-of-file nil))))
    (direnv--load-env env)))

(defun direnv--sentinel (proc event)
  (unwind-protect
      (when (and (eq (process-status proc) 'exit)
                 (not (eq (process-exit-status proc) 0))
                 (with-current-buffer direnv-buffer-name
                   (save-excursion
                     (goto-char (point-min))
                     (search-forward ".envrc is blocked" nil 'noerror)
                     (y-or-n-p ".envrc is blocked. Allow?"))))
        (direnv-allow))))

(defun direnv--load-env (env)
  (let ((msg (with-current-buffer direnv-buffer-name
               (save-excursion
                 (goto-char (point-min))
                 (buffer-substring (point-min) (point-at-eol))))))
    (unless (string-empty-p msg)
      (message "%s" msg)))
  (seq-each #'direnv--set-env-var env))

(defun direnv--set-env-var (var-pair)
  (setenv (car var-pair) (cdr var-pair))
  (when (equal (car var-pair) "PATH")
    (setq exec-path (split-string (cdr var-pair) ":"))))

(provide 'direnv)

;;; direnv.el ends here
