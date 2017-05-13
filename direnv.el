;;; direnv.el --- Direnv for Emacs -*- lexical-binding: t -*-

;; Copyright © 2017 Emanuel Evans

;; Author: Emanuel Evans <mail@emanuel.industries>
;; URL: http://github.com/shosti/direnv.el
;; Version: 0.0.1
;; Created: 12 May 2017
;; Keywords: environment
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
(require 'subr-x)
(require 'timer)

(defgroup direnv nil
  "Emacs integration with direnv"
  :group 'environment
  :prefix "direnv-")

(defcustom direnv-command "direnv"
  "The direnv command to use."
  :group 'direnv
  :type 'string)

(defcustom direnv-slow-command-delay 1
  "Delay in seconds before prompting to kill direnv command.

Set to nil to disable long command checks."
  :group 'direnv
  :type 'number)

(defconst direnv-buffer-name "*direnv*")
(defconst direnv-process-name "direnv")

(defvar direnv-slow-timer nil)

(defun direnv ()
  "Load environment using direnv."
  (interactive)
  (direnv--clean)
  (make-process :name direnv-process-name
                :buffer direnv-buffer-name
                :command (cons direnv-command '("export" "json"))
                :filter #'direnv--filter
                :sentinel #'direnv--sentinel
                :stderr "*direnv*")
  (when direnv-slow-command-delay
    (setq direnv-slow-timer
          (run-with-timer direnv-slow-command-delay nil #'direnv--check-slow))))

(defun direnv-allow ()
  "Allow the relevant .envrc and load it."
  (interactive)
  (direnv--clean)
  (if (eq (call-process direnv-command
                        nil
                        (list direnv-buffer-name t)
                        nil
                        "allow") 0)
      (direnv)
    (if (with-current-buffer direnv-buffer-name
          (save-excursion
            (goto-char (point-min))
            (search-forward ".envrc file not found" nil 'noerror)))
        (message ".envrc file not found")
      (error "Error calling `direnv allow', see buffer %s" direnv-buffer-name))))

(defun direnv--get-process ()
  "Get the current direnv process."
  (get-process direnv-process-name))

(defun direnv--clean ()
  "Clean any running direnv processes."
  (when-let ((proc (direnv--get-process)))
    (interrupt-process proc))
  (when-let ((buf (get-buffer direnv-buffer-name)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))))
  (direnv--kill-slow-timer))

(defun direnv--check-slow ()
  "Notify the user of a long-running direnv process and prompt to cancel."
  (switch-to-buffer-other-window direnv-buffer-name)
  (unless (y-or-n-p "Direnv is taking a while.  Continue? ")
    (interrupt-process (direnv--get-process))))

(defun direnv--kill-slow-timer ()
  "Kill any running slow process checker."
  (when (timerp direnv-slow-timer)
    (cancel-timer direnv-slow-timer)
    (setq direnv-slow-timer nil)))

(defun direnv--filter (_proc string)
  "Parse and load direnv json output.

PROC is assumed to be the direnv process object and STRING is its
stdout."
  (when-let ((env
              (condition-case nil
                  (let ((json-key-type 'string))
                    (json-read-from-string string))
                (json-end-of-file nil))))
    (direnv--load-env env)))

(defun direnv--sentinel (proc _event)
  "Handle direnv process events.

PROC is assumed to be the direnv process object and EVENT is the
relevant event."
  (unwind-protect
      (when (and (eq (process-status proc) 'exit)
                 (not (eq (process-exit-status proc) 0))
                 (with-current-buffer direnv-buffer-name
                   (save-excursion
                     (goto-char (point-min))
                     (search-forward ".envrc is blocked" nil 'noerror))))
        (direnv--kill-slow-timer)
        (direnv--maybe-allow))
    (direnv--kill-slow-timer)))

(defun direnv--maybe-allow (&optional prompt)
  "Notify the user of a blocked .envrc and prompt for action.

PROMPT specifies an optional prompt to use."
  (let* ((prompt (or prompt ".envrc is blocked. Allow? (y, n, v to visit) "))
         (str (read-key (propertize prompt
                                    'face 'minibuffer-prompt))))
    (cond ((member str '(?y ?Y)) (direnv-allow))
          ((member str '(?n ?N ? ?)))
          ((member str '(?v ?V)) (direnv--visit-envrc))
          (t (progn
               (direnv--maybe-allow "Please answer y, n, or v. "))))))

(defun direnv--visit-envrc ()
  "Visit the corresponding .envrc file."
  (when-let ((dir (locate-dominating-file default-directory ".envrc")))
    (find-file-other-window (concat dir ".envrc"))
    (shell-script-mode)))

(defun direnv--load-env (env)
  "Load ENV environment variables provided by direnv."
  (let ((msg (with-current-buffer direnv-buffer-name
               (save-excursion
                 (goto-char (point-min))
                 (buffer-substring (point-min) (point-at-eol))))))
    (unless (string-empty-p msg)
      (message "%s" msg)))
  (seq-each #'direnv--set-env-var env))

(defun direnv--set-env-var (var-pair)
  "Set an environment variable pair VAR-PAIR.

This also takes care of setting `exec-path' when necessary."
  (setenv (car var-pair) (cdr var-pair))
  (when (equal (car var-pair) "PATH")
    (setq exec-path (parse-colon-path (cdr var-pair)))))

(provide 'direnv)

;;; direnv.el ends here
