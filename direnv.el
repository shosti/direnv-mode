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
(defvar direnv-blocked-dirs (make-hash-table :test #'equal))

(defun direnv ()
  "Load environment using direnv."
  (interactive)
  (direnv--clean)
  (if (direnv--blocked-p)
      (message "direnv: blocked")
    (make-process :name direnv-process-name
                  :buffer direnv-buffer-name
                  :command (cons direnv-command '("export" "json"))
                  :filter #'direnv--filter
                  :sentinel #'direnv--sentinel
                  :stderr "*direnv*")
    (when direnv-slow-command-delay
      (setq direnv-slow-timer
            (run-with-timer direnv-slow-command-delay nil #'direnv--check-slow)))))

(defun direnv-allow (&optional force)
  "Allow the relevant .envrc and load it.

Optional prefix argument FORCE means that the relevant .envrc
will be unblocked if necessary."
  (interactive "P")
  (direnv--clean)

  (when force
    (direnv--unblock))

  (when (direnv--blocked-p)
    (user-error
     "Current .envrc is blocked in direnv; call with prefix arg to force "))

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

(defun direnv-block ()
  "Block the current relevant .envrc.

You will no longer be prompted to allow .envrc if the directory
is blocked.  Directories can be unblocked by calling
`direnv-allow' with a prefix argument."
  (interactive)
  (if-let ((dir (direnv--find-envrc-dir)))
      (progn
        (puthash dir t direnv-blocked-dirs)
        (message
         "Direnv disabled for %s; call `direnv-allow' with prefix arg to unblock"
         dir))
    (error ".envrc not found in %s" default-directory)))

(defun direnv--unblock ()
  "Unblock .envrc."
  (when-let ((dir (direnv--find-envrc-dir)))
    (remhash dir direnv-blocked-dirs)))

(defun direnv--blocked-p ()
  "Check whether the relevant .envrc is blocked."
  (when-let ((dir (direnv--find-envrc-dir)))
    (gethash dir direnv-blocked-dirs)))

(defun direnv--find-envrc-dir ()
  "Find the directory with the dominating .envrc."
  (expand-file-name (locate-dominating-file default-directory ".envrc")))

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
  (let* ((prompt (or prompt ".envrc is blocked. Allow? (y, n, v to visit, b to block permanently) "))
         (str (read-key (propertize prompt
                                    'face 'minibuffer-prompt))))
    (cond ((member str '(?y ?Y)) (direnv-allow))
          ((member str '(?n ?N ? ?)))
          ((member str '(?v ?V)) (direnv--visit-envrc))
          ((member str '(?b ?B)) (direnv-block))
          (t (progn
               (direnv--maybe-allow "Please answer y, n, v, or b. "))))))

(defun direnv--visit-envrc ()
  "Visit the corresponding .envrc file."
  (when-let ((dir (direnv--find-envrc-dir)))
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
