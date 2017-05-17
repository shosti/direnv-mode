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

(defcustom direnv-disable-functions (list #'minibufferp
                                          #'direnv-in-direnv-buffer-p
                                          #'direnv-incompatible-mode-p)
  "List of functions that determine whether to disable direnv."
  :group 'direnv
  :type 'list)

(defcustom direnv-incompatible-minor-modes '(helm-mode)
  "List of minor modes that are incompatible with direnv tracking."
  :group 'direnv
  :type 'list)

(defconst direnv-out-buffer-name "*direnv*")
(defconst direnv-proc-buffer-name "*direnv-process*")
(defconst direnv-process-name "direnv")

(defvar direnv-slow-timer nil)
(defvar direnv-blocked-dirs (make-hash-table :test #'equal))
(defvar direnv-inhibit nil)
(defvar direnv-last-dir nil)

(define-minor-mode direnv-mode
  "Minor mode for automatically setting environment variables with direnv."
  :group 'direnv
  :lighter " direnv"
  (if direnv-mode
      (direnv--add-hooks)
    (direnv--remove-hooks)))

(define-globalized-minor-mode global-direnv-mode direnv-mode
  direnv-mode-turn-on)

(defun direnv-mode-turn-on ()
  "Enable `direnv-mode' for this buffer."
  (unless (minibufferp)
    (direnv-mode 1)))

;;;###autoload
(defun direnv ()
  "Load environment using direnv."
  (interactive)
  (message "B: DIRENV-INHIBIT: %s" direnv-inhibit)
  (unless (or direnv-inhibit (equal direnv-last-dir default-directory))
    (message "DIRENV-LAST-DIR: %s" direnv-last-dir)
    (setq direnv-last-dir default-directory)
    (direnv--clean)
    (if (direnv--blocked-p)
        (message "direnv: blocked")
      (make-process :name direnv-process-name
                    :buffer direnv-proc-buffer-name
                    :command (cons direnv-command '("export" "json"))
                    :filter #'direnv--filter
                    :sentinel (direnv--sentinel (called-interactively-p))
                    :stderr "*direnv*")
      (when direnv-slow-command-delay
        (setq direnv-slow-timer
              (run-with-timer direnv-slow-command-delay nil #'direnv--check-slow))))))

;;;###autoload
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
                        (list direnv-out-buffer-name t)
                        nil
                        "allow") 0)
      (direnv)
    (if (with-current-buffer direnv-out-buffer-name
          (save-excursion
            (goto-char (point-min))
            (search-forward ".envrc file not found" nil 'noerror)))
        (message ".envrc file not found")
      (error "Error calling `direnv allow', see buffer %s" direnv-out-buffer-name))))

;;;###autoload
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
  (when-let ((dir (locate-dominating-file default-directory ".envrc")))
    (expand-file-name dir)))

(defun direnv--get-process ()
  "Get the current direnv process."
  (get-buffer-process direnv-proc-buffer-name))

(defun direnv--clean ()
  "Clean any running direnv processes."
  (when-let ((proc (direnv--get-process)))
    (when (process-live-p proc)
      (kill-process proc)))
  (when-let ((buf (get-buffer direnv-out-buffer-name)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))))
  (direnv--kill-slow-timer))

(defun direnv--check-slow ()
  "Notify the user of a long-running direnv process and prompt to cancel."
  (let ((direnv-inhibit t))
    (let ((answer
           (read-key
            (propertize
             "Direnv is taking a while.  Continue? (y, n, v to visit buffer, b to block .envrc)"
             'face 'minibuffer-prompt))))
      (cond ((member answer '(?n ?N))
             (kill-process (direnv--get-process)))
            ((member answer '(?v ?V)) (direnv--visit-envrc)
             (message "DIRENV-INHIBIT: %s" direnv-inhibit)
             (switch-to-buffer-other-window direnv-out-buffer-name))
            ((member answer '(?b ?B)) (direnv-block))))))

(defun direnv--kill-slow-timer ()
  "Kill any running slow process checker."
  (when (timerp direnv-slow-timer)
    (cancel-timer direnv-slow-timer)
    (setq direnv-slow-timer nil)))

(defun direnv--filter (_proc string)
  "Parse and load direnv json output.

PROC is assumed to be the direnv process object and STRING is its
stdout."
  (let ((direnv-inhibit t))
    (when-let ((env
                (condition-case nil
                    (let ((json-key-type 'string))
                      (json-read-from-string string))
                  (json-end-of-file nil))))
      (direnv--load-env env))))

(defun direnv--sentinel (called-interactively)
  "Return a function that handles direnv process events.

CALLED-INTERACTIVELY indicates that `direnv' was called
interactively so blocking feedback is acceptable."
  (lambda (proc _event)
    (let ((direnv-inhibit t))
      (unwind-protect
          (when (and (eq (process-status proc) 'exit)
                     (not (eq (process-exit-status proc) 0))
                     (with-current-buffer direnv-out-buffer-name
                       (save-excursion
                         (goto-char (point-min))
                         (search-forward ".envrc is blocked" nil 'noerror))))
            (direnv--kill-slow-timer)
            (direnv--maybe-allow called-interactively))
        (direnv--kill-slow-timer)))))

(defun direnv--maybe-allow (prompt-for-allow &optional prompt)
  "Notify the user of a blocked .envrc and prompt for action.

PROMPT-FOR-ALLOW indicates that the user should be prompted to
allow instead of just issuing a message.

PROMPT specifies an optional prompt to use."
  (if prompt-for-allow
      (let* ((prompt (or prompt ".envrc is blocked. Allow? (y, n, v to visit, b to block permanently) "))
             (str (read-key (propertize prompt
                                        'face 'minibuffer-prompt))))
        (cond ((member str '(?y ?Y)) (direnv-allow))
              ((member str '(?n ?N ? ?)))
              ((member str '(?v ?V)) (direnv--visit-envrc))
              ((member str '(?b ?B)) (direnv-block))
              (t (progn
                   (direnv--maybe-allow "Please answer y, n, v, or b. ")))))
    (message ".envrc is blocked, please call `direnv-allow' to allow.")))

(defun direnv--visit-envrc ()
  "Visit the corresponding .envrc file."
  (when-let ((dir (direnv--find-envrc-dir)))
    (find-file-other-window (concat dir ".envrc"))
    (shell-script-mode)))

(defun direnv--load-env (env)
  "Load ENV environment variables provided by direnv."
  (let ((msg (with-current-buffer direnv-out-buffer-name
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

(defun direnv--hook ()
  "Hook to use to set up direnv in `direnv-mode'."
  (when (and direnv-mode
             (not (seq-find #'funcall direnv-disable-functions)))
    (direnv)))

(defun direnv-in-direnv-buffer-p ()
  "Return non-nil if currently in a direnv buffer."
  (member (buffer-name)
          (list direnv-out-buffer-name direnv-proc-buffer-name)))

(defun direnv-incompatible-mode-p ()
  (seq-find
   (lambda (sym)
     (and (boundp sym) (symbol-value sym)))
   direnv-incompatible-minor-modes))

(defun direnv--add-hooks ()
  "Add hooks to set up direnv."
  (add-hook 'buffer-list-update-hook #'direnv--hook))

(defun direnv--remove-hooks ()
  "Remove hooks to set up direnv."
  (remove-hook 'buffer-list-update-hook #'direnv--hook))

(provide 'direnv)

;;; direnv.el ends here
