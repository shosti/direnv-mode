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
                                          #'direnv-incompatible-mode-p)
  "List of functions that determine whether to disable direnv."
  :group 'direnv
  :type 'list)

(defcustom direnv-incompatible-minor-modes '(helm-mode edebug-mode)
  "List of minor modes that are incompatible with direnv tracking."
  :group 'direnv
  :type 'list)

(defvar direnv-blocked-dirs (make-hash-table :test 'equal))
(defvar direnv-dominating-dirs-cache (make-hash-table :test 'equal))
(defvar direnv-inhibit nil)
(defvar direnv-last-dir nil)
(defvar direnv-running nil)
(defvar direnv-checkup-timer nil)

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
  "Enable variable `direnv-mode' for this buffer."
  (unless (minibufferp)
    (add-hook 'after-save-hook #'direnv--check-envrc-save)
    (direnv-mode 1)))

;;;###autoload
(defun direnv ()
  "Load environment using direnv."
  (interactive)
  (unless (or direnv-inhibit
              direnv-running
              (and (not (called-interactively-p 'any))
                   (equal direnv-last-dir default-directory)))
    (let ((direnv-inhibit t)
          (dir default-directory))
      (setq direnv-last-dir dir)
      (setq direnv-running t)
      (if (direnv--blocked-p)
          (message "direnv: blocked")
        (let ((direnv-buffer (generate-new-buffer "*direnv-out*"))
              (stderr-buffer (generate-new-buffer "*direnv-stderr*")))
          (setq direnv-checkup-timer (run-with-timer direnv-slow-command-delay nil
                                                     (direnv--checkup direnv-buffer)))
          (make-process :name "direnv"
                        :buffer direnv-buffer
                        :command (cons direnv-command '("export" "json"))
                        :sentinel (direnv--sentinel direnv-buffer
                                                    stderr-buffer
                                                    dir
                                                    (called-interactively-p 'any))
                        :stderr stderr-buffer))))))

;;;###autoload
(defun direnv-allow (&optional force)
  "Allow the relevant .envrc and load it.

Optional prefix argument FORCE means that the relevant .envrc
will be unblocked if necessary."
  (interactive "P")
  (let ((direnv-inhibit t))
    (when force
      (direnv--unblock))

    (when (direnv--blocked-p)
      (user-error
       "Current .envrc is blocked in direnv; call with prefix arg to force "))

    (with-temp-buffer
      (if (eq (call-process direnv-command
                            nil
                            t
                            nil
                            "allow" "json") 0)
          (let ((direnv-inhibit nil)
                (direnv-last-dir nil))
            (direnv)
            (message "direnv: allowed %s" default-directory))
        (if (save-excursion
              (goto-char (point-min))
              (search-forward ".envrc file not found" nil 'noerror))
            (message ".envrc file not found")
          (error "Error calling `direnv allow': %s" (buffer-string)))))))

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

(defun direnv-clear-caches ()
  "Clear out all direnv caches."
  (setq direnv-dominating-dirs-cache (make-hash-table :test 'equal)))

(defun direnv--unblock ()
  "Unblock .envrc."
  (when-let ((dir (direnv--find-envrc-dir)))
    (remhash dir direnv-blocked-dirs)))

(defun direnv--blocked-p ()
  "Check whether the relevant .envrc is blocked."
  (when-let ((dir (direnv--find-envrc-dir)))
    (gethash dir direnv-blocked-dirs)))

(defun direnv--find-envrc-dir (&optional dir)
  "Find the directory with the dominating .envrc.
Optional argument DIR indicates which directory to start from."
  (let ((current-dir (or dir default-directory)))
    (if-let ((dominating-dir (gethash current-dir direnv-dominating-dirs-cache)))
        (if (eq dominating-dir :none)
            nil
          (expand-file-name dominating-dir))
      (if-let ((dominating-dir (locate-dominating-file current-dir ".envrc")))
          (puthash current-dir (expand-file-name dominating-dir) direnv-dominating-dirs-cache)
        (puthash current-dir :none direnv-dominating-dirs-cache)
        nil))))

(defun direnv--checkup (direnv-buffer)
  "Notify the user of a long-running direnv process belonging to
DIRENV-BUFFER and prompt to cancel."
  (lambda ()
    (when direnv-running
      (let ((direnv-inhibit t)
            (proc (get-buffer-process direnv-buffer)))
        (if (process-live-p proc)
            (let ((answer
                   (read-key
                    (propertize
                     "Direnv is taking a while.  Continue? (y, n, v to visit buffer, b to block .envrc)"
                     'face 'minibuffer-prompt))))
              (cond ((member answer '(?n ?N))
                     (kill-process (get-buffer-process direnv-buffer)))
                    ((member answer '(?v ?V)) (direnv--visit-envrc)
                     (switch-to-buffer-other-window direnv-buffer))
                    ((member answer '(?b ?B)) (direnv-block))))
          (setq direnv-running nil))))))

(defun direnv--parse (direnv-buffer stderr-buffer)
  "Parse direnv output in DIRENV-BUFFER.
STDERR-BUFFER should contain direnv's stderr output."
  (let ((direnv-inhibit t))
    (when-let ((env
                (condition-case nil
                    (let ((json-key-type 'string))
                      (with-current-buffer direnv-buffer
                        (goto-char (point-min))
                        (json-read)))
                  (json-end-of-file nil))))
      (direnv--load-env env stderr-buffer))))

(defun direnv--sentinel (direnv-buffer
                         stderr-buffer
                         dir
                         called-interactively)
  "Return a function that handles direnv process events.

DIRENV-BUFFER should be the parent of the process.
STDERR-BUFFER should contain the process's sterr.
DIR indicates the directory for which direnv is working.
CALLED-INTERACTIVELY indicates that direnv was called
interactively so blocking feedback is acceptable."
  (lambda (proc _event)
    (let ((direnv-inhibit t))
      (unwind-protect
          (when (eq (process-status proc) 'exit)
            (cancel-timer direnv-checkup-timer)
            (if (eq (process-exit-status proc) 0)
                ;; We don't want to actually apply the vars if the directory has
                ;; changed.
                (when (equal default-directory dir)
                  (direnv--parse direnv-buffer stderr-buffer))
              (when (with-current-buffer stderr-buffer
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward ".envrc is blocked" nil 'noerror)))
                (direnv--maybe-allow called-interactively)))
            (let ((kill-buffer-query-functions nil))
              (kill-buffer direnv-buffer)
              (kill-buffer stderr-buffer)))
        (unless (process-live-p proc)
          (setq direnv-running nil))))))

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

(defun direnv--visit-envrc (&optional dir)
  "Visit the corresponding .envrc file in DIR or the current directory."
  (when-let ((dir (direnv--find-envrc-dir dir)))
    (find-file-other-window (concat dir ".envrc"))
    (shell-script-mode)))

(defun direnv--load-env (env stderr-buffer)
  "Load ENV environment variables provided by direnv.
STDERR-BUFFER should contain direnv's stderr output."
  (let ((msg (with-current-buffer stderr-buffer
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
  "Hook to use to set up direnv the current buffer."
  (when (and direnv-mode
             (not direnv-inhibit)
             (not (seq-find #'funcall direnv-disable-functions)))
    (direnv)))

(defun direnv--check-envrc-save ()
  "Clear caches if current buffer is a .envrc file."
  (when (equal (file-name-nondirectory (buffer-file-name)) ".envrc")
    (direnv-clear-caches)))

(defun direnv-incompatible-mode-p ()
  "Check for minor modes that are incompatible with direnv."
  (seq-find
   (lambda (sym)
     (and (boundp sym) (symbol-value sym)))
   direnv-incompatible-minor-modes))

(defun direnv--add-hooks ()
  "Add hooks to set up direnv."
  (add-hook 'buffer-list-update-hook #'direnv--hook))

(defun direnv--remove-hooks ()
  "Remove hooks to set up direnv."
  (remove-hook 'buffer-list-update-hook #'direnv--hook)
  (remove-hook 'after-save-hook #'direnv--check-envrc-save))

(provide 'direnv)

;;; direnv.el ends here
