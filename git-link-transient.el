;;; git-link-transient.el --- Transient interface for git-link  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2024 Skye Shaw and others

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient interface (magit-like menu) for git-link.
;; Call `git-link-dispatch' to show the menu.
;;
;; You need to have `transient' installed as a dependency.
;; (it's not listed as the dependency of git-link because we want it to be optional.)

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'git-link)

(defun git-link-dispatch--action ()
  "Finally call `git-link' with transient arguments."
  (let* ((args (transient-args 'git-link-dispatch))
         (git-link-default-branch (transient-arg-value "branch=" args))
         (git-link-default-remote (transient-arg-value "remote=" args))
         (git-link-use-commit (transient-arg-value "use_commit" args))
         (git-link-use-single-line-number (transient-arg-value "line_number" args)))
    (call-interactively #'git-link)))

(defun git-link-dispatch--copy ()
  "The copy command in transient suffix."
  (interactive)
  (let ((git-link-open-in-browser nil)
        (git-link-add-to-kill-ring t))
    (git-link-dispatch--action)))

(defun git-link-dispatch--open ()
  "The open command in transient suffix."
  (interactive)
  (let ((git-link-open-in-browser t))
    (git-link-dispatch--action)))

(defclass git-link--transient-bare-option (transient-option) ()
  "Similar to `transient-option', but format without argument string.")

(cl-defmethod transient-format ((obj git-link--transient-bare-option))
  (format " %s %s (%s)"
          (transient-format-key obj)
          (transient-format-description obj)
          (let ((v (oref obj value)))
            (if (> (length v) 0)
                (propertize v 'face 'transient-value)
              (propertize "default" 'face 'transient-inactive-value)))))

(defclass git-link--transient-bare-switch (transient-switch) ()
  "Similar to `transient-switch', but format without argument string, only yes/no.")

(cl-defmethod transient-format ((obj git-link--transient-bare-switch))
  (format " %s %s (%s)"
          (transient-format-key obj)
          (transient-format-description obj)
          (if (oref obj value)
              (propertize "on" 'face 'transient-value)
            (propertize "off" 'face 'transient-inactive-value))))

(transient-define-infix git-link-dispatch--branch ()
  :class git-link--transient-bare-option
  :argument "branch="
  :description "Branch"
  :prompt "Branch: "
  :key "b"
  :init-value (lambda (obj) (oset obj value git-link-default-branch))
  :reader (lambda (prompt &rest _)
            (completing-read
             prompt
             (remove nil (list git-link-default-branch (git-link--branch))))))

(transient-define-infix git-link-dispatch--remote ()
  :class git-link--transient-bare-option
  :argument "remote="
  :description "Remote"
  :key "r"
  :init-value (lambda (obj) (oset obj value git-link-default-remote))
  :reader (lambda (&rest _) (git-link--read-remote)))

(transient-define-infix git-link-dispatch--use-commit ()
  :class git-link--transient-bare-switch
  :argument "use_commit"
  ;; the value should be "use_commit" (the argument) or nil. not t
  :init-value (lambda (obj) (oset obj value (and git-link-use-commit "use_commit")))
  :description "Use commit"
  :key "c")

(transient-define-infix git-link-dispatch--line-number ()
  :class git-link--transient-bare-switch
  :argument "line_number"
  :description "Line number"
  :init-value (lambda (obj) (oset obj value (and git-link-use-single-line-number "line_number")))
  :if-not 'use-region-p
  :key "n")

;;;###autoload (autoload 'git-link-dispatch "git-link-transient" nil t)
(transient-define-prefix git-link-dispatch ()
  "Git link dispatch."
  [:description
   "Options"
   (git-link-dispatch--branch)
   (git-link-dispatch--remote)
   (git-link-dispatch--use-commit)
   (git-link-dispatch--line-number)]
  [:description
   "Git link"
   ("l" "Copy link" git-link-dispatch--copy)
   ("o" "Open in browser" git-link-dispatch--open)])

(provide 'git-link-transient)
;;; git-link-transient.el ends here
