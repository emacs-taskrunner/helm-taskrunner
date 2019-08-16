;;; helm-taskrunner.el --- Retrieve build system/taskrunner tasks via helm -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/helm-taskrunner
;; Version: 1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: build-system taskrunner build task-runner tasks ivy

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package provides an helm interface to the taskrunner library

;;;; Installation

;;;;; MELPA
;; If installed form MELPA then simply add:
;; (require 'helm-taskrunner) to your init.el

;;;;; Manual

;; Install these required packages:

;; projectile
;; taskrunner
;; helm

;; Then put this folder in your load-path, and put this in your init:

;; (require 'helm-taskrunner)

;;;; Usage

;; When in any buffer, either call the command `helm-taskrunner' or
;; `helm-taskrunner-update-cache' to be presented with a list of targets/tasks in
;; your project.
;; Additionally, if you would like to rerun the last ran command, use
;; `helm-taskrunner-rerun-last-command'.

;;;; Credits

;; This package would not have been possible without the following
;; packages:
;; helm [1] which helped me create the interface
;;
;; [1] https://github.com/emacs-helm/helm

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'helm)
(require 'taskrunner)

;;;; Customizable
(defvaralias 'helm-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'helm-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'helm-taskrunner-leiningen-buffer-name 'taskrunner-leiningen-buffer-name)
(defvaralias 'helm-taskrunner-leiningen-task-section-regexp 'taskrunner-leiningen-task-section-header-regexp)
(defvaralias 'helm-taskrunner-gradle-taskbuffer-name 'taskrunner-gradle-tasks-buffer-name)
(defvaralias 'helm-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'helm-taskrunner-ant-tasks-buffer-name 'taskrunner-ant-tasks-buffer-name)

;;;; Functions

(defun helm-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not then prompt the user to select a project."
  (let ((in-project-p (projectile-project-p)))
    (when (not in-project-p)
      (if (package-installed-p 'helm-projectile)
          (helm-projectile-switch-project)
        (projectile-switch-project))
      )
    )
  )

(defun helm-taskrunner ()
  "Launch helm to select a task which is ran in the currently visited project."
  (interactive)
  (helm-taskrunner--check-if-in-project)

  (when (projectile-project-p)
    (helm :sources (helm-build-sync-source "helm-taskrunner-tasks"
                     :candidates (taskrunner-get-tasks-from-cache))
          :prompt "Task to run: "
          :buffer "*helm taskrunner*")
    )
  )

(defun helm-taskrunner-rerun-last-command ()
  "Rerun the last task ran in the currently visited project."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (when (projectile-project-p)
    (taskrunner-rerun-last-task (projectile-project-root))
    )
  )

(provide 'helm-taskrunner)
;;; helm-taskrunner.el ends here
