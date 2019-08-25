;;; helm-taskrunner.el --- Retrieve build system/taskrunner tasks via helm -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/helm-taskrunner
;; Version: 1.0
;; Package-Requires: ((emacs "24"))
;; Keywords: build-system taskrunner build task-runner tasks helm convenience

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
;; When in any buffer recognized by projectile, call the command
;; `helm-taskrunner' to launch an helm menu which shows all possible tasks/targets
;; in the project.  If you add new tasks then call `helm-taskrunner-update-cache'
;; to make sure that the newly added commands will be shown.  You can use the
;; command `helm-taskrunner-task-buffers' to show all buffers which were used to
;; run a task Additionally, if you would like to rerun the last ran command, use
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
(require 'cl-lib)

(defgroup helm-taskrunner nil
  "Group for helm-taskrunner frontend."
  :group 'convenience)

;;;; Variables

(defcustom helm-taskrunner-project-warning
  "helm-taskrunner: The currently visited buffer must be in a project in order to select a task!
Please switch to a project which is recognized by projectile!"
  "Warning used to indicate that the user is currently visiting a project."
  :group 'helm-taskrunner
  :type 'string)

(defcustom helm-taskrunner-no-targets-found-warning
  "helm-taskrunner: No targets found in the current project!"
  "Warning used to indicate that no targets were found."
  :group 'helm-taskrunner
  :type 'string)

(defcustom helm-taskrunner-no-files-found-warning
  "helm-taskrunner: There are no configuration files for any taskrunner/build system in the current project."
  "Warning used to indicate that no configuration files were found in the current project."
  :group 'helm-taskrunner
  :type 'string)

(defcustom helm-taskrunner-use-fuzzy-match t
  "Variable used to enable/disable fuzzy matching for helm-taskrunner instances."
  :group 'helm-taskrunner
  :type 'boolean
  :options '(nil t))

(defconst helm-taskrunner-no-buffers-warning
  "helm-taskrunner: No taskrunner buffers are currently opened!"
  "Warning used to indicate that there are not task buffers opened.")

(defvaralias 'helm-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'helm-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'helm-taskrunner-leiningen-buffer-name 'taskrunner-leiningen-buffer-name)
(defvaralias 'helm-taskrunner-leiningen-task-section-regexp 'taskrunner-leiningen-task-section-header-regexp)
(defvaralias 'helm-taskrunner-gradle-taskbuffer-name 'taskrunner-gradle-tasks-buffer-name)
(defvaralias 'helm-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'helm-taskrunner-ant-tasks-buffer-name 'taskrunner-ant-tasks-buffer-name)

(defconst helm-taskrunner-action-list
  (helm-make-actions
   "Run task in root without args"
   'helm-taskrunner--root-task
   "Run task in root and prompt for args"
   'helm-taskrunner--root-task-prompt
   "Run task in current directory without args"
   'helm-taskrunner--current-dir
   "Run task in current directory and prompt for args"
   'helm-taskrunner--current-dir-prompt
   "Run task in another directory"
   'helm-taskrunner--select-dir
   "Run task in another directory and prompt for args"
   'helm-taskrunner--select-dir-prompt)
  "Actions for helm-taskrunner.")

(defconst helm-taskrunner-buffer-action-list
  (helm-make-actions
   "Switch to buffer"
   'switch-to-buffer
   "Kill buffer"
   'helm-taskrunner--kill-buffer
   "Kill all buffers"
   'helm-taskrunner--kill-all-buffers)
  "Actions for helm-taskrunner buffer list.")

(defvar helm-taskrunner--project-files '()
  "Used to store the project files and their paths.")

;;;; Functions

(defun helm-taskrunner--root-task (TASK)
  "Run the task TASK in the project root without asking for extra args.
This is the default command when selecting/running a task/target."
  (taskrunner-run-task TASK nil nil t))

(defun helm-taskrunner--root-task-prompt (TASK)
  "Run the task TASK in the project root and ask the user for extra args."
  (taskrunner-run-task TASK nil t t))

(defun helm-taskrunner--current-dir (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Do not prompt the user to supply any extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) nil t))))

(defun helm-taskrunner--current-dir-prompt (TASK)
  "Run the task TASK in the directory visited by the current buffer.
Prompt the user to supply extra arguments."
  (let ((curr-file (buffer-file-name)))
    (when curr-file
      (taskrunner-run-task TASK (file-name-directory curr-file) t t))))

(defun helm-taskrunner--select-dir (TASK)
  "Run the task TASK in a directory chosen by the user."
  (let ((command-directory (read-directory-name "Directory: " (projectile-project-root))))
    (message command-directory)
    (when command-directory
      (taskrunner-run-task TASK command-directory nil t))))

(defun helm-taskrunner--select-dir-prompt (TASK)
  "Run the task TASK in a directory chosen by the user.
Prompt the user to supply extra arguments."
  (let ((command-directory (read-directory-name "Directory: " (projectile-project-root))))
    (when command-directory
      (taskrunner-run-task TASK command-directory t t))))

(defun helm-taskrunner--kill-buffer (BUFFER-NAME)
  "Kill the buffer name BUFFER-NAME."
  (kill-buffer BUFFER-NAME))

(defun helm-taskrunner--kill-all-buffers (UNUSED)
  "Kill all helm-taskrunner task buffers.
The argument UNUSED is simply there since a Helm action requires a function with
one input."
  ;; Silence bytecompile warning. TEMP will be a string passed by helm but it is
  ;; useless since this function kills all buffers. If there this function
  ;; accepts no arguments then helm throws an error.
  UNUSED
  (taskrunner-kill-compilation-buffers))


(defun helm-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not then prompt the user to select a project."
  (if (not (projectile-project-p))
      (if (package-installed-p 'helm-projectile)
          (progn
            (require 'helm-projectile)
            (helm-projectile-switch-project))
        (projectile-switch-project))
    t))

(defun helm-taskrunner--run-helm-for-targets (TARGETS)
  "Launch a Helm instance with candidates TARGETS.
If TARGETS is nil then a warning is shown which mentions that no targets were found."
  (if (null TARGETS)
      (message helm-taskrunner-no-targets-found-warning)
    (helm :sources (helm-build-sync-source "helm-taskrunner-tasks"
                     :candidates TARGETS
                     :action helm-taskrunner-action-list)
          :prompt "Task to run: "
          :buffer "*helm-taskrunner*"
          :fuzzy helm-taskrunner-use-fuzzy-match)))

;;;###autoload
(defun helm-taskrunner ()
  "Launch helm to select a task which is ran in the currently visited project.
This command runs asynchronously and depending on the number of tasks which
have to be retrieved, it might take several seconds."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-get-tasks-async 'helm-taskrunner--run-helm-for-targets)
    (message helm-taskrunner-project-warning)))

;;;###autoload
(defun helm-taskrunner-update-cache ()
  "Refresh the task cache for the current project and show all tasks."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-refresh-cache-async 'helm-taskrunner--run-helm-for-targets)
    (message helm-taskrunner-project-warning)))

;;;###autoload
(defun helm-taskrunner-rerun-last-command ()
  "Rerun the last task ran in the currently visited project."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-rerun-last-task (projectile-project-root))
    (message helm-taskrunner-project-warning)))

;;;###autoload
(defun helm-taskrunner-task-buffers ()
  "Show all helm-taskrunner task buffers."
  (interactive)
  (let ((taskrunner-buffers (taskrunner-get-compilation-buffers)))
    (if taskrunner-buffers
        (helm :sources (helm-build-sync-source "helm-taskrunner-buffer-source"
                         :candidates taskrunner-buffers
                         :action helm-taskrunner-buffer-action-list)
              :prompt "Buffer to open: "
              :buffer "*helm-taskrunner-buffers*"
              :default 'switch-to-buffer)
      (message helm-taskrunner-no-buffers-warning))))

;;;###autoload
(defun helm-taskrunner-kill-all-buffers ()
  "Kill all helm-taskrunner compilation buffers."
  (interactive)
  (taskrunner-kill-compilation-buffers))

(defun helm-taskrunner--open-file (FILENAME)
  "Open the file FILENAME.
This function is meant to be used with helm only."
  (setq helm-taskrunner--project-files  (car (assoc (intern FILENAME) helm-taskrunner--project-files)))
  (find-file helm-taskrunner--project-files))

(defun helm-taskrunner--select-system (SYS)
  "Retrive the files for the taskrunner/build system SYS."
  (setq helm-taskrunner--project-files   (car (assoc (intern SYS) helm-taskrunner--project-files)))
  (if (stringp helm-taskrunner--project-files)
      (find-file helm-taskrunner--project-files)
    (helm
     :sources (helm-build-sync-source "helm-taskrunner-files-source"
                :candidates (cl-map 'list (lambda (elem)
                                            (car elem))
                                    helm-taskrunner--project-files)
                :action '(("Select file: " . helm-taskrunner--open-file)))
     :prompt "Select a file: "
     :buffer "*helm-taskrunner-files*"
     :default 'helm-taskrunner--open-file)))

;;;###autoload
(defun helm-taskrunner-config-files ()
  "Open the configuration files(if any are present) at project root."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (setq helm-taskrunner--project-files (taskrunner-collect-taskrunner-files (projectile-project-root)))
  (if helm-taskrunner--project-files
      (helm
       :sources (helm-build-sync-source "helm-taskrunner-files-source"
                  :candidates (cl-map 'list (lambda (elem)
                                              (car elem))
                                      helm-taskrunner--project-files)
                  :action '(("Select build system" . helm-taskrunner--select-system)))
       :prompt "Select a taskrunner: "
       :buffer "*helm-taskrunner-files*"
       :default 'helm-taskrunner--get-config-file-paths)
    (message helm-taskrunner-no-files-found-warning)))

(provide 'helm-taskrunner)
;;; helm-taskrunner.el ends here
