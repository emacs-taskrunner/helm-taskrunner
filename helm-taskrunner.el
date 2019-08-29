;;; helm-taskrunner.el --- Retrieve build system/taskrunner tasks via helm -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Yavor Konstantinov

;; Author: Yavor Konstantinov <ykonstantinov1 AT gmail DOT com>
;; URL: https://github.com/emacs-taskrunner/helm-taskrunner
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (projectile "2.0.0") (helm "v3.0"))
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
;; (OPTIONAL) helm-projectile
;; Then put this folder in your load-path, and put this in your init:

;; (require 'helm-taskrunner)

;;;; Usage
;; When in any buffer recognized by projectile, call the command
;; `helm-taskrunner' to launch an helm menu which shows all possible
;; tasks/targets in the project.  If you add new tasks then call
;; `helm-taskrunner-update-cache' to make sure that the newly added commands
;; will be shown.  You can use the command `helm-taskrunner-task-buffers' to
;; show all buffers which were used to run a task.  If you would like to kill
;; all buffers then you can use the command `helm-taskrunner-kill-all-buffers'
;; Additionally, if you would like to rerun the last ran command, use
;; `helm-taskrunner-rerun-last-command' If you would like to see which commands
;; you have ran previously, you can call the command
;; `helm-taskrunner-command-history' which will display a history of the latest
;; ran commands.

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
(require 'helm-source)
(require 'projectile)
(require 'taskrunner)
(require 'cl-lib)

(defgroup helm-taskrunner nil
  "Group for helm-taskrunner frontend."
  :prefix "helm-taskrunner-"
  :group 'convenience)

;;;; Variables

;; Customizable variables
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

(defcustom helm-taskrunner-prompt-before-show nil
  "Whether or not to prompt the user before showing `helm-taskrunner' windon."
  :group 'helm-taskrunner
  :type 'boolean
  :options '(t nil))

(defcustom helm-taskrunner-use-fuzzy-match t
  "Variable used to enable/disable fuzzy matching for `helm-taskrunner' instances."
  :group 'helm-taskrunner
  :type 'boolean
  :options '(nil t))

(defcustom helm-taskrunner-command-history-empty-warning
  "helm-taskrunner: Command history is empty!"
  "Warning used to indicate that the command history is empty for the project."
  :group 'helm-taskrunner
  :type 'string)

(defcustom helm-taskrunner-no-custom-commands-warning
  "helm-taskrunner: There are no custom commands for this project!"
  "Warning used to indicate that there are no custom commands for the project.."
  :group 'helm-taskrunner
  :type 'string)

(defcustom helm-taskrunner-tasks-being-retrieved-warning
  "helm-taskrunner: The tasks are currently being retrieved. They will be displayed when ready."
  "Warning used to indicate that the tasks are being retrieved.
This is only used when the minor mode is on."
  :group 'helm-taskrunner
  :type 'string)

(defcustom helm-taskrunner-no-buffers-warning
  "helm-taskrunner: No taskrunner buffers are currently opened!"
  "Warning used to indicate that there are not task buffers opened."
  :group 'helm-taskrunner
  :type 'string)

;; Variable aliases for customizable variables used in the backend
(defvaralias 'helm-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'helm-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'helm-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'helm-taskrunner-build-dir-list 'taskrunner-build-dir-list)
(defvaralias 'helm-taskrunner-source-dir-list 'taskrunner-source-dir-list)
(defvaralias 'helm-taskrunner-go-task-bin-path 'taskrunner-go-task-bin-path)
(defvaralias 'helm-taskrunner-mage-bin-path 'taskrunner-mage-bin-path)
(defvaralias 'helm-taskrunner-doit-bin-path 'taskrunner-doit-bin-path)
(defvaralias 'helm-taskrunner-no-previous-command-ran-warning 'taskrunner-no-previous-command-ran-warning)
(defvaralias 'helm-taskrunner-command-history-size 'taskrunner-command-history-size)

;; Internal/non-public variables
(defvar helm-taskrunner--retrieving-tasks-p nil
  "Variable used to indicate if tasks are being retrieved in the background.")

(defvar helm-taskrunner--tasks-queried-p nil
  "Variable used to indicate if the user queried for tasks before they were ready.")

(defvar helm-taskrunner--project-cached-p nil
  "Stores the status of the project in the cache.
Used to enable prompts before displaying `helm-taskrunner'.")

(defvar helm-taskrunner--project-files '()
  "Used to store the project files and their paths.")

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
   'helm-taskrunner--select-dir-prompt
   "Create custom command"
   'helm-taskrunner--customize-command
   "Delete all custom commands"
   'helm-taskrunner-delete-all-custom-commands)
  "Actions for `helm-taskrunner'.")

(defconst helm-taskrunner-buffer-action-list
  (helm-make-actions
   "Switch to buffer"
   'switch-to-buffer
   "Kill buffer"
   'helm-taskrunner--kill-buffer
   "Kill all buffers"
   'helm-taskrunner--kill-all-buffers)
  "Actions for `helm-taskrunner' buffer list.")

;;;; Functions

;; Minor mode related

;; TODO: There might be an issue if the user switches projects too quickly(as in
;; open one project and then directly open another). This might lead to the
;; caches being corrupted.

(defun helm-taskrunner--projectile-hook-function ()
  "Collect tasks in the background when `projectile-switch-project' is called."
  (setq helm-taskrunner--retrieving-tasks-p t)
  (taskrunner-get-tasks-async (lambda (TARGETS)
                                (setq helm-taskrunner--retrieving-tasks-p nil)
                                ;; If the tasks were queried, show them to the user
                                (when helm-taskrunner--tasks-queried-p
                                  (setq helm-taskrunner--tasks-queried-p nil)
                                  (helm-taskrunner--run-helm-for-targets TARGETS)))
                              (projectile-project-root)))

;; Thanks to Marcin Borkowski for the `:init-value' tip
;; http://mbork.pl/2018-11-03_A_few_remarks_about_defining_minor_modes
;;;###autoload
(define-minor-mode helm-taskrunner-minor-mode
  "Minor mode for asynchronously collecting project tasks when a project is switched to."
  :init-value nil
  :lighter " HT"
  :global t
  ;; Add/remove the hooks when minor mode is toggled on or off
  (if helm-taskrunner-minor-mode
      (add-hook 'projectile-after-switch-project-hook #'helm-taskrunner--projectile-hook-function)
    (remove-hook 'projectile-after-switch-project-hook #'helm-taskrunner--projectile-hook-function)))

;; Functions which run tasks in a specific directory
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
  "Kill all `helm-taskrunner' task buffers.
The argument UNUSED is simply there since a Helm action requires a function with
one input."
  ;; Silence bytecompile warning. TEMP will be a string passed by helm but it is
  ;; useless since this function kills all buffers. If there this function
  ;; accepts no arguments then helm throws an error.
  UNUSED
  (taskrunner-kill-compilation-buffers))

;; https://github.com/emacs-taskrunner/ivy-taskrunner/issues/1
;; This is used to silence the bytecompiler if a user installs the package
;; without using package.el
(declare-function helm-projectile-switch-project "ext:helm-projectile")

(defun helm-taskrunner--check-if-in-project ()
  "Check if the currently visited buffer is in a project.
If it is not then prompt the user to select a project."
  (if (not (projectile-project-p))
      (if (require 'helm-projectile nil 'noerror)
          (helm-projectile-switch-project)
        (projectile-switch-project))
    t))

(defmacro helm-taskrunner--show-helm-task-instance (TARGET-LIST)
  "Show in an instance of `helm' for TARGET-LIST."
  `(helm :sources (helm-build-sync-source "helm-taskrunner-tasks"
                    :candidates ,TARGET-LIST
                    :action helm-taskrunner-action-list)
         :prompt "Task to run: "
         :buffer "*helm-taskrunner*"
         :fuzzy helm-taskrunner-use-fuzzy-match))

(defun helm-taskrunner--run-helm-for-targets (TARGETS)
  "Launch a `helm' instance with candidates TARGETS.
If TARGETS is nil then a warning is shown which mentions that no targets were found."
  (if (null TARGETS)
      (message helm-taskrunner-no-targets-found-warning)
    ;; If the user wants a prompt and the project is not cached then ask to show
    ;; when ready
    (if (and helm-taskrunner-prompt-before-show
             helm-taskrunner--project-cached-p)
        (when (y-or-n-p "Show helm-taskrunner? ")
          (helm-taskrunner--show-helm-task-instance TARGETS))
      (helm-taskrunner--show-helm-task-instance TARGETS))))

;;;###autoload
(defun helm-taskrunner ()
  "Launch `helm' to select a task which is ran in the currently visited project.
This command runs asynchronously and depending on the number of tasks which
have to be retrieved, it might take several seconds."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (progn
        (setq helm-taskrunner--project-cached-p (not (taskrunner-project-cached-p (projectile-project-root))))
        (if (and helm-taskrunner-minor-mode
                 helm-taskrunner--retrieving-tasks-p)
            (progn
              (setq helm-taskrunner--tasks-queried-p t)
              (message helm-taskrunner-tasks-being-retrieved-warning))
          (taskrunner-get-tasks-async 'helm-taskrunner--run-helm-for-targets)))
    (message helm-taskrunner-project-warning)))

;; Custom commands

(defun helm-taskrunner--customize-command (COMMAND)
  "Customize the command COMMAND and add it to cache."
  (let* ((taskrunner-program (car (split-string COMMAND " ")))
         ;; Concat the arguments since we might be rerunning a command with arguments from history
         (task-name (mapconcat 'identity
                               (cdr (split-string COMMAND " ")) " "))
         (new-task-name (read-string "Arguments to add to command: " task-name)))
    (when new-task-name
      (taskrunner-add-custom-command (projectile-project-root) (concat taskrunner-program " " new-task-name))
      (when (y-or-n-p "Run new command? ")
        (taskrunner-run-task (concat taskrunner-program " " new-task-name) (projectile-project-root) nil t)))))

(defun helm-taskrunner--delete-selected-command (COMMAND)
  "Remove the command COMMAND from the custom command cache."
  (when COMMAND
    (taskrunner-delete-custom-command (projectile-project-root) COMMAND)))

;;;###autoload
(defun helm-taskrunner-delete-custom-command ()
  "Delete a custom command and remove it from the tasks output."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((custom-tasks (taskrunner-get-custom-commands (projectile-project-root))))
        (if custom-tasks
            (helm :sources (helm-build-sync-source "helm-taskrunner-custom-commands"
                             :candidates custom-tasks
                             :action (helm-make-actions
                                      "Delete command"
                                      'helm-taskrunner--delete-selected-command))
                  :prompt "Command to remove: "
                  :buffer "*helm-taskrunner-custom-commands*"
                  :fuzzy helm-taskrunner-use-fuzzy-match)
          (message helm-taskrunner-no-custom-commands-warning)))
    (message helm-taskrunner-project-warning)))

;;;###autoload
(defun helm-taskrunner-delete-all-custom-commands (&optional _)
  "Delete all custom commands for the currently visited project."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-delete-all-custom-commands (projectile-project-root))
    (message helm-taskrunner-project-warning)))

;; Update caches
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
  "Rerun the last command/task ran in the currently visited project."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (taskrunner-rerun-last-task (projectile-project-root))
    (message helm-taskrunner-project-warning)))

;;;###autoload
(defun helm-taskrunner-task-buffers ()
  "Show all `helm-taskrunner' task buffers."
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
  "Kill all `helm-taskrunner' compilation buffers."
  (interactive)
  (taskrunner-kill-compilation-buffers))

;; Functions related to opening config files
(defun helm-taskrunner--open-file (FILENAME)
  "Open the file FILENAME.
This function is meant to be used with helm only."
  (setq helm-taskrunner--project-files  (car (alist-get (intern FILENAME) helm-taskrunner--project-files)))
  (find-file helm-taskrunner--project-files))

(defun helm-taskrunner--select-system (SYS)
  "Retrive the files for the taskrunner/build system SYS."
  (setq helm-taskrunner--project-files (car (alist-get (intern SYS) helm-taskrunner--project-files)))
  ;; (message "%s %s" SYS helm-taskrunner--project-files)
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

;; Functions related to command history
;;;###autoload
(defun helm-taskrunner-command-history ()
  "Show the command history for the currently visited project."
  (interactive)
  (helm-taskrunner--check-if-in-project)
  (if (projectile-project-p)
      (let ((commands-ran (taskrunner-get-commands-from-history (projectile-project-root))))
        (if commands-ran
            (helm
             :sources (helm-build-sync-source "helm-taskrunner-command-history"
                        :candidates commands-ran
                        :action 'helm-taskrunner-action-list)
             :prompt "Command to run: "
             :buffer "*helm-taskrunner-command-history*"
             :fuzzy helm-taskrunner-use-fuzzy-match)
          (message helm-taskrunner-command-history-empty-warning)))
    (message helm-taskrunner-project-warning)))

;; Cache cleanup
(defun helm-taskrunner-clean-up-projects ()
  "Remove all projects which do not exist anymore from the caches.
This command will overwrite the cache file."
  (interactive)
  (taskrunner-clean-up-projects))

;; Notifications

;; If the compilation function is present then that means that the Emacs using
;; this package has notifications
(when (fboundp 'taskrunner--compilation-notification)
  (defun helm-taskrunner-notifications-on ()
    "Turn on `helm-taskrunner' desktop notifications when a task is finished."
    (interactive)
    (taskrunner-notification-on))

  (defun helm-taskrunner-notifications-off ()
    "Turn off `helm-taskrunner' desktop notifications when a task is finished."
    (interactive)
    (taskrunner-notification-off)))

(provide 'helm-taskrunner)
;;; helm-taskrunner.el ends here
