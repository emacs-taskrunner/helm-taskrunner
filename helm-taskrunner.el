(require 'helm)
(require 'taskrunner)

(defvaralias 'helm-taskrunner-preferred-js-package-manager 'taskrunner-preferred-js-package-manager)
(defvaralias 'helm-taskrunner-get-all-make-targets 'taskrunner-retrieve-all-make-targets)
(defvaralias 'helm-taskrunner-leiningen-buffer-name 'taskrunner-leiningen-buffer-name)
(defvaralias 'helm-taskrunner-leiningen-task-section-regexp 'taskrunner-leiningen-task-section-header-regexp)
(defvaralias 'helm-taskrunner-gradle-taskbuffer-name 'taskrunner-gradle-tasks-buffer-name)
(defvaralias 'helm-taskrunner-gradle-heading-regexps 'taskrunner-gradle-heading-regexps)
(defvaralias 'helm-taskrunner-ant-tasks-buffer-name 'taskrunner-ant-tasks-buffer-name)

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
