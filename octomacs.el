;;; octomacs.el --- Octopress interface for Emacs
;;
;; Copyright (C) 2012 Jacob Helwig <jacob@technosorcery.net>
;;
;; Author: Jacob Helwig <jacob@technosorcery.net>
;; Homepage: http://technosorcery.net
;; Version: 0.0.1
;; URL: https://github.com/jhelwig/octomacs
;;
;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; octomacs.el provides an interface for interacting with Octopress
;; (http://octopress.org/).
;;
;; Add the following to your .emacs:
;;
;;     (add-to-list 'load-path "/path/to/octomacs")
;;     (require 'octomacs)
;;
;; Configuring Octomacs will make interaction a little nicer.
;; Specifically, setting `octomacs-workdir-alist'.
;;
;;     M-x customize-group RET octomacs RET
;;
;; If rvm.el is installed, Octomacs will attempt to use it whenever it
;; needs to run a command in the Octopress directory.
;;
;; If ido.el is installed, Octomacs will use it for prompting of which
;; Octopress project to use.
;;
;; Calling the interactive function `octomacs-new-post' will prompt
;; for which project to use (configurable from
;; `octomacs-workdir-alist', or can be manually specified as a path to
;; an Octopress instance), and for the title of the post to create.
;;

;;; Code:

(defgroup octomacs nil "Octopress interface for Emacs."
  :group 'tools)

(defcustom octomacs-workdir-alist nil
  "*The locations of Octopress working directories."
  :group 'octomacs
  :type '(repeat (cons (string :tag "Instance name")
                       (file   :tag "Location"))))

;;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar octomacs-workdir-history nil
  "Project names & directories recently used with `octomacs'.")

(defun octomacs-read-project ()
  "Prompt for a project name (as defined in `octomacs-workdir-alist') or a directory name.
Returns the directory as defined in `octomacs-workdir-alist' or
the specified directory name.  Passes the directory through
`expand-file-name', and `directory-file-name'."
  (let (project-or-dir)
    (setq project-or-dir (if (featurep 'ido)
                             (ido-completing-read
                              "Choose Octopress project: "
                              (delete-dups (append
                                            (mapcar 'car octomacs-workdir-alist)
                                            octomacs-workdir-history))
                              nil
                              nil
                              nil
                              'octomacs-workdir-history
                              (car octomacs-workdir-history)
                              nil)
                           (completing-read
                            (format
                             "Choose Octopress project (default %s): "
                             (car octomacs-workdir-history))
                            (delete-dups (append
                                          (mapcar 'car octomacs-workdir-alist)
                                          octomacs-workdir-history))
                            nil
                            nil
                            nil
                            (cons 'octomacs-workdir-history 1)
                            (car octomacs-workdir-history)
                            nil)
                           ))
    (directory-file-name (expand-file-name (if (assoc project-or-dir octomacs-workdir-alist)
                                               (cdr (assoc project-or-dir octomacs-workdir-alist))
                                             project-or-dir)))))

(defun octomacs-read-post-name ()
  "Prompt for the title to use for a new post."
  (read-string "New post title: "))

(defun octomacs-new-post-interactive ()
  "Return the (interactive) arguments for `octomacs-new-post'."
  (let* ((project (octomacs-read-project))
         (post-name (octomacs-read-post-name)))
    (list post-name project)))

(defun octomacs-rake (directory task &rest arguments)
  "Run rake task TASK with specified ARGUMENTS in DIRECTORY"
  (if (featurep 'rvm)
      (octomacs-rake-with-rvm directory task arguments)
    (octomacs-rake-without-rvm directory task arguments)))

(defun octomacs-bg-rake (directory task &rest arguments)
  "Run rake task TASK with specified ARGUMENTS in DIRECTORY"
  (if (featurep 'rvm)
      (octomacs-bg-rake-with-rvm directory task arguments)
    (octomacs-bg-rake-without-rvm directory task arguments)))

(defun octomacs-shell-escape-string (string)
  "Escape single quotes in STRING."
  (replace-regexp-in-string "'" "'\\\\''" string))

(defun octomacs-format-rake-task-with-args (task &optional arguments)
  "Build a shell suitable string of the rake TASK name with the specified ARGUMENTS."
  (let ((arguments-string (if arguments
                              (format "[%s]" (if (listp arguments) (mapconcat 'octomacs-shell-escape-string arguments ", ") arguments))
                            "")))
    (format "'%s%s'" task arguments-string)))

(defun octomacs-rake-with-rvm (directory task &optional arguments)
  "Run rake task TASK with specified ARGUMENTS in DIRECTORY using rvm"
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (rvmrc-info (or (rvm--load-info-rvmrc) (rvm--load-info-ruby-version) (rvm--load-info-gemfile)))
         (rvm-command (if rvmrc-info
                          (format "%s %s do" rvm-executable (mapconcat 'identity rvmrc-info rvm--gemset-separator))
                        "")))
    (shell-command-to-string (format "%s rake %s" rvm-command (octomacs-format-rake-task-with-args task arguments)))))

(defun octomacs-rake-without-rvm (directory task &optional arguments)
  "Run rake task TASK with specified ARGUMENTS in DIRECTORY"
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    (shell-command-to-string (format "rake %s" (octomacs-format-rake-task-with-args task arguments)))))

(defun octomacs-bg-rake-with-rvm (directory task &optional arguments)
  "Run rake task TASK with specified ARGUMENTS in DIRECTORY using rvm"
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (rvmrc-info (or (rvm--load-info-rvmrc) (rvm--load-info-ruby-version) (rvm--load-info-gemfile)))
	 (rvm-command (if rvmrc-info
                          (format "%s %s do" rvm-executable (mapconcat 'identity rvmrc-info rvm--gemset-separator))
                        "")))
    (start-process-shell-command (format "octomacs-rake %s" task)
				 (format "*octomacs-process* %s" task)
				 (shell-command-to-string (format "%s rake %s"
								  rvm-command
								  (octomacs-format-rake-task-with-args task arguments))))))

(defun octomacs-bg-rake-without-rvm (directory task &optional arguments)
  "Run rake task TASK with specified ARGUMENTS in DIRECTORY"
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    (start-process-shell-command (format "octomacs-rake %s" task)
				 (format "*octomacs %s*" task)
				 (format "rake %s" (octomacs-format-rake-task-with-args task arguments)))))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun octomacs-new-post (post-name directory)
  "Create a post called POST-NAME in the Octopress work tree in DIRECTORY"
  (interactive (octomacs-new-post-interactive))
  (let* ((octopress-directory (file-name-as-directory (expand-file-name directory)))
         (rake-output (octomacs-rake octopress-directory "new_post" post-name))
         (rake-output-match-pos (string-match "Creating new post: " rake-output))
         (file-name (if rake-output-match-pos
                        (concat octopress-directory (replace-regexp-in-string "\n" "" (substring rake-output (match-end 0))))
                      nil)))
    (if file-name
        (find-file file-name)
      (message (concat "Unable to create post: " rake-output)))))

;;;###autoload
(defun octomacs-deploy (directory &optional generatep)
  "untested function"
  (interactive (list (octomacs-read-project) "p"))
  (let ((octopress-directory (file-name-as-directory (expand-file-name directory))))
    (case generatep
      ((4) (octomacs-bg-rake octopress-directory "gen_deploy"))
      (t   (octomacs-bg-rake octopress-directory "deploy")))))

;;;###autoload
(defun octomacs-preview (directory &optional generatep)
  "untested function"
  (interactive (list (octomacs-read-project) "p"))
  (let ((octopress-directory (file-name-as-directory (expand-file-name directory))))
    (case generatep
      ((4) (octomacs-bg-rake octopress-directory "generate preview"))
      (t   (octomacs-bg-rake  octopress-directory "preview")))
    (browse-url "http://localhost:4000")))
(provide 'octomacs)

;;;###autoload
(defun octomacs-edit-post (post)
  )

;;; octomacs.el ends here
