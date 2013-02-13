(provide 'bzr-todo)

;; Run 'code' at the root of the branch which dirname is in.
(defmacro at-bzr-root (dirname &rest code)
  `(let ((default-directory (locate-dominating-file (expand-file-name ,dirname) ".bzr"))) ,@code))

(defun bzr-todo ()
  "Find all TODOs in current branch."
  (interactive)
  (let ((default-directory (concat (locate-dominating-file (expand-file-name ".") ".bzr") "/")))
    (compile "bzr todo" t)))

