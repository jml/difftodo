(provide 'bzr-todo)

(defun search-upwards (filename starting-path)
  "Search for `filename' in every directory from `starting-path' up."
  (defun parent-dir (path)
    (file-name-directory (directory-file-name path)))

  (let ((path (file-name-as-directory starting-path)))
    (if (file-exists-p (concat path filename))
        path
      (let ((parent (parent-dir path)))
        (if (string= parent path)
            nil
          (search-upwards filename parent))))))


(defmacro with-cd (dirname &rest code)
  `(let ((old-dirname default-directory)
	 (start-buffer (current-buffer)))
     (cd ,dirname)
     (unwind-protect (progn ,@code)
       (let ((end-buffer (current-buffer)))
	 ;; (cd ,dirname)
	 (set-buffer start-buffer)
	 (cd old-dirname)
	 (set-buffer end-buffer)))))


(defun branch-todo ()
  (interactive)
  (with-cd (search-upwards ".bzr" (expand-file-name "."))
   (compile "bzr todo" t)))
