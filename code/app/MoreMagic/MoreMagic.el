(defun more-magic (beg end len)
  (progn
    (basic-save-buffer)
    (shell-command
     (concat "/home/morten/studie/turtledove/code/app/MoreMagic/MoreMagic "
             (buffer-file-name)
             )
     )
    (revert-buffer t t)
    (message "%s" "Foobar")
    (set (make-local-variable 'after-change-functions) 'more-magic)
    )
  )

(set (make-local-variable 'after-change-functions) 'more-magic)
;; (add-hook 'after-change-functions 'more-magic)