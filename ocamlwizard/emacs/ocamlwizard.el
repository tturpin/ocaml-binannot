;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;  Ocamlwizard                                                           ;
;  David Baudet and Mohamed Iguernelala                                  ;
;  Copyright 2008 INRIA Saclay - Ile-de-France                           ;
;                                                                        ;
;  This software is free software; you can redistribute it and/or        ;
;  modify it under the terms of the GNU Library General Public           ;
;  License version 2.1, with the special exception on linking            ;
;  described in file LICENSE.                                            ;
;                                                                        ;
;  This software is distributed in the hope that it will be useful,      ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ocamlwizard-locate ()
  "locate declaration/definition of an Ocaml identifier using Ocamlwizard"
  (interactive)
  (save-excursion
    (search-backward-regexp "[^a-zA-Z._0-9]")
    (forward-char 1)
    (setq start (point))
    (search-forward-regexp "[^a-zA-Z._0-9]")
    (setq end (- (point) 1))
    (setq word (buffer-substring start end)))
  (setq file (buffer-name))
  (setq buffer (get-buffer-create "*ocamlwizard*"))
  (save-excursion
    (set-buffer buffer)
    (compilation-minor-mode 1)
    (erase-buffer)
    (insert "\n\n"))
  (call-process 
   "ocamlwizard" nil buffer nil
   "locate" word  
   (concat (int-to-string (- start 1)) "-" (int-to-string (- end 1)))
   file)
  (next-error)
)


(defun ocamlwizard-match-with-completion ()
  "complete an Ocaml match construct using Ocamlwizard"
  (interactive)
;  (save-excursion
    (setq end (point))
    (re-search-backward "[^ \t\n]")
    (forward-char 1)
    (setq start (point))
    (delete-char (- end start))
;)
  (do-auto-save)
  (setq exit-status 
	(call-process "ocamlwizard" nil (list t nil) nil "completion" "-printer" "ocaml-pp" "-pos"  
		      (int-to-string (- (point) 1)) (buffer-name)))
  (if (not (eq exit-status 10))
      (message "ocamlwizard: no completion"))
  (search-backward "$")
  (delete-char 1)
)


(defun ocamlwizard-path-completion ()
  "complete an Ocaml identifier using Ocamlwizard"
  (interactive)
  (setq buffer (get-buffer-create "*ocamlwizard*"))
  (save-excursion
    (set-buffer buffer)
    (erase-buffer))
  (do-auto-save)
  (setq exit-status 
	(call-process "ocamlwizard" nil buffer nil "completion" "-printer" "ocaml-pp" "-pos"  
		      (int-to-string (- (point) 1)) (buffer-name)))
  (message (concat "ocamlwizard: exit-status=" (int-to-string exit-status)))
  (save-excursion
    (set-buffer buffer)
    (let* ((text (buffer-string))
	  (list (split-string text "[\n]+")))
      ;(message (int-to-string (length list)))
      (setq alist (mapcar (function (lambda (x) (list x x))) list))
      ;(message (int-to-string (length alist)))
      ))
  (setq choice 
	(completing-read "possible completion: " alist nil t))
;  (backward-kill-word 1)
  (insert choice)
)


(defun ocamlwizard-expand-patvar ()
  "expands a pattern-matching variable using Ocamlwizard"
  (interactive)
  (save-excursion
    (search-backward-regexp "[^a-zA-Z._0-9]")
    (forward-char 1)
    (setq start (point))
    (search-forward-regexp "[^a-zA-Z._0-9]")
    (setq end (- (point) 1))
    (setq word (buffer-substring start end))
    (search-forward "->")
    (setq arrow (point)))
  (setq file (buffer-name))
  (setq buffer (get-buffer-create "*ocamlwizard*"))
  (save-excursion
    (set-buffer buffer)
    (compilation-minor-mode 1)
    (erase-buffer)
    (insert "\n\n"))
  (call-process 
   "ocamlwizard" nil buffer nil
   "completion" "-printer" "ocaml-pp" "-pos"  (int-to-string (- arrow 1))
   "-expand" (concat (int-to-string (- start 1)) "-" (int-to-string (- end 1)))
   file)
)


(defun ocamlwizard-expand-patvar ()
  "expands a pattern-matching variable using Ocamlwizard"
  (interactive)
  (save-excursion
    (search-backward-regexp "[^a-zA-Z._0-9]")
    (forward-char 1)
    (setq start (point))
    (search-forward-regexp "[^a-zA-Z._0-9]")
    (setq end (- (point) 1))
    (setq word (buffer-substring start end))
    (search-forward "->")
    (setq arrow (point)))
  (setq file (buffer-name))
  (setq buffer (get-buffer-create "*ocamlwizard*"))
  (save-excursion
    (set-buffer buffer)
    (compilation-minor-mode 1)
    (erase-buffer)
    (insert "\n\n"))
  (do-auto-save)
  (goto-char start)
  (delete-char (- end start))
  (setq exit-status 
	(call-process 
	 "ocamlwizard" nil (list t nil) nil
	 "completion" "-printer" "ocaml-pp" "-pos"  (int-to-string (- arrow 1))
	 "-expand" (concat (int-to-string (- start 1)) "-" (int-to-string (- end 1)))
	 file))
  (if (not (eq exit-status 10))
      (message "ocamlwizard: no completion"))
  (search-backward "$")
  (delete-char 1)
)

(defun ocamlwizard-rename (name)
  "rename a value using Ocamlwizard"
  (interactive "sRename with: ")
  (setq pos (point))
;  (save-excursion
;    (search-backward-regexp "[^a-zA-Z._0-9]")
;    (forward-char 1)
;    (setq start (point))
;    (search-forward-regexp "[^a-zA-Z._0-9]")
;    (setq end (- (point) 1))
;    (setq word (buffer-substring start end)))
  (setq file (buffer-name))
  (setq buffer (get-buffer-create "*ocamlwizard*"))
  (save-excursion
    (set-buffer buffer)
    (compilation-minor-mode 1)
    (erase-buffer))
  (do-auto-save)
  (setq exit-status 
	(call-process 
	 "ocamlwizard" nil buffer nil
	 "completion" "refactor" "-rename"
	 (concat (int-to-string (- pos 1)) "-" (int-to-string pos))
	 name
	 file))
  (if (eq exit-status 0)
      ; Thank you stackoverflow:
      (progn
	(save-excursion
	  (clear-visited-file-modtime)
	  (widen)
	  (delete-region (point-min) (point-max))
	  (insert-file-contents (buffer-file-name)))
       ; Problem: if we undo and then redo, emacs forgets the goto.
	(goto-char pos)
	(set-buffer-modified-p nil)
	(set-visited-file-modtime)))
  (display-message-or-buffer buffer)
  )

(defun ocamlwizard ()
  (interactive)
  (define-key (current-local-map) [f1] 'ocamlwizard-locate)
  (define-key (current-local-map) [f3] 'ocamlwizard-match-with-completion)
  (define-key (current-local-map) [f12] 'ocamlwizard-path-completion)
  (define-key (current-local-map) [f11] 'ocamlwizard-expand-patvar)
  (define-key (current-local-map) "\C-c\C-or" 'ocamlwizard-rename)
  )
