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

(global-set-key [f1] 'ocamlwizard-locate)

(defun ocamlwizard-match-with-completion ()
  "complete an Ocaml match construct using Ocamlwizard"
  (interactive)
  (setq exit-status 
	(call-process "ocamlwizard" nil (list t nil) nil "completion" "-printer" "ocaml-pp" "-pos"  
		      (int-to-string (- (point) 1)) (buffer-name)))
  (if (not (eq exit-status 10))
      (message "ocamlwizard: no completion"))
)

(global-set-key [f3] 'ocamlwizard-match-with-completion)

(defun ocamlwizard-path-completion ()
  "complete an Ocaml identifier using Ocamlwizard"
  (interactive)
  (setq buffer (get-buffer-create "*ocamlwizard*"))
  (save-excursion
    (set-buffer buffer)
    (erase-buffer))
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

(global-set-key [f12] 'ocamlwizard-path-completion)

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
)

(global-set-key [f11] 'ocamlwizard-expand-patvar)
