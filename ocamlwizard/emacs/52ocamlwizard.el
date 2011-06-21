;(require 'ocamlwizard)

;(add-to-list 'auto-mode-alist '("\\.ml[i]?\\'" . ocamlwizard))
(autoload 'ocamlwizard "ocamlwizard" "OCamlwizard enhancements" t)
(add-hook 'tuareg-mode-hook 'ocamlwizard)
