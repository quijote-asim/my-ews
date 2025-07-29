;;; init.el --- Emacs Writing Studio init -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/emacs-writing-studio/
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;; Emacs Writing Studio init file: https://lucidmanager.org/tags/emacs
;;
;; This init file is tangled from: documents/ews-book/99-appendix.org
;;
;; This file is a starter kit for developing a configuration and is not a package
;; that is regularly updated.
;;
;;; Code:

;; Emacs 29 available?

(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires version 29 or later"))

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name
			     "custom.el"
			     user-emacs-directory))

(load custom-file :no-error-if-file-is-missing)

;; Bind key for customising variables

(keymap-global-set "C-c w v" 'customize-variable)

;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))

;; Load EWS functions

(load-file (concat (file-name-as-directory user-emacs-directory)
		   "ews.el"))

;; Check for missing external software

(ews-missing-executables
 '(("gs" "mutool")
   "pdftotext"
   "soffice"
   "zip"
   "ddjvu"
   "curl"
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc") 
   ("grep" "ripgrep")
   ("convert" "gm")
   "dvipng"
   "latex"
   "hunspell"
   "git"
   "trans"))

;;; LOOK AND FEEL

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; segui automáticamente los enlaces simbólicos
;; no pregunta cuando el archivo está bajo control de versiones

(setq vc-follow-symlinks t)

;; Poner el fondo transparente
(set-frame-parameter (selected-frame) 'alpha '(92 . 90))
(add-to-list 'default-frame-alist '(alpha . (92 . 90)))

;; Para maximizr al arrancar

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Short answers only please

(setq-default use-short-answers t)

;; Desactiva C-z y C-x z para evitar ejecutar suspend-frame por error

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Scratch buffer settings

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+title: Emacs Writing Studio\n#+subtitle: Scratch Buffer\nThe text in this buffer is not saved when exiting Emacs!\n\n")

;; Spacious padding

(use-package spacious-padding
  :custom
  (line-spacing 3)
  (spacious-padding-mode 1))

;; Modus and EF Themes

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(modus-operandi-tinted
			    modus-vivendi-tinted))
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t m" . modus-themes-select)
   ("C-c w t s" . consult-theme)))

(use-package ef-themes)

;; like `load-theme' but also call `ef-themes-post-load-hook'
(ef-themes-select 'ef-dark)

;; Mixed-pitch mode

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; Window management
;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; Ajustes del calendario

;; formato de fecha y semana comienza en lunes
(setq calendar-date-style 'european
      calendar-week-start-day 1)

;; Nombres de dias y meses en castellano
(setq calendar-day-header-array ["Do" "Lu" "Ma" "Mi" "Ju" "Vi" "Sá"])
(setq calendar-day-name-array ["domingo" "lunes" "martes" "miércoles" "jueves" "viernes" "sábado"])
(setq calendar-month-abbrev-array ["Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"])
(setq calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"])

;;; MINIBUFFER COMPLETION

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

(use-package savehist
  :init
  (savehist-mode))

;; Search for partial matches in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

;; Contextual menu with right mouse button

(when (display-graphic-p)
  (context-menu-mode))

;; Improved help buffers

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

;;; Text mode settings

(use-package text-mode
  :ensure
  nil
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

;; Check spelling with flyspell and hunspell

(use-package flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary (mapconcat 'identity ews-hunspell-dictionaries ","))
  (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

;;; Ricing Org mode

(use-package org
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'error)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-id-link-to-org-use-id t)
  (org-fold-catch-invisible-edits 'show) 
;; Otros ajustes para org-mode
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-reverse-note-order t))

;; Configuración de mi flujo de trabajo
;; Configuración de TODO en org-mode
(setq org-todo-keywords
      '((sequence "Tarea:(t!)" "Acción:(a!)" "Esperando:(e@/!)" "Hito:(h!)" "Parado:(p@/!)" "|" "Hecho:(H@!)" "Cancelado:(c@/!)")))

;; Colores para los estados de TODO
(setq org-todo-keyword-faces
      '(("Tarea" . (:foreground "magente" :weight bold))
        ("Acción" . (:foreground "red" :weight bold))
        ("Esperando" . (:foreground "gray" :weight bold))
        ("Hito" . (:foreground "green" :weight bold))
        ("Parado" . (:foreground "gray" :weight bold))
        ("Hecho" . (:foreground "green" :weight bold))
        ("Cancelado" . (:foreground "yellow" :weight bold))))

;; Show hidden emphasis markers

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Org modern: Most features are disabled for beginning users

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-table nil)
  (org-modern-keyword nil)
  (org-modern-timestamp nil)
  (org-modern-priority nil)
  (org-modern-checkbox nil)
  (org-modern-tag nil)
  (org-modern-block-name nil)
  (org-modern-keyword nil)
  (org-modern-footnote nil)
  (org-modern-internal-target nil)
  (org-modern-radio-target nil)
  (org-modern-statistics nil)
  (org-modern-progress nil))

;; INSPIRATION

;; Doc-View

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Managing Bibliographies

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Citar to access bibliographies

(use-package citar
  :defer t
  :custom
  (citar-bibliography ews-bibtex-files)
  :bind
  (("C-c w b o" . citar-open)))

;; Read RSS feeds with Elfeed

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory (getenv "HOME"))
		 "org/bujo/elfeed.org"))))

;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Emacs Multimedia System

(use-package emms
  :config
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("C-c w m b" . emms-browser)
   ("C-c w m e" . emms)
   ("C-c w m p" . emms-play-playlist )
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

;; Open files with external applications

(use-package openwith
  :config
  (openwith-mode t)
  :custom
  (openwith-associations nil))

;; Fleeting notes
(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-goto-interface 'outline-path-completion)
  (org-capture-templates
   '(("d" "Nota al diario 📆"
      plain (file+datetree ql-journal-file )
      "+  %?"
      :empty-lines 1)
     ("t" "Tarea" entry (file+headline ql-tasks-file "Tareas")
      "* Tareas: %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
      :empty-lines 1)
     )))

;; Denote

(use-package denote
  :defer t
  :custom
  (denote-sort-keywords t)
  (denote-link-description-function #'ews-denote-link-description-title-case) ; eliminada para arreglar el formato de los títulos
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c w d b" . denote-find-backlink)
   ("C-c w d d" . denote-date)
   ("C-c w d l" . denote-find-link)
   ("C-c w d i" . denote-link-or-create)
   ("C-c w d k" . denote-rename-file-keywords)
   ("C-c w d n" . denote)
   ("C-c w d o" . denote-open-or-create)
   ("C-c w d r" . denote-rename-file)
   ("C-c w d R" . denote-rename-file-using-front-matter)))

(use-package denote-org
  :bind
  (("C-c w d h" . denote-org-link-to-heading)
   ("C-c w d s" . denote-org-extract-org-subtree)))

;; Denote

(setq denote-directory "~/Notas")

;; Consult convenience functions

(use-package consult
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-c w g" . consult-grep))
  :config
  (add-to-list 'consult-preview-allowed-hooks 'visual-line-mode))

;; Consult-Notes for easy access to notes

(use-package consult-notes
  :custom
  (consult-notes-denote-display-keywords-indicator "_")
  :bind
  (("C-c w d f" . consult-notes)
   ("C-c w d g" . consult-notes-search-in-all-notes))
  :init
  (consult-notes-denote-mode))

;; Citar-Denote to manage literature notes

(use-package citar-denote
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))

;; Explore and manage your Denote collection

(use-package denote-explore
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-barchart-keywords)
   ("C-c w x e" . denote-explore-barchart-filetypes)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ("C-c w x x" . denote-explore-random-regex)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-barchart-degree)))

;; Set some Org mode shortcuts

(use-package org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

;; Distraction-free writing

(use-package olivetti
  :demand t
  :bind
  (("C-c w o" . ews-olivetti)))

;; Undo Tree

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind
  (("C-c w u" . undo-tree-visualise)))

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-global-bibliography ews-bibtex-files
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

;; Lookup words in online dictionaries

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  (("C-c w s d" . dictionary-lookup-definition)))

(use-package powerthesaurus
  :bind
  (("C-c w s p" . powerthesaurus-transient)))

;; Writegood-Mode for weasel words, passive writing and repeated word detection

(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  :hook
  (text-mode . writegood-mode))

;; Titlecasing

(use-package titlecase
  :bind
  (("C-c w s t" . titlecase-dwim)
   ("C-c w s c" . ews-org-headings-titlecase)))

;; Abbreviations

(add-hook 'text-mode-hook 'abbrev-mode)

;; Lorem Ipsum generator

(use-package lorem-ipsum
  :custom
  (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
  :init
  (setq lorem-ipsum-sentence-separator
        (if sentence-end-double-space "  " " "))
  :bind
  (("C-c w s i" . lorem-ipsum-insert-paragraphs)))

;; ediff

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Enable Other text modes

;; Fountain mode for writing scripts

(use-package fountain-mode)

;; Markdown mode

(use-package markdown-mode)

;; PUBLICATION

;; Generic Org Export Settings

(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

;; epub export

(use-package ox-epub
  :demand t
  :init
  (require 'ox-org))

;; LaTeX PDF Export settings

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; EWS paperback configuration

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("ews"
     "\\documentclass[11pt, twoside, hidelinks]{memoir}
      \\setstocksize{9.25in}{7.5in}
      \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
      \\setlrmarginsandblock{1.5in}{1in}{*} 
      \\setulmarginsandblock{1in}{1.5in}{*}
      \\checkandfixthelayout
      \\layout
      \\setcounter{tocdepth}{0}
      \\setsecnumdepth{subsection}
      \\renewcommand{\\baselinestretch}{1.2}
      \\setheadfoot{0.5in}{0.75in}
      \\setlength{\\footskip}{0.8in}
      \\chapterstyle{bianchi}
      \\renewcommand{\\beforechapskip}{-30pt}
      \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
      \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
      \\setsubsubsecheadstyle{\\normalfont\\centering}
      \\pagestyle{myheadings}
      \\usepackage[font={small, it}]{caption}
      \\usepackage{ccicons}
      \\usepackage{ebgaramond}
      \\usepackage[authoryear]{natbib}
      \\bibliographystyle{apalike}
      \\usepackage{svg}
      \\hyphenation{mini-buffer}
      \\renewcommand{\\LaTeX}{LaTeX}
      \\renewcommand{\\TeX}{TeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;; ADMINISTRATION

;; Mis ficheros de agenda

(setq org-agenda-files (list ql-tasks-file))

;; Ficheros para organizar

(setq org-refile-targets
      '((ql-tasks-file :maxlevel . 2)  ; agenda
        (ql-collections-file :maxlevel . 3)  ; Colecciones
        (ql-index-file :maxlevel . 2)  ; fichero de indice
        (ql-archive-file :maxlevel . 3))) ; Archive

;; Mis etiquetas

(setq org-tag-alist-for-agenda
      '(;; áreas
        ("@Hogar" . ?H)
        ("@Sysadmin" . ?S)
	("@Ocio" . ?O)
	("@Blog" . ?B)
      
        ;; Entornos
        ("_Ordenador" . ?C)
        ("_Teléfono" . ?T)
	("_Escritorio" . ?E)
        ("_Calle" . ?R)

        ;; Tipo/Estado
        ("_Procesar" . ?D)
        ("_Acción" . ?N)
	("_Proyecto" . ?P)
	("_Hábito" . ?h)

        ;; Actividaes
        ("@planificar" . ?p)
        ("@configurar" . ?c)
        ("@escribir" . ?w)
        ("@investigar" . ?i)
        ("@email" . ?e)
        ("@llamar" . ?l)
	("@publicar" . ?b)
        ("@recados" . ?r)))

;; Anotar log
(setq org-agenda-start-with-log-mode t)

;; Visualización de la agenda

;; Abrir la agenda en una ventana única
(setq org-agenda-window-setup 'only-window)

;; Ajustar la vista de columnas
(setq org-agenda-prefix-format
      '((agenda . " %b ")
        (todo   . " %b ")
        (tags   . " %b ")
        (search . " %b ")))

(setq org-columns-default-format "%PRIORITY %25ITEM %SCHEDULED %DEADLINE %TAGS")

;; Teclas para la agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; Eliminr las etiquetas del archivo
(setq org-agenda-remove-tags t)

;; VISTAS PARA LA AGENDA
;; Tareas capturadas por ubicar

;; Bind org agenda command and custom agenda

(setq org-agenda-custom-commands
      '(("0" "Tareas por organizar" tags-todo "+_Procesar")
        ("t" agenda "Tareas de hoy"
               ((org-agenda-span 'day)
                (org-agenda-entry-types '(:deadline :scheduled))
       ;;         (org-agenda-skip-function '(org-agenda-skip-deadline-if-shown))
                (org-agenda-overriding-header "Tareas para hoy")))
        ("p" "Lista de Proyectos" tags-todo "+_Proyecto")
        ("h" "Lista de Proyectos parados" todo "HOLD")))

;; FILE MANAGEMENT

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil))

;; Hide or display hidden files

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ( "."     . dired-omit-mode))
  :custom (dired-omit-files "^\\.[a-zA-Z0-9]+"))

;; Backup files

(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              version-control t
              delete-old-versions t
              create-lockfiles nil)

;; Recent files

(use-package recentf
  :config
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 50)
  :bind
  (("C-c w r" . recentf-open)))

;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r d" . bookmark-delete))

;; Image viewer

(use-package emacs
  :custom
  (image-dired-external-viewer "gimp")
  :bind
  ((:map image-mode-map
         ("k" . image-kill-buffer)
         ("<right>" . image-next-file)
         ("<left>"  . image-previous-file))
   (:map dired-mode-map
         ("C-<return>" . image-dired-dired-display-external))))

(use-package image-dired
  :bind
  (("C-c w I" . image-dired))
  (:map image-dired-thumbnail-mode-map
        ("C-<right>" . image-dired-display-next)
        ("C-<left>"  . image-dired-display-previous)))

;; ADVANCED UNDOCUMENTED EXPORT SETTINGS FOR EWS

;; GraphViz for flow diagrams
;; requires GraphViz software
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;;;; Mis añadidos especiales


;;; ql-org-headers.el --- Gestión automática de encabezados Org -*- lexical-binding: t; -*-

;; Author: Quijote Libre
;; Created: 2025-06-30
;; Keywords: org, metadata, automation
;; Version: 0.3

;;; Commentary:
;; Inserta y actualiza automáticamente los encabezados:
;; - #+author: Quijote Libre
;; - #+lastmod: con la fecha actual
;; - #+startup: content
;; Respeta los encabezados fijos de Denote y evita duplicados.

;;; Code:

(defun ql/org-clean-and-insert-headers ()
  "Insertar o actualizar encabezados Org: author, lastmod y startup."
  (save-excursion
    (goto-char (point-min))
    (let* ((timestamp (format-time-string "%Y-%m-%d %a %H:%M"))
           (is-denote (and buffer-file-name
                           (boundp 'denote-directory)
                           (string-prefix-p (expand-file-name denote-directory)
                                            (expand-file-name buffer-file-name))))
           (insert-pos nil))

      ;; 1. Eliminar duplicados
      (dolist (header '("author" "lastmod" "startup"))
        (goto-char (point-min))
        (let ((found nil))
          (while (re-search-forward (format "^#\+%s:.*$" header) nil t)
            (if (not found)
                (setq found t)
              (beginning-of-line)
              (kill-line 1)))))

      ;; 2. Detectar existencia después de limpieza
      (goto-char (point-min))
      (let ((author-exists (re-search-forward "^#\+author:" nil t))
            (lastmod-exists (progn (goto-char (point-min)) (re-search-forward "^#\+lastmod:" nil t)))
            (startup-exists (progn (goto-char (point-min)) (re-search-forward "^#\+startup:" nil t))))

        ;; 3. Calcular posición de inserción
        (goto-char (point-min))
        (if is-denote
            (let ((headers '("title" "date" "filetags" "identifier"))
                  (max-end 0))
              (dolist (hdr headers)
                (goto-char (point-min))
                (when (re-search-forward (format "^#\+%s:.*$" hdr) nil t)
                  (setq max-end (max max-end (point)))))
              (setq insert-pos max-end))
          ;; No Denote
          (when (re-search-forward "^\([^#]\|$\)" nil t)
            (beginning-of-line)
            (setq insert-pos (point))))

        ;; 4. Insertar encabezados que faltan
        (goto-char insert-pos)
        (unless (bolp) (insert "
"))
        (unless author-exists
          (insert "#+author: Quijote Libre
"))
        (unless lastmod-exists
          (insert (format "#+lastmod: <%s>
" timestamp)))

        ;; 5. Actualizar lastmod si ya existía
        (when lastmod-exists
          (goto-char (point-min))
          (when (re-search-forward "^#\+lastmod:.*$" nil t)
            (replace-match (format "#+lastmod: <%s>" timestamp))))


	;; 6. Insertar startup si falta
(unless startup-exists
  (goto-char (point-min))
  (if (re-search-forward "^#\\+lastmod:.*$" nil t)
      (progn
        (end-of-line)
        (insert "\n#+startup: content"))
    ;; Si no hay lastmod, buscar dónde termina el bloque de encabezados
    (goto-char (point-min))
    (when (re-search-forward "^\\([^#]\\|$\\)" nil t)
      (beginning-of-line)
      (unless (bolp) (insert "\n"))
      (insert "#+startup: content\n"))))))

(defun ql/org-setup-header-management ()
  "Activar la actualización automática de encabezados en archivos Org."
  (when (and buffer-file-name
             (string-suffix-p ".org" buffer-file-name))
    (add-hook 'before-save-hook #'ql/org-clean-and-insert-headers nil t)))

(add-hook 'find-file-hook #'ql/org-setup-header-management)

(defun ql/org-clean-headers-buffer ()
  "Ejecutar manualmente la limpieza y actualización de encabezados en el buffer actual."
  (interactive)
  (if (and buffer-file-name
           (string-suffix-p ".org" buffer-file-name))
      (progn
        (ql/org-clean-and-insert-headers)
        (message "Encabezados Org actualizados."))
    (message "Este buffer no es un archivo .org válido.")))

(provide 'ql-org-headers)

;;; ql-org-headers.el ends here
