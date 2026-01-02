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
;; This init file is tangled from: documents/99-appendix.org
;;
;; This file provides a starter kit for developing a configuration and is
;; not a package that is regularly updated.
;;
;;; Code:

;; Emacs 29 available?

(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires version 29 or later"))

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

;; (load-file (concat (file-name-as-directory user-emacs-directory) "ews.el"))

;; Load EWS y  QL-EWS functions

(load-file (expand-file-name "ews.el" user-emacs-directory))
(load-file (expand-file-name "ql-ews.el" user-emacs-directory))

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


(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; --- Fuente
(set-face-attribute
 'default nil
 :family "Aporetic Sans Mono"
 :height 143
 :weight 'regular)

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
      initial-scratch-message (concat "#+title: Emacs Writing Studio\n"
					"#+subtitle: Scratch Buffer\n\n"
					"The text in this buffer is not saved "
					"when exiting Emacs!\n\n"))

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

(use-package ef-themes
  :config
  ;; Opcional pero recomendado: que los comandos de Modus operen sobre ef-themes
  (ef-themes-take-over-modus-themes-mode 1)

  ;; Carga inicial del tema ef-*
  (modus-themes-load-theme 'ef-dark))

;; like `load-theme' but also call `ef-themes-post-load-hook'
;; (ef-themes-select 'ef-dark)

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

;; Nombres de día/mes en español para timestamps
(setq system-time-locale "es_ES.UTF-8")

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
  (which-key-sort-order 'which-key-description-order)
  :init
  (which-key-add-key-based-replacements
    "C-c w"   "Emacs Writing Studio"
    "C-c w b" "Bibliographic"
    "C-c w d" "Denote"
    "C-c w m" "Multimedia"
    "C-c w s" "Spelling and Grammar"
    "C-c w t" "Themes"
    "C-c w x" "Explore"))

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
  (ispell-hunspell-add-multi-dic (mapconcat 'identity ews-hunspell-dictionaries ","))
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

;;; CONFIGURACION DE ORG-MODE
;; NOTA: Consolidación de múltiples bloques use-package en uno único
;; FECHA: 2025-11-02
;; RAZÓN: Eliminar redundancia y mejorar mantenibilidad
;; CAMBIOS: Fusión sin modificación de funcionalidad

(use-package org
  ;; Apariencia y visualización
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-fold-catch-invisible-edits 'show)
  
  ;; Gestión de tareas y logging
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-reverse-note-order t)
  
  ;; Enlaces y referencias
  (org-id-link-to-org-use-id t)
  
  ;; Exportación
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y")
  
  ;; Atajos de teclado globales
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  
  ;; Atajos específicos del modo Org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

;; Configuración de mi flujo de trabajo
;; Configuración de TODO en org-mode

;; --- Secuencia de TODOs coherente con tus estados ---
(setq org-todo-keywords
      '((sequence
         "Resultado(r!)"    ; activos -> rojo
         "Acción(a!)"       ; hacer -> amarillo fuerte
         "Esperando(e@/!)"  ; en espera -> amarillo suave
         "Parado(p@/!)"     ; pausado -> azul
         "|"
         "Hecho(h@!)"       ; hecho -> verde
         "Cancelado(c@!)"))) ; cancelado -> gris

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
  (rmh-elfeed-org-files (list ql-elfeed-file)))

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

;; --- Denote: configuración principal (init.el) ---

(use-package denote
  :defer t
  :init
   ;; Carpeta raíz de Denote (ajusta ql-denote-directory con las variables generalesa)
  (setq denote-directory ql-denote-directory)
  (denote-rename-buffer-mode 1)

  :custom
  ;; Tu configuración existente
  (denote-sort-keywords nil)
  (with-eval-after-load 'denote
  (setq denote-link-description-format
        #'ews-denote-link-description-title-case))

  ;; Prompts para que pida subcarpeta en cada nueva nota (sin silos)
  (denote-prompts '(title subdirectory keywords))
  (denote-allow-multi-word-keywords t)

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
   ("C-c w d n" . denote-signature)
   ("C-c w d o" . denote-open-or-create)
   ("C-c w d r" . denote-rename-file)
   ("C-c w d R" . denote-rename-file-using-front-matter)))

;; Denote auxiliary packages

(use-package denote-org
  :bind
  (("C-c w d h" . denote-org-link-to-heading)))

(use-package denote-sequence)

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

;; Distraction-free writing

(use-package olivetti
  :demand t
  :bind
  (("C-c w o" . ews-olivetti)))

;; Vundo

(use-package vundo
  :bind
  (("C-M-/" . vundo)))

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
;; Markdown mode

(use-package fountain-mode)
(use-package markdown-mode)

;;; PUBLICATION

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
        \\renewcommand{\\baselinestretch}{1.25}
        \\setheadfoot{0.5in}{0.75in}
        \\setlength{\\footskip}{0.8in}
        \\chapterstyle{bianchi}
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
\\hyphenation{mini-buffer}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;; ADMINISTRATION

;; Mis ficheros de agenda

(setq org-agenda-files '("~/notes/agenda/"))

;; Define con que anticipación salen en agenda los deadlines

(setq org-deadline-warning-days 7)

;; Plantillas de capturas

(setq org-capture-templates
      `(
        ;; 1) Tarea rápida -> InBox
        ("t" "Tarea rápida al InBox ☑️" entry
         (file+headline ,ql-inbox-file "InBox 📥")
         "** Tarea: %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :empty-lines 1
         :prepend t)

        ;; 2) Acción siguiente
        ("a" "Acción siguiente ⏭️" entry
         (file "~/notes/agenda/mimoc--acciones-siguientes.org")
         "*** Acción: %?\nSCHEDULED: %t\n:PROPERTIES:\n:AREA: :Bloque3:Profesional:\n:CREATED: %U\n:END:\n"
         :empty-lines 1)

        ;; 3) Resultado esperado
        ("p" "Resultado esperado 🏁" entry
         (file "~/notes/agenda/mimoc--resultados-esperados.org")
         "*** Resultado:(r!) %?\n:PROPERTIES:\n:AREA: :Bloque4:Sistemas:\n:REFERENCIAS:\n:CREATED: %U\n:END:\n** Acción: Siguiente Acción\n"
         :empty-lines 1)

        ;; 4) Diario: Log del día (datetree -> 'Nota 📋')
	;; Diario: Nota 📋
	("n" "Nota 📋" plain (file+datetree ,ql-diary-file)
	 "**** %^{Título}\n:PROPERTIES:\n:tipo: [Nota 📋]\n:relacion: %^{Proyecto (opcional)}\n:END:\n\n- *Contexto*:  %?\n- *Decisión*:\n- *Resultado*: "
	 :empty-lines 1)
       
	;; 5) Diario: Idea (datetree -> 'Idea 💡')
	("i" "Idea 💡" plain  (file+datetree ,ql-diary-file)
             "**** %^{Idea}\n:PROPERTIES:\n:tipo: [Idea 💡]:END:\n\n- *Descripción*:  %?\n- *Motivo*: "
         :empty-lines 1)

	;; 6) Diario: Bug (datetree -> 'Bug ⚠️')
        ("e" "Bug ⚠️" plain (file+datetree ,ql-diary-file)
             "**** %^{Incidencia}\n:PROPERTIES:\n:tipo: [Bug ⚠️]\n:relacion: %^{Proyecto (opcional)}\n:END:\n\n- *Qué falló*:  %?\n- *Sintómas*:\n- *Decisión*:\n- *Resultado*: "
             :empty-lines 1)
	
	;; 7) Diario: Reflexión (datetree -> 'Reflexion 💭')
        ("r" "Reflexión 💭" plain (file+datetree ,ql-diary-file)
             "**** %^{Tema de la reflexión}\n:PROPERTIES:\n:tipo: [Reflexión 💭]\n:END:\n\n%?\n- *Relevancia*:\n- *Impacto esperado: "
             :empty-lines 1)
	
        ))

;; Mis etiquetas

(setq org-tag-alist-for-agenda
      '(;; áreas
        ("@Hogar" . ?H)
        ("@Personal" . ?P)
	("@Ocio" . ?O)
	("@Blog" . ?B)
      
        ;; Entornos
        ("_Ordenador" . ?C)
        ("_Teléfono" . ?T)
	("_Escritorio" . ?E)
        ("_Calle" . ?S)

        ;; Tipo/Estado
        ("_Planifica" . ?P)
        ("_Hacer" . ?W)
	("_Hábito" . ?H)
	("_Revisar" . ?R)

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

;; --- Caras ligadas a la paleta del tema ef en uso ---

(defun ql-agenda-minimos--ef ()
  "Mínimos visuales para la agenda usando la paleta ef-themes."
  (ef-themes-with-colors
    (custom-set-faces
     `(org-agenda-structure ((t (:weight semibold :height 1.05))))
     `(org-agenda-date-today ((t (:weight bold :underline t))))
     `(org-agenda-done ((t (:foreground ,green))))
     `(org-scheduled-previously ((t (:foreground ,red :weight bold))))
     `(org-deadline-announce ((t (:foreground ,red))))
     `(org-agenda-current-time ((t (:weight bold))))
     `(org-agenda-time-grid ((t (:foreground ,fg-dim)))))))

(add-hook 'ef-themes-post-load-hook #'ql-agenda-minimos--ef)

;; Bind org agenda command and custom agenda

(setq org-agenda-custom-commands
      '(("0" "Tareas por organizar"
         ((tags-todo "mimoc+inbox")))

        ("t" "Tareas de hoy" agenda ""
         ((org-agenda-span 'day)
          (org-agenda-entry-types '(:deadline :scheduled))
          (org-agenda-overriding-header "Tareas para hoy")
          ;; Oculta DONE/HECHO + filtra InBox que no toca
          (org-agenda-skip-function
           (lambda ()
             (or (org-agenda-skip-entry-if 'todo 'done)
                 (mimoc-agenda-skip-inbox-if-not-soon))))
          ;; Reduce repeticiones entre SCHEDULED y DEADLINE
          (org-agenda-skip-scheduled-if-deadline-is-shown t)
          (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
          ;; Presentación más limpia (solo “columna 3”: título)
          (org-agenda-prefix-format '((agenda . "  ")))
          (org-agenda-remove-tags t)
          (org-agenda-use-time-grid nil)
          (org-agenda-tags-column 0)))

        ("p" "Resultados activos"
         ((tags-todo "mimoc+resultado")))

        ("h" "Resultados parados"
         ((todo "HOLD")))

        ("o" "Lista completa de objetivos"
         ((tags-todo "LEVEL=2"
                     ((org-agenda-overriding-header "Objetivos de Bloques Principales")
                      (org-agenda-files '("~/notes/agenda/20251101T114319==mimoc--hábitos-y-objetivos.org"))))
          (tags-todo "LEVEL=3&TODO=\"TODO\""
                     ((org-agenda-overriding-header "Objetivos de Subáreas (Bloque 4)")
                      (org-agenda-files '("~/notes/agenda/20251101T114319==mimoc--hábitos-y-objetivos.org"))))))

        ("P" "Lista completa de resultados activos"
         ((tags-todo "mimoc+resultado/TODO|HOLD"
                     ((org-agenda-overriding-header "Resultados en Curso")
                      (org-agenda-sorting-strategy '(category-keep))))))

        ("i" "Revisión de áreas inactivas"
         ((tags "LEVEL=3&CATEGORY=\"Sistemas\""
                ((org-agenda-overriding-header "Áreas del Bloque 4 (Sistemas)")
                 (org-agenda-files '("~/notes/agenda/20251101T114319==mimoc--hábitos-y-objetivos.org"))
                 (org-agenda-prefix-format "  %-20b ")
                 (org-agenda-sorting-strategy '(category-keep))))))

        ("e" "Equilibrio entre bloques principales"
         ((tags "LEVEL=1"
                ((org-agenda-overriding-header "Contadores de Bloques Principales")
                 (org-agenda-files '("~/notes/agenda/20251101T114319==mimoc--hábitos-y-objetivos.org"))
                 (org-agenda-prefix-format "  ")))))))

;; Refile en los ficheros de agenda hasta nivel 3

(setq org-refile-targets
      '(("~/notes/agenda/20251101T114111==mimoc--acciones-siguientes.org" :maxlevel . 2)
        ("~/notes/agenda/20251101T114211==mimoc--resultados-esperados.org" :maxlevel . 2)
        ("~/notes/agenda/20251101T114319==mimoc--hábitos-y-objetivos.org" :maxlevel . 2)))

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

;; Bind key for customising variables

(keymap-global-set "C-c w v" 'customize-variable)

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name
			     "custom.el"
			     user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

;; ADVANCED UNDOCUMENTED EXPORT SETTINGS FOR EWS

;; Use GraphViz for flow diagrams
;; requires GraphViz software

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;;;; Mis añadidos especiales

;;; ox-hugo mi herramienta para publicar

;; Instala/activa ox-hugo
(use-package ox-hugo
  :ensure t
  :after org
  :commands (org-hugo-export-wim-to-md)
  :custom
  (org-hugo-base-dir "~/work/blog/my-blog/")  ;; ruta raíz de tu sitio Hugo
  (org-hugo-auto-export-on-save t))                ;; habilita auto-export al guardar

;; archivo a crear en  blog/.dir-locals.el
;; Con el se convierten automaticamente los archivos que se inserten
;; ~/notes/denote/.dir-locals.el
;; ((org-mode . ((eval . (org-hugo-auto-export-mode)))))

;; Establecer las carpetas de exportación
(setq org-hugo-auto-export-directory "~/work/blog/my-blog/content/blog/")
(setq org-hugo-auto-export-pages-directory "~/work/blog/my-blog/content/pages/")

(defun ql-hugo-export-if-publicar ()
  "Exportar solo archivos que contengan '__publicar.org' en el nombre."
  (when (and (string= (file-name-extension (buffer-file-name)) "org")
             (string-match "__publicar\\.org$" (file-name-nondirectory (buffer-file-name))))
    (org-hugo-auto-export-to-md)))

(add-hook 'after-save-hook 'ql-hugo-export-if-publicar)

;;; ql-org-headers.el ends here

