;;; ews.el --- Convenience functions for authors  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Created: 1 January 2024
;; Version: 1.0
;; Keywords: convenience
;; Homepage: https://lucidmanager.org/tags/emacs/
;; URL: https://github.com/pprevos/emacs-writing-studio

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
;;; Commentary:
;;
;; Series of convenience functions for Emacs Writing Studio
;; https://lucidmanager.org/tags/emacs
;;
;;; Code:

;; Emacs Writing Studio Customisation

(defgroup ews ()
  "Emacs Writing Studio."
  :group 'files
  :link '(url-link :tag "Homepage" "https://lucidmanager.org/tags/emacs/"))

(defcustom ews-bibtex-directory
  (concat (file-name-as-directory (getenv "HOME")) "archive/bibl")
  "Location of BibTeX files and attachments."
  :group 'ews
  :type 'directory)

(defcustom ews-denote-para-keywords
  '("Proyecto" "Área" "Recurso" "Archivo")
  "List of keywords to use for implementing the PARA method with Denote."
  :group 'ews
  :type 'list)

(defcustom ews-org-heading-level-capitalise nil
  "Minimum level of Org headings to be capitalised
Nil implies all levels are capitalised."
  :group 'ews
  :type  '(choice (const :tag "All headings" nil)
		  (integer :tag "Highest level" 2)))

;; Check for missing external software
;;;###autoload
(defun ews-missing-executables (prog-list)
  "Identify missing executables in PROG-LIST.
Sublists indicate that one of the entries is required."
  (let ((missing '()))
    (dolist (exec prog-list)
      (if (listp exec)
          (unless (cl-some #'executable-find exec)
            (push (format "(%s)" (mapconcat 'identity exec " or ")) missing))
        (unless (executable-find exec)
          (push exec missing))))
    (if missing
        (message "Missing executable files(s): %s"
                 (mapconcat 'identity missing ", "))
      (message "No missing executable files."))))

;;; BIBLIOGRAPHY
(defvar ews-bibtex-files
  (when (file-exists-p ews-bibtex-directory)
    (directory-files ews-bibtex-directory t "^[A-Z|a-z|0-9].+.bib$"))
  "List of BibTeX files. Use `ews-bibtex-register' to configure.")

;;;###autoload
(defun ews-bibtex-register ()
  "Register the contents of the `ews-bibtex-directory' with `ews-bibtex-files`.
Use when adding or removing a BibTeX file from or to `ews-bibtex-directory'."
  (interactive)
  (when (file-exists-p ews-bibtex-directory)
    (let ((bib-files (directory-files ews-bibtex-directory t
				      "^[A-Z|a-z|0-9].+.bib$")))
      (setq ews-bibtex-files bib-files
  	    org-cite-global-bibliography bib-files
	    citar-bibliography bib-files)))
  (message "Registered:\n%s" (mapconcat #'identity ews-bibtex-files "\n")))

(defun ews--bibtex-combined-biblio-lookup ()
  "Combines `biblio-lookup' and `biblio-doi-insert-bibtex'."
  (require 'biblio)
  (let* ((dbs (biblio--named-backends))
         (db-list (append dbs '(("DOI" . biblio-doi-backend))))
         (db-selected (biblio-completing-read-alist
                       "Backend:"
                       db-list)))
    (if (eq db-selected 'biblio-doi-backend)
        (let ((doi (read-string "DOI: ")))
          (biblio-doi-insert-bibtex doi))
      (biblio-lookup db-selected))))

;;;###autoload
(defun ews-bibtex-biblio-lookup ()
  "Insert Biblio search results into current buffer or select BibTeX file."
  (interactive)
  (if-let ((current-mode major-mode)
	   ews-bibtex-files
	   (bibfiles (length ews-bibtex-files))
	   (bibfile (cond ((eq bibfiles 1) (car ews-bibtex-files))
			  ((equal major-mode 'bibtex-mode)
			   (buffer-file-name))
			  (t (completing-read
			      "Select BibTeX file:" ews-bibtex-files)))))
      (progn (find-file bibfile)
	     (goto-char (point-max))
	     (ews--bibtex-combined-biblio-lookup)
	     (save-buffer))
    (message "No BibTeX file(s) defined.")))

;; Search for missing BibTeX attachments and filenames
(defun ews--bibtex-extract-attachments ()
  "Extract attachment file names from BibTeX files in `ews-bibtex-directory'."
  (ews-bibtex-register)
  (let ((attachments '()))
    (dolist (bibtex-file ews-bibtex-files)
      (with-temp-buffer
        (insert-file-contents bibtex-file)
        (goto-char (point-min))
        (while (re-search-forward "file.*=.*{\\([^}]+\\)}" nil t)
          (let ((file-paths (split-string (match-string 1)
                                          "[[:space:]]*;[[:space:]]*")))
            (dolist (file-path file-paths)
              (push (expand-file-name (string-trim file-path)
                                      ews-bibtex-directory)
                    attachments))))))
    attachments))

(defun ews--bibtex-extract-files ()
  "List files recursively in `ews-bibtex-directory', excluding `.bib' and `.csl'."
  (seq-remove (lambda (file)
                (or (string-suffix-p ".bib" file)
                    (string-suffix-p ".csl" file)))
              (mapcar 'expand-file-name
                      (directory-files-recursively ews-bibtex-directory ""))))

;;;###autoload
(defun ews-bibtex-missing-files ()
  "List BibTeX attachments not listed in a BibTeX file entry."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (attachments (ews--bibtex-extract-attachments))
         (missing (cl-remove-if
                   (lambda (f) (member f attachments)) files)))
    (message "%s files not registered in bibliography" (length missing))
    (dolist (file missing)
      (message file))))

;;;###autoload
(defun ews-bibtex-missing-attachments ()
  "List BibTeX file entries with missing attachment(s)."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (attachments (ews--bibtex-extract-attachments))
         (missing (cl-remove-if
                   (lambda (f) (member f files)) attachments)))
    (message "%s BibTeX files without matching attachment." (length missing))
    (dolist (file missing)
      (message file))))

;; Denote
;;;###autoload
(defun ews-denote-assign-para ()
  "Move your note to either Project, Area, Reource or Archive (PARA).
Configure the PARA names with `ews-denote-para-keywords'."
  (interactive)
  (if-let* ((file (buffer-file-name))
            ((denote-filename-is-note-p file))
            (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
            (keywords (seq-remove (lambda (keyword)
                                    (member keyword ews-denote-para-keywords))
                                  all-keywords))
            (para (completing-read "Select category: " ews-denote-para-keywords))
            (new-keywords (push para keywords)))
      (denote-rename-file
       file
       (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
       new-keywords
       (denote-retrieve-filename-signature file))
    (message "Current buffer is not a Denote file.")))

;; Distraction-free writing
(defvar ews-olivetti-point nil
  "Stores the point position before enabling Olivetti mode.")

;;;###autoload
(defun ews-olivetti ()
  "Distraction-free writing environment enhancing Olivetti mode.

Stores the window configuration when enabling Olivetti mode.
Restores the previous configuration when existing Olivetti mode
and moves point to the last location."
  (interactive)
  (if olivetti-mode
      (progn
        (if (eq (length (window-list)) 1)
            (progn
              (jump-to-register 1)
              (goto-char ews-olivetti-point)))
        (olivetti-mode 0)
        (text-scale-set 0))
    (progn
      (setq ews-olivetti-point (point))
      (window-configuration-to-register 1)
      (delete-other-windows)
      (text-scale-set 1)
      (olivetti-mode t))))

;;;###autoload
(defun ews-org-insert-notes-drawer ()
  "Generate or open a NOTES drawer under the current heading.
If a drawer exists for this section, a new line is created at the end of the
current note."
  (interactive)
  (push-mark)
  (org-previous-visible-heading 1)
  (forward-line)
  (if (looking-at-p "^[ \t]*:NOTES:")
      (progn
        (org-fold-hide-drawer-toggle 'off)
        (re-search-forward "^[ \t]*:END:" nil t)
        (forward-line -1)
        (org-end-of-line)
        (org-return))
    (org-insert-drawer nil "NOTES"))
  (org-unlogged-message "Press <C-u C-SPACE> to return to the previous position."))

;;;###autoload
(defun ews-org-count-words ()
  "Add word count to each heading property drawer in an Org mode buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            (end (save-excursion (org-end-of-subtree)))
            (word-count (count-words start end)))
       (org-set-property "WORDCOUNT" (number-to-string word-count))))))

;;;###autoload
(defun ews-org-insert-screenshot ()
  "Take a screenshot with the maim program and insert as an Org mode link."
  (interactive)
  (let ((filename (read-file-name "Enter filename for screenshot: " default-directory)))
    (unless (string-equal "png" (file-name-extension filename))
      (setq filename (concat (file-name-sans-extension filename) ".png")))
    (call-process-shell-command (format "maim --select %s" filename))
    (insert (format "#+caption: %s\n" (read-from-minibuffer "Caption: ")))
    (insert (format "[[file:%s]]" filename))
    (org-redisplay-inline-images)))

;;;###autoload
(defun ews-org-headings-titlecase (&optional arg)
  "Cycle through all headings in an Org buffer and convert them to title case.
When used with universal argument (ARG) converts to sentence case.
Customise `titlecase-style' for styling."
  (interactive "P")
  (require 'titlecase)
  (let ((style (if arg 'sentence titlecase-style)))
    (message "Converting headings to '%s' style" style)
    (org-map-entries
     (lambda ()
       (let* ((heading (substring-no-properties (org-get-heading t t t t)))
	      (level (org-current-level))
	      (heading-lower (downcase heading))
              (new-heading (titlecase--string heading-lower style)))
	 (when (<= level (or ews-org-heading-level-capitalise 999))
	   (org-edit-headline new-heading)))))))

;; (defun ews-denote-link-description-title-case (file)

;;   "Return link description for FILE.


;; If the region is active, use it as the description.

;; The title is formatted with the `titlecase' package.


;; This function is useful as the value of `denote-link-description-function' to

;; generate links in titlecase for attachments."
;;   (require 'titlecase)
;;   (let* ((file-type (denote-filetype-heuristics file))
;;           (title (denote-retrieve-title-or-filename file file-type))
;; 	 (clean-title (if (string-match-p " " title)
;; 			  title
;; 			(replace-regexp-in-string "\\([a-zA-Z0-9]\\)-\\([a-zA-Z0-9]\\)" "\\1 \\2" title)))
;;          (region-text (denote--get-active-region-content)))
;;    (cond
;;      (region-text region-text)
;;      (title (format "%s" (titlecase--string clean-title titlecase-style)))
;;     (t ""))))

;; Versión de Protesilaos

(defun ews-denote-link-description-title-case (file)
  "Return link description for FILE.

If the region is active, use it as the description.
The title is formatted with the `titlecase' package.

This function is useful as the value of `denote-link-description-function' to
generate links in titlecase for attachments."
  (require 'titlecase)
  (let* ((file-type (denote-filetype-heuristics file))
         (title (denote-retrieve-title-or-filename file file-type)))
    (cond
     ((denote--get-active-region-content))
     ((or (null title) (string-blank-p title))
      "")
     ((string-match-p " " title) ; Maybe you want `string-blank-p' here?
      title)
     (t
      (let ((clean-title (replace-regexp-in-string "\\([a-zA-Z0-9]\\)-\\([a-zA-Z0-9]\\)" "\\1 \\2" title)))
        (format "%s" (titlecase--string clean-title titlecase-style)))))))

;;; ews.el --- files for Quijote Libre

;; Copyright (C) 2025 Quijote Libre

;; Author: Quijote Libre 
;; Created: 29 april 2025
;; Version: 1.0

;; Definición de ficheros clave

(defconst ql-index-file     "~/org/20250430T130243==bujo--índice-de-mi-bullet-journal-digital.org"    "Archivo índice del sistema.")
;; (defconst ql-journal-file     "~/org/20250426T191311==bujo--diario-personal.org"    "Archivo de notas diarias")
(defconst ql-tasks-file       "~/org/20250426T191557==pkm--lista-de-tareas__list.org"   "Archivo de tareas")
;; (defconst ql-collections-file "~/org/20250430T125832--colecciones__bujo.org" "Archivo de tareas archivadas")
(defconst ql-colections-file     "~/org/20250903T113133==pkm--colecciones__list.org" "Archivo de colecciones del bujo")
(defconst ql-archive-file     "~/org/20250905T194624==pkm--lista-de-logros__list.org" "Archivo de tareas terminadas")
(defconst ql-elfeed-file     "~/org/20250221T153515==bujo--fuentes-rss__lista.org" "Archivo de fuentes rss")

;;; MIS FUNCIONES

;;; ============================================================================
;;;  Traducción inteligente en Emacs con translate-shell (+ Flyspell/Hunspell)
;;;
;;;  REQUISITOS (sistema):
;;;    - translate-shell (binario `trans`)
;;;        Debian/Ubuntu:  sudo apt install translate-shell
;;;        Fedora:         sudo dnf install translate-shell
;;;        Arch:           sudo pacman -S translate-shell
;;;        macOS (brew):   brew install translate-shell
;;;    - Hunspell + diccionarios (es, en_US, fr)
;;;        Debian/Ubuntu:  sudo apt install hunspell hunspell-es hunspell-en-us hunspell-fr
;;;        Fedora:         sudo dnf install hunspell hunspell-es hunspell-en hunspell-fr
;;;        Arch:           sudo pacman -S hunspell hunspell-es_es hunspell-en_us hunspell-fr
;;;
;;;  REGLAS DE TRADUCCIÓN:
;;;    - Si el ORIGEN ≠ español  -> destino = español.
;;;    - Si el ORIGEN =  español -> destino FIJO (por defecto inglés US).
;;;    - Toggle para cambiar el destino ES→(inglés ⇄ francés).
;;;    - Al hacer toggle se cambia también el diccionario de Flyspell (en_US ⇄ fr_FR).
;;;    - (Opcional en el código): cuando el origen es ES y traduces, se ajusta el
;;;      diccionario de Flyspell del buffer al destino (en_US o fr_FR).
;;;
;;;  ATAJOS:
;;;    C-c d e  -> Activa el diccionario Inglés
;;;    C-c d s  -> Activa el diccionario Español
;;;    C-c d f  -> Activa el diccionario Francés
;;;    C-c t b  -> traducir región a un buffer (*Translation*)
;;;    C-c t r  -> traducir región y reemplazar en el buffer actual
;;;    C-c t t  -> alternar destino ES → (inglés ⇄ francés) y cambiar Flyspell
;;;
;;;  HOOKS (corrector):
;;;    - ispell con Hunspell por defecto a español (es_ES).
;;;    - Activa Flyspell en text-mode y org-mode (no en prog-mode).
;;; ---------------------------------------------------------------------------
;;; NOTA SOBRE `global-set-key`:
;;;
;;; - Si enlazas una función definida con `defun` y `(interactive)`,
;;;   basta con usar `#'mi-funcion`. Ejemplo:
;;;
;;;     (defun ql/flyspell-dict-en ()
;;;       "Cambia el corrector (Flyspell/Hunspell) a inglés US."
;;;       (interactive)
;;;       (ispell-change-dictionary "en_US")
;;;       (message "Diccionario cambiado a: inglés (US)"))
;;;
;;;     (global-set-key (kbd "C-c s e") #'ql/flyspell-dict-en)
;;;
;;; - Si quieres hacer algo rápido sin definir función con nombre,
;;;   puedes usar un `lambda` con `(interactive)` dentro. Ejemplo:
;;;
;;;     (global-set-key (kbd "C-c s e")
;;;       (lambda () (interactive)
;;;         (ispell-change-dictionary "en_US")
;;;         (message "Diccionario cambiado a: inglés (US)")))
;;;
;;; En resumen:
;;;   • `#'mi-funcion` → más limpio si la función ya existe como comando.
;;;   • `(lambda () (interactive) …)` → útil para bindings rápidos ad-hoc.
;;; ============================================================================

(require 'cl-lib)

;; Config Flyspell/Hunspell (corrector)
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "es_ES")            ; por defecto español

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook  #'flyspell-mode)

;;; --- Parámetros de traducción ------------------------------------------------

;; Cuando el origen NO es español, traducir a:
(defvar ql/translate-dest-when-not-es "es")

;; Cuando el origen ES español, destino actual (toggle entre "en" y "fr"):
(defvar ql/translate-es-dest "en"    ; valores esperados: "en" o "fr"
  "Destino cuando el texto fuente es español. Alterna con `ql/translate-toggle-es-dest`.")

(defun ql/translate--set-flyspell-dict-for (lang)
  "Cambia el diccionario de Flyspell a LANG si está disponible.
   LANG: \"en\" -> en_US, \"fr\" -> fr_FR, \"es\" -> es_ES."
  (when (bound-and-true-p flyspell-mode)
    (pcase (downcase lang)
      ("en" (ispell-change-dictionary "en_US"))
      ("fr" (ispell-change-dictionary "fr_FR"))
      ("es" (ispell-change-dictionary "es_ES")))))

(defun ql/translate-toggle-es-dest ()
  "Alterna el destino cuando el origen es español entre inglés y francés.
También cambia el diccionario de Flyspell en consecuencia."
  (interactive)
  (setq ql/translate-es-dest (if (string= ql/translate-es-dest "en") "fr" "en"))
  (ql/translate--set-flyspell-dict-for ql/translate-es-dest)
  (message "Destino (origen ES) ahora: %s"
           (if (string= ql/translate-es-dest "en") "inglés (en_US)" "francés (fr_FR)")))

;;; --- Utilidades --------------------------------------------------------------

(defun ql/trans--ensure-bin ()
  (unless (executable-find "trans")
    (user-error "No encuentro `trans` (translate-shell). Instálalo e inténtalo de nuevo.")))

(defun ql/trans--region-bounds ()
  (unless (use-region-p)
    (user-error "Selecciona una región primero"))
  (cons (region-beginning) (region-end)))

(defun ql/string-trim (s)
  (replace-regexp-in-string "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" (or s "")))

(defun ql/trans--detect-lang (beg end)
  "Devuelve el código ISO (p.ej. \"es\", \"en\", \"fr\") detectado por `trans`."
  (ql/trans--ensure-bin)
  (with-temp-buffer
    (let ((status (call-process-region beg end "trans" nil t nil "-b" "-identify")))
      (unless (and (integerp status) (= status 0))
        (user-error "Fallo detectando idioma con `trans`"))
      (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
             (code (downcase (ql/string-trim raw))))
        (cond
         ((string-match "\\b\\([a-z][a-z]\\(-[A-Za-z]+\\)?\\)\\b" code)
          (downcase (match-string 1 code)))
         (t "auto"))))))

(defun ql/trans--compute-dest (src)
  "Si SRC es español → `ql/translate-es-dest`; si no → `ql/translate-dest-when-not-es`."
  (let* ((src2 (downcase src))
         (is-es (or (string= src2 "es") (string-prefix-p "es-" src2))))
    (if is-es ql/translate-es-dest ql/translate-dest-when-not-es)))

(defun ql/trans--call (beg end dest &optional replace)
  "Ejecuta `trans -b :DEST` sobre la región. Si REPLACE es no-nil, reemplaza en buffer."
  (ql/trans--ensure-bin)
  (let ((cmd (list "trans" "-b" (format ":%s" dest))))
    (if replace
        (shell-command-on-region beg end (mapconcat #'identity cmd " ") t t)
      (with-temp-buffer
        (apply #'call-process-region beg end (car cmd) nil t nil (cdr cmd))
        (buffer-string)))))

;;; --- Comandos principales ----------------------------------------------------

(defun ql/translate-region-to-buffer-smart ()
  "Traduce la región a un buffer `*Translation*`.
Reglas: origen≠ES→ES; origen=ES→(EN/FR según `ql/translate-es-dest`)."
  (interactive)
  (cl-destructuring-bind (beg . end) (ql/trans--region-bounds)
    (let* ((src  (ql/trans--detect-lang beg end))
           (dest (ql/trans--compute-dest src))
           (txt  (ql/trans--call beg end dest nil)))
      (with-current-buffer (get-buffer-create "*Translation*")
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Origen detectado: %s\nDestino: %s\n\n" src dest))
        (insert txt)
        (goto-char (point-min))
        (read-only-mode 1)
        (display-buffer (current-buffer))))))

(defun ql/translate-region-replace-smart ()
  "Traduce la región y la reemplaza en el buffer actual.
Reglas: origen≠ES→ES; origen=ES→(EN/FR según `ql/translate-es-dest`).
Ajusta el diccionario de Flyspell al destino cuando el origen es ES."
  (interactive)
  (cl-destructuring-bind (beg . end) (ql/trans--region-bounds)
    (kill-new (buffer-substring-no-properties beg end)) ;; backup al kill-ring
    (let* ((src  (ql/trans--detect-lang beg end))
           (dest (ql/trans--compute-dest src)))
      (ql/trans--call beg end dest t)
      ;; Si veníamos de ES, ponemos Flyspell al idioma destino para seguir corrigiendo bien
      (when (or (string= src "es") (string-prefix-p "es-" src))
        (ql/translate--set-flyspell-dict-for dest))
      (message "Traducido de %s a %s (reemplazado)." src dest))))

;;; --- Atajos ------------------------------------------------------------------

(global-set-key (kbd "C-c d e") (lambda () (interactive) (ispell-change-dictionary "en\_US")))
(global-set-key (kbd "C-c d s") (lambda () (interactive) (ispell-change-dictionary "es\_ES")))
(global-set-key (kbd "C-c d f") (lambda () (interactive) (ispell-change-dictionary "fr\_FR")))
(global-set-key (kbd "C-c t b") #'ql/translate-region-to-buffer-smart) ;; traducir a buffer
(global-set-key (kbd "C-c t r") #'ql/translate-region-replace-smart)   ;; traducir y reemplazar
(global-set-key (kbd "C-c t t") #'ql/translate-toggle-es-dest)         ;; toggle EN⇄FR (y Flyspell)

