;; Copyright (C) 2025 Quijote Libre
;; Author: Quijote Libre 
;; Created: 29 april 2025
;; Version: 0.1
;; ql-ews.el --- files for Quijote Libre

;; ==============================
;; Grupo QL
;; ==============================

(defgroup ql nil
  "Configuraciones personales del sistema QL."
  :group 'applications
  :prefix "ql-")

;; ==============================
;; Archivos clave
;; ==============================

(defcustom ql-denote-directory (expand-file-name "~/org")
  "Directorio raíz donde vivirán las notas de Denote."
  :type 'directory
  :group 'ql)

(defcustom ql-index-file "~/org/agenda/20251101T113643==mimoc--bujo-index.org"
  "Archivo índice del sistema."
  :type 'file
  :group 'ql)

(defcustom ql-bitacora-file "~/org/agenda/20251101T114427==mimoc--log-diario.org"
  "Archivo para logs diarios"
   :type 'file
   :group 'ql)

(defcustom ql-inbox-file "~/org/agenda/20251101T113906==mimoc--inbox-tareas.org"
  "Archivo de capturas"
  :type 'file
  :group 'ql)

(defcustom ql-tasks-file "~/org/agenda/20251101T114111==mimoc--acciones-siguientes.org"
  "Archivo de tareas"
  :type 'file
  :group 'ql)

(defcustom ql-collections-file "~/org/20251101T114319==mimoc--hábitos-y-objetivos.org"
  "Archivo de hábitos y objetivos"
  :type 'file
  :group 'ql)

(defcustom ql-collections-file "~/org/20251101T114211==mimoc--proyectos-activos.org"
  "Archivo de Proyectos Activos"
  :type 'file
  :group 'ql)

;; (defcustom ql-archive-file "~/org/20250905T194624==pkm--lista-de-logros__list.org"
;;   "Archivo de tareas terminadas"
;;   :type 'file
;;   :group 'ql)

(defcustom ql-elfeed-file "~/org/20250221T153515==bujo--fuentes-rss__lista.org"
  "Archivo de fuentes RSS."
  :type 'file
  :group 'ql)

;; -----------------------------------------------------------------------------
;; Actualización automática de #+LASTMOD:
;; -----------------------------------------------------------------------------

(define-minor-mode ql/lastmod-auto-update-mode
  "Actualiza #+LASTMOD: al guardar (solo en Org)."
  :init-value nil
  :lighter " LastMod"
  (if ql/lastmod-auto-update-mode
      (add-hook 'before-save-hook #'ql/--update-lastmod nil t)
    (remove-hook 'before-save-hook #'ql/--update-lastmod t)))

(defun ql/--update-lastmod ()
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (ql/--rx-key "lastmod") nil t)
        (replace-match (format "#+LASTMOD: %s"
                               (format-time-string "%Y-%m-%d %H:%M"))
                       t t)))))

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
;;;    - Traduce por defecto a Español
;;;
;;;  ATAJOS:
;;;    C-c t -> traduce directamente al idioma configurado (por defecto español)
;;;    C-u C-c t= → pregunta a qué idioma traducir (ejemplo: =en=, =fr= , etc.).
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

;; Config Flyspell/Hunspell (corrector)
(setq ispell-program-name "hunspell")
(setq ispell-dictionary "es_ES")            ; por defecto español

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook  #'flyspell-mode)

;; Idioma por defecto
(defcustom ql-translate-default-target "es"
  "Idioma destino por defecto para traducción con translate-shell.
Usado por `ql-translate-region`."
  :type 'string
  :group 'ql)

;; ==============================
;; Función principal
;; ==============================
(defun ql-translate-region (&optional ask)
  "Traduce la región seleccionada usando `trans`.
Detecta automáticamente el idioma fuente.
Sin prefijo: traduce a `ql-translate-default-target`.
Con C-u: pregunta el idioma destino."
  (interactive "P")
  (unless (use-region-p)
    (user-error "Selecciona texto antes de traducir"))
  (let* ((lang (if ask
                   (read-string "Idioma destino (ej. es, en, fr): "
                                ql-translate-default-target)
                 ql-translate-default-target))
         (cmd (format "trans -b :%s" lang)))
    (shell-command-on-region (region-beginning) (region-end) cmd t)))

;; ==============================
;; Atajo de teclado
;; ==============================

(global-set-key (kbd "C-c t") #'ql-translate-region)

;;; ql-ews.el ends here
