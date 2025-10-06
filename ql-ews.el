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

(defcustom ql-index-file "~/org/20250430T130243==bujo--índice-de-mi-bullet-journal-digital.org"
  "Archivo índice del sistema."
  :type 'file
  :group 'ql)

;; Archivo de diario (comentado en tu código, lo dejo opcional)
;; (defcustom ql-journal-file \"~/org/20250426T191311==bujo--diario-personal.org\"
;;   \"Archivo de notas diarias.\"
;;   :type 'file
;;   :group 'ql)

(defcustom ql-tasks-file "~/org/20250426T191557==pkm--lista-de-tareas__list.org"
  "Archivo de tareas."
  :type 'file
  :group 'ql)

(defcustom ql-collections-file "~/org/20250903T113133==pkm--colecciones__list.org"
  "Archivo de colecciones del bujo."
  :type 'file
  :group 'ql)

(defcustom ql-archive-file "~/org/20250905T194624==pkm--lista-de-logros__list.org"
  "Archivo de tareas terminadas."
  :type 'file
  :group 'ql)

(defcustom ql-elfeed-file "~/org/20250221T153515==bujo--fuentes-rss__lista.org"
  "Archivo de fuentes RSS."
  :type 'file
  :group 'ql)

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

