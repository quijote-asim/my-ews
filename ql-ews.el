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

(defcustom ql-diary-file "~/org/agenda/20251101T114427==mimoc--log-diario.org"
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

(defcustom ql-goal-and-habits-file "~/org/20251101T114319==mimoc--hábitos-y-objetivos.org"
  "Archivo de hábitos y objetivos"
  :type 'file
  :group 'ql)

(defcustom ql-projects-file "~/org/20251101T114211==mimoc--resultados-esperados.org"
  "Archivo de Proyectos Activos"
  :type 'file
  :group 'ql)

(defcustom ql-to-reevaluate-file "~/org/20251101T113907==mimoc--temas-por-reevaluar.org"
  "Archivo de Proyectos Activos"
  :type 'file
  :group 'ql)

(defcustom ql-archive-file "~/org/20250905T194624==mimoc--temas-terminados.org"
  "Archivo de tareas terminadas"
  :type 'file
  :group 'ql)

(defcustom ql-elfeed-file "~/org/20250221T153515==bujo--fuentes-rss__lista.org"
  "Archivo de fuentes RSS."
  :type 'file
  :group 'ql)

;; -----------------------------------------------------------------------------
;; Actualización automática de #+LASTMOD:
;; -----------------------------------------------------------------------------

(define-minor-mode ql-lastmod-auto-update-mode
  "Actualiza #+LASTMOD: al guardar (solo en Org)."
  :init-value nil
  :lighter " LastMod"
  (if ql-lastmod-auto-update-mode
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

;;; FUNCIONES PARA MIMOC

(defun mimoc-date-today-p (ts)
  "TS es un time-stamp Org; true si es hoy."
  (when ts
    (let* ((d (calendar-gregorian-from-absolute (org-time-to-absolute ts)))
           (td (calendar-current-date)))
      (equal d td))))

(defun mimoc-deadline-within-days-p (days)
  "¿Deadline dentro de DAYS (incluyendo hoy)?"
  (let ((dl (org-get-deadline-time (point))))
    (when dl
      (let* ((abs (org-time-to-absolute dl))
             (today (org-today)))
        (and (>= abs today) (<= abs (+ today days)))))))

(defun mimoc-agenda-skip-inbox-if-not-soon ()
  "Saltar elementos de la categoría InBox salvo si:
   - SCHEDULED es hoy, o
   - DEADLINE < 7 días."
  (let ((cat (org-get-category)))
    (when (string-match-p "\\`InBox\\'" cat)
      (let ((sch (org-get-scheduled-time (point)))
            (dl  (org-get-deadline-time (point))))
        (unless (or (mimoc-date-today-p sch)
                    (mimoc-deadline-within-days-p 7))
          (or (outline-next-heading) (point-max)))))))

;; Exportar objetivos a archivo temporal
(defun mimoc-export-objetivos-to-temp ()
  "Exporta todos los objetivos a archivo temporal para revisión."
  (interactive)
  (let ((temp-file (make-temp-file "objetivos-" nil ".org")))
    (with-temp-file temp-file
      (insert "#+title: Lista Completa de Objetivos\n")
      (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
      (org-map-entries
       (lambda ()
         (when (and (member (org-get-todo-state) '("TODO"))
                    (string-match "Objetivo" (org-get-heading t t t t)))
           (insert "* " (org-get-heading t t t t) "\n")
           (insert ":PROPERTIES:\n")
           (insert ":BLOQUE: " (or (org-entry-get nil "CATEGORY") "Sin categoría") "\n")
           (insert ":END:\n\n")))
       nil
       (list ql-objectives-file)))
    (find-file temp-file)
    (message "Objetivos exportados a: %s" temp-file)))

;; Exportar resultados a archivo temporal
(defun mimoc-export-resultados-to-temp ()
  "Exporta todos los resultados activos a archivo temporal."
  (interactive)
  (let ((temp-file (make-temp-file "resultados-" nil ".org")))
    (with-temp-file temp-file
      (insert "#+title: Lista Completa de Resultados Activos\n")
      (insert "#+date: " (format-time-string "[%Y-%m-%d %a]") "\n\n")
      (org-map-entries
       (lambda ()
         (when (and (member (org-get-todo-state) '("TODO" "HOLD"))
                    (string-match "RESULTADO" (org-get-heading t t t t)))
           (insert "* " (org-get-heading t t t t) "\n")
           (let ((area (org-entry-get nil "AREA"))
                 (refs (org-entry-get nil "REFERENCIAS")))
             (insert ":PROPERTIES:\n")
             (when area (insert ":AREA: " area "\n"))
             (when refs (insert ":REFERENCIAS: " refs "\n"))
             (insert ":END:\n\n"))))
       nil
       (list ql-projects-file)))
    (find-file temp-file)
    (message "Resultados exportados a: %s" temp-file)))

;; Activar área inactiva
(defun mimoc-activar-area-inactiva ()
  "Mueve una subárea de 'Áreas Inactivas' a la sección activa del Bloque 4."
  (interactive)
  (let* ((areas-inactivas '("Paquetes y apps"
                            "Comunicación y cuentas"
                            "Finanzas personales"
                            "Organización de archivos"
                            "Fotografía"
                            "Salud y bienestar"
                            "Vida digital y seguridad"
                            "Resultados y tareas (metodología)"
                            "Ocio y comunidad"))
         (area-elegida (completing-read "Área a activar: " areas-inactivas)))
    (find-file ql-objectives-file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\*\\* %s" (regexp-quote area-elegida)) nil t)
        (progn
          (org-cut-subtree)
          (re-search-backward "^\\*\\* Áreas Inactivas")
          (org-backward-heading-same-level 1)
          (org-end-of-subtree)
          (org-paste-subtree 2)
          (message "Área '%s' activada. Añade objetivos/resultados ahora." area-elegida))
      (message "Área no encontrada."))))

(defun mimoc-crear-estructura-dia ()
  "Crea estructura tripartita para el día actual en log-diario."
  (interactive)
  (find-file ql-diary-file)
  (let* ((fecha-actual (calendar-current-date))
         (fecha-string (format-time-string "%Y-%m-%d %A"))
         (regexp-denote (format-time-string "%m%dT"))) ; MMDDT para denote-links
    ;; Navegar o crear entrada datetree para hoy
    (org-datetree-find-date-create fecha-actual)
    (org-end-of-subtree t t)
    ;; Verificar si ya existe la estructura
    (unless (save-excursion
              (org-back-to-heading t)
              (re-search-forward "^\\*\\*\\*\\* Log del día 📋" 
                                (save-excursion (org-end-of-subtree t t) (point))
                                t))
      ;; Crear los 3 headings
      (insert "\n**** Log del día 📋\n\n")
      (insert "**** Reflexiones 💭\n\n")
      (insert (format "**** Emergencia del día 🔝\n\n#+BEGIN: denote-links :regexp \"%s\" :sort-by-component date :reverse-sort t :include-date t\n#+END:\n"
                      regexp-denote))
      (message "Estructura del día %s creada" fecha-string))))

;; Atajos globales
(global-set-key (kbd "C-c w d i") #'mimoc-crear-estructura-dia)
(global-set-key (kbd "C-c w x o") #'mimoc-export-objetivos-to-temp)
(global-set-key (kbd "C-c w x p") #'mimoc-export-resultados-to-temp)
(global-set-key (kbd "C-c w a") #'mimoc-activar-area-inactiva)

;;; ql-ews.el ends here
