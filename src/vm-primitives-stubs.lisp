;;; ============================================================================
;;; STUBS POUR PRIMITIVES VM
;;; ============================================================================
;;; 
;;; Ces stubs permettent de CHARGER loader-compilable.lisp en Lisp natif
;;; sans erreur. Ils ne sont JAMAIS exécutés - seulement utilisés pour
;;; permettre au compilateur de parser les définitions.
;;;
;;; Les primitives VM réelles sont dans le compilateur et la VM.
;;; ============================================================================

(defun vm-make-hash-table (&rest args)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-make-hash-table est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-gethash (key hash-table)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-gethash est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-hash-set (hash-table key value)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-hash-set est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-hash-table-count (hash-table)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-hash-table-count est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-hash-has-key (hash-table key)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-hash-has-key est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-listp (obj)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-listp est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-symbolp (obj)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-symbolp est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-keywordp (obj)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-keywordp est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-consp (obj)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-consp est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-atom (obj)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-atom est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-car (cons)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-car est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-cdr (cons)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-cdr est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-cons (car cdr)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-cons est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-cadr (list)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-cadr est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-equal (a b)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-equal est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-first (list)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-first est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-second (list)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-second est une primitive VM - ne peut pas être exécutée en Lisp natif"))

(defun vm-rest (list)
  "Stub - Ne pas exécuter! Utilisé seulement pour compilation."
  (error "vm-rest est une primitive VM - ne peut pas être exécutée en Lisp natif"))
