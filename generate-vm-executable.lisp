;;;; Génération d'un fichier MIPS exécutable complet
;;;; Phase 11 - Phase 7 : VM1 bootstrap executable

(load "src/compiler.lisp")

(format t "~%╔══════════════════════════════════════════════════════════╗~%")
(format t "║  GÉNÉRATION FICHIER MIPS EXÉCUTABLE COMPLET              ║~%")
(format t "╚══════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; LECTURE DU CODE COMPILÉ
;;; ============================================================================

(defun read-all-forms (filename)
  "Lit toutes les formes d'un fichier"
  (with-open-file (stream filename :direction :input)
    (loop for form = (read stream nil nil)
          while form
          collect form)))

(format t "Lecture de src/vm-compilable.lisp...~%")
(defparameter *vm-forms* (read-all-forms "src/vm-compilable.lisp"))
(format t "✓ ~A formes lues~%~%" (length *vm-forms*))

;;; ============================================================================
;;; COMPILATION DE TOUTES LES FORMES
;;; ============================================================================

(defparameter *compiled-globals* '())
(defparameter *compiled-functions* '())
(defparameter *function-labels* (make-hash-table :test 'eq))
(defparameter *globals-count* 0)
(defparameter *functions-count* 0)

(format t "Compilation des formes...~%")

;; Phase 1: DEFCONSTANT, DEFPARAMETER, DEFVAR
(dolist (form *vm-forms*)
  (when (and (consp form) (member (first form) '(defconstant defvar defparameter)))
    (handler-case
        (let ((code (compile-lisp form)))
          (setf *compiled-globals* (append *compiled-globals* code))
          (incf *globals-count*))
      (error (e)
        (format t "  ✗ ~A ~A: ~A~%" (first form) (second form) e)))))

(format t "  ✓ Globales: ~A formes compilées (~A instructions)~%" 
        *globals-count* (length *compiled-globals*))

;; Phase 2: DEFUN
(dolist (form *vm-forms*)
  (when (and (consp form) (eq (first form) 'defun))
    (let ((fn-name (second form)))
      (handler-case
          (let ((code (compile-lisp form))
                (label (format nil "FN_~A" (string-upcase (symbol-name fn-name)))))
            ;; Enregistrer le label de la fonction
            (setf (gethash fn-name *function-labels*) label)
            ;; Ajouter label au début du code
            (push (list :LABEL label) code)
            (setf *compiled-functions* (append *compiled-functions* code))
            (incf *functions-count*))
        (error (e)
          (format t "  ✗ ~A: ~A~%" fn-name e))))))

(format t "  ✓ Fonctions: ~A compilées (~A instructions)~%~%" 
        *functions-count* (length *compiled-functions*))

;;; ============================================================================
;;; GÉNÉRATION SECTION .DATA
;;; ============================================================================

(defun generate-data-section ()
  "Génère la section .data avec le layout mémoire"
  (let ((lines '()))
    (push ".data" lines)
    (push "" lines)
    (push "# Layout mémoire VM" lines)
    (push "vm_memory:        .space 1048576    # 1MB RAM (adresses 0-1048575)" lines)
    (push "" lines)
    (push "# Variables d'état VM" lines)
    (push "vm_state:         .word 0           # +STATE-READY+ = 0" lines)
    (push "vm_instr_count:   .word 0           # Compteur instructions" lines)
    (push "vm_verbose:       .word 0           # Mode verbeux (0 = off)" lines)
    (push "heap_pointer:     .word 161         # +HEAP-START+ = 161" lines)
    (push "" lines)
    (push "# Constantes de layout" lines)
    (push "# +REGISTERS-START+ = 1" lines)
    (push "# +REGISTERS-SIZE+  = 160 (40 registres × 4 octets)" lines)
    (push "# +HEAP-START+      = 161" lines)
    (push "# +HEAP-LIMIT+      = heap_start + heap_size" lines)
    (push "" lines)
    (nreverse lines)))

;;; ============================================================================
;;; GÉNÉRATION SECTION .TEXT
;;; ============================================================================

(defun generate-text-section ()
  "Génère la section .text avec le point d'entrée main"
  (let ((lines '()))
    (push ".text" lines)
    (push ".globl main" lines)
    (push "" lines)
    (push "# Point d'entrée principal" lines)
    (push "main:" lines)
    (push "    # Initialisation des registres système" lines)
    (push "    li $sp, 1047552        # Stack pointer: maxmem - 1024" lines)
    (push "    li $gp, 161            # Heap pointer: +heap-start+" lines)
    (push "    li $fp, 0              # Frame pointer" lines)
    (push "" lines)
    (push "    # Initialisation des variables globales" lines)
    (push "    jal INIT_GLOBALS       # Appel obligatoire au démarrage" lines)
    (push "" lines)
    (push "    # Initialisation VM (optionnel selon utilisation)" lines)
    (push "    # jal FN_INIT_MEMORY_LAYOUT" lines)
    (push "    # jal FN_RESET_VM" lines)
    (push "" lines)
    (push "    # Point d'arrêt: programme minimal sans boucle d'exécution" lines)
    (push "    # Pour exécuter du code, décommenter les appels ci-dessus" lines)
    (push "    # ou ajouter vos propres appels de fonction ici" lines)
    (push "" lines)
    (push "    # Fin du programme" lines)
    (push "    li $v0, 10             # Syscall exit" lines)
    (push "    syscall" lines)
    (push "" lines)
    (nreverse lines)))

;;; ============================================================================
;;; CONVERSION INSTRUCTIONS ASM → MIPS TEXTE
;;; ============================================================================

(defun asm-to-mips-text (instr)
  "Convertit une instruction ASM (:LI val reg) en texte MIPS"
  (case (first instr)
    (:LABEL
     (format nil "~A:" (second instr)))
    (:LI
     (format nil "    li ~A, ~A" (third instr) (second instr)))
    (:MOVE
     (format nil "    move ~A, ~A" (third instr) (second instr)))
    (:ADD
     (format nil "    add ~A, ~A, ~A" (fourth instr) (second instr) (third instr)))
    (:SUB
     (format nil "    sub ~A, ~A, ~A" (fourth instr) (second instr) (third instr)))
    (:MUL
     (format nil "    mul ~A, ~A" (second instr) (third instr)))
    (:DIV
     (format nil "    div ~A, ~A" (second instr) (third instr)))
    (:ADDI
     (format nil "    addi ~A, ~A, ~A" (fourth instr) (second instr) (third instr)))
    (:LW
     (format nil "    lw ~A, ~A(~A)" (fourth instr) (third instr) (second instr)))
    (:SW
     (format nil "    sw ~A, ~A(~A)" (second instr) (fourth instr) (third instr)))
    (:BEQ
     (format nil "    beq ~A, ~A, ~A" (second instr) (third instr) (fourth instr)))
    (:BNE
     (format nil "    bne ~A, ~A, ~A" (second instr) (third instr) (fourth instr)))
    (:J
     (format nil "    j ~A" (second instr)))
    (:JAL
     (format nil "    jal ~A" (second instr)))
    (:JR
     (format nil "    jr ~A" (second instr)))
    (:SLT
     (format nil "    slt ~A, ~A, ~A" (fourth instr) (second instr) (third instr)))
    (:MFLO
     (format nil "    mflo ~A" (second instr)))
    (:MFHI
     (format nil "    mfhi ~A" (second instr)))
    (t
     (format nil "    # Unknown instruction: ~A" instr))))

;;; ============================================================================
;;; GÉNÉRATION FICHIER COMPLET
;;; ============================================================================

(format t "Génération du fichier MIPS complet...~%")

(with-open-file (stream "output/vm-executable.mips"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  
  ;; Header
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "#                    VM1 - Machine Virtuelle Compilée                          ~%")
  (format stream "#                     Générée automatiquement                                   ~%")
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "#~%")
  (format stream "# Source: src/vm-compilable.lisp~%")
  (format stream "# Compilateur: src/compiler.lisp (Phase 11)~%")
  (format stream "# Générateur: generate-vm-executable.lisp~%")
  (format stream "#~%")
  (format stream "# Statistiques:~%")
  (format stream "#   Globales:   ~A formes (~A instructions)~%" 
          *globals-count* (length *compiled-globals*))
  (format stream "#   Fonctions:  ~A (~A instructions)~%" 
          *functions-count* (length *compiled-functions*))
  (format stream "#   TOTAL:      ~A instructions~%" 
          (+ (length *compiled-globals*) (length *compiled-functions*)))
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "~%")
  
  ;; Section .data
  (dolist (line (generate-data-section))
    (format stream "~A~%" line))
  
  (format stream "~%")
  
  ;; Section .text
  (dolist (line (generate-text-section))
    (format stream "~A~%" line))
  
  ;; Code d'initialisation globales
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "# INITIALISATION VARIABLES GLOBALES~%")
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "~%")
  (format stream "INIT_GLOBALS:~%")
  (dolist (instr *compiled-globals*)
    (format stream "~A~%" (asm-to-mips-text instr)))
  (format stream "    jr $ra~%")
  (format stream "~%")
  
  ;; Fonctions compilées
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "# FONCTIONS VM COMPILÉES (~A fonctions)~%" *functions-count*)
  (format stream "# ═══════════════════════════════════════════════════════════════════════════════~%")
  (format stream "~%")
  (dolist (instr *compiled-functions*)
    (format stream "~A~%" (asm-to-mips-text instr))))

(format t "✓ Fichier généré: output/vm-executable.mips~%~%")

;;; ============================================================================
;;; STATISTIQUES FINALES
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════~%")
(format t "GÉNÉRATION TERMINÉE~%")
(format t "═══════════════════════════════════════════════════════════~%")
(format t "Globales:         ~A formes (~A instructions)~%" 
        *globals-count* (length *compiled-globals*))
(format t "Fonctions:        ~A (~A instructions)~%" 
        *functions-count* (length *compiled-functions*))
(format t "Instructions:     ~A MIPS~%" 
        (+ (length *compiled-globals*) (length *compiled-functions*)))
(format t "Fichier:          output/vm-executable.mips~%")
(format t "═══════════════════════════════════════════════════════════~%~%")

(format t "✅ Fichier MIPS exécutable prêt!~%~%")
(format t "Prochaines étapes:~%")
(format t "  1. Vérifier la syntaxe MIPS~%")
(format t "  2. Charger dans VM0 (si disponible)~%")
(format t "  3. Tester avec programmes simples~%")
(format t "~%")
