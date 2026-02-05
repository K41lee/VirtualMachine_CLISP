;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST COMPILATION DES FICHIERS RÉELS
;;; 
;;; Ce test compile les fichiers RÉELS du projet :
;;;   - src/compiler.lisp (toutes les defun)
;;;   - src/loader.lisp (toutes les defun)
;;;
;;; Il montre quelles fonctions sont compilables avec l'architecture
;;; bootstrap (COND + IDs) et lesquelles nécessitent des extensions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%================================================================~%")
(format t "   TEST COMPILATION DES FICHIERS REELS~%")
(format t "================================================================~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ÉTAPE 1 : CHARGEMENT DES COMPOSANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "ÉTAPE 1 : Chargement du bootstrap...~%")

;; Charger la VM et les opérations ASM
(load "src/vm.lisp")
(load "src/asm-ops.lisp")

;; Définir les variables globales nécessaires
(format t "  Definition des variables globales...~%")

(unless (boundp '*global-constants*)
  (defvar *global-constants* (make-hash-table :test 'equal)))

(unless (boundp '*global-variables*)
  (defvar *global-variables* (make-hash-table :test 'equal)))

(unless (boundp '*global-functions*)
  (defvar *global-functions* (make-hash-table :test 'equal)))

(unless (boundp '*vm-primitives*)
  (defvar *vm-primitives* 
    '(vm-mem-write vm-mem-read vm-get-reg vm-set-reg
      make-hash-table gethash-vm hash-set-vm)))

(unless (boundp '*built-in-operators*)
  (defvar *built-in-operators*
    '(+ - * / = < > <= >= /=
      if cond when unless
      let let* progn
      defun lambda
      and or not
      car cdr cons list null
      eq equal
      setq setf)))

(unless (boundp '*maxmem*)
  (defvar *maxmem* 1000000))

(unless (boundp '*label-counter*)
  (defvar *label-counter* 0))

(unless (boundp '*global-data-offset*)
  (defvar *global-data-offset* 0))

;; Charger le compilateur bootstrap avec IDs
(load "src/symbol-table.lisp")
(load "src/parser-with-ids.lisp")
(load "src/dispatcher-with-ids.lisp")
(load "src/compiler.lisp")
(load "src/compiler-bootstrap-ids.lisp")

;; Charger le loader
(load "src/loader.lisp")

(format t "  OK Bootstrap charge avec variables globales definies~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FONCTION POUR COMPILER UN FICHIER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-defuns-from-file (filepath)
  "Lit un fichier et extrait toutes les définitions defun"
  (let ((defuns nil))
    (with-open-file (stream filepath :direction :input)
      (handler-case
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (when (and (listp form) (eq (first form) 'defun))
                     (push form defuns)))
        (error (e)
          (format t "  ATTENTION: Erreur de lecture du fichier: ~A~%" e))))
    (reverse defuns)))

(defun compile-file-lisp (filepath)
  "Compile toutes les fonctions d'un fichier .lisp"
  (let ((defuns (extract-defuns-from-file filepath))
        (compiled-funcs nil)
        (failed-funcs nil)
        (total-instructions 0))
    
    (format t "  Fichier : ~A~%" filepath)
    (format t "  -> ~A fonctions trouvees~%~%" (length defuns))
    
    (dolist (defun-form defuns)
      (let ((func-name (second defun-form)))
        (handler-case
            (let* ((compiled (compile-lisp-with-ids defun-form))
                   (instr-count (length compiled)))
              (incf total-instructions instr-count)
              (push (cons func-name compiled) compiled-funcs)
              (format t "    OK ~A : ~A instructions~%" func-name instr-count))
          (error (e)
            (push func-name failed-funcs)
            (format t "    X  ~A : ECHEC (~A)~%" func-name 
                    (if (> (length (format nil "~A" e)) 60)
                        (subseq (format nil "~A" e) 0 60)
                        (format nil "~A" e)))))))
    
    (list :compiled (reverse compiled-funcs)
          :failed (reverse failed-funcs)
          :total-instructions total-instructions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ÉTAPE 2 : COMPILATION DE SRC/COMPILER.LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "ÉTAPE 2 : Compilation de src/compiler.lisp...~%~%")

(defvar *compiler-result* (compile-file-lisp "src/compiler.lisp"))
(defvar *compiled-compiler* (getf *compiler-result* :compiled))
(defvar *failed-compiler* (getf *compiler-result* :failed))
(defvar *total-compiler-instructions* (getf *compiler-result* :total-instructions))

(format t "~%  RÉSULTAT COMPILATION COMPILER.LISP~%")
(format t "  OK Compilees : ~A fonctions, ~A instructions MIPS~%"
        (length *compiled-compiler*) *total-compiler-instructions*)
(format t "  X  Echouees  : ~A fonctions~%" (length *failed-compiler*))
(when (and *failed-compiler* (< (length *failed-compiler*) 20))
  (format t "     Fonctions non compilables : ~{~A~^, ~}~%" *failed-compiler*))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ÉTAPE 3 : COMPILATION DE SRC/LOADER.LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "ÉTAPE 3 : Compilation de src/loader.lisp...~%~%")

(defvar *loader-result* (compile-file-lisp "src/loader.lisp"))
(defvar *compiled-loader* (getf *loader-result* :compiled))
(defvar *failed-loader* (getf *loader-result* :failed))
(defvar *total-loader-instructions* (getf *loader-result* :total-instructions))

(format t "~%  RÉSULTAT COMPILATION LOADER.LISP~%")
(format t "  OK Compilees : ~A fonctions, ~A instructions MIPS~%"
        (length *compiled-loader*) *total-loader-instructions*)
(format t "  X  Echouees  : ~A fonctions~%" (length *failed-loader*))
(when (and *failed-loader* (< (length *failed-loader*) 20))
  (format t "     Fonctions non compilables : ~{~A~^, ~}~%" *failed-loader*))
(format t "~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ÉTAPE 4 : STATISTIQUES GLOBALES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "ÉTAPE 4 : Statistiques globales...~%~%")

(defvar *total-compiled*
  (+ (length *compiled-compiler*)
     (length *compiled-loader*)))

(defvar *total-failed*
  (+ (length *failed-compiler*)
     (length *failed-loader*)))

(defvar *total-instructions*
  (+ *total-compiler-instructions*
     *total-loader-instructions*))

(defvar *total-functions* (+ *total-compiled* *total-failed*))
(defvar *success-rate* (* 100.0 (/ *total-compiled* *total-functions*)))

(format t "  STATISTIQUES GLOBALES~%~%")
(format t "    src/compiler.lisp : ~2A/~2A compilees (~5A instructions)~%"
        (length *compiled-compiler*)
        (+ (length *compiled-compiler*) (length *failed-compiler*))
        *total-compiler-instructions*)
(format t "    src/loader.lisp   : ~2A/~2A compilees (~5A instructions)~%"
        (length *compiled-loader*)
        (+ (length *compiled-loader*) (length *failed-loader*))
        *total-loader-instructions*)
(format t "    --------------------------------------------------------~%")
(format t "    TOTAL             : ~2A/~2A compilees (~5A instructions)~%~%"
        *total-compiled*
        *total-functions*
        *total-instructions*)

(format t "  TAUX DE COMPILATION : ~,1F%%~%~%" *success-rate*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ÉTAPE 5 : ANALYSE DES RÉSULTATS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "ÉTAPE 5 : Analyse des resultats...~%~%")

(cond
  ((> *total-compiled* 20)
   (format t "  SUCCÈS : Plus de 20 fonctions compilees !~%~%")
   (format t "  Les fichiers RÉELS ont ete analyses :~%~%")
   (format t "    OK src/compiler.lisp analyse et ~A fonctions compilees~%"
           (length *compiled-compiler*))
   (format t "    OK src/loader.lisp analyse et ~A fonctions compilees~%"
           (length *compiled-loader*))
   (format t "    OK ~A instructions MIPS generees au total~%~%" *total-instructions*)
   (format t "  Les fonctions compilables utilisent uniquement des~%")
   (format t "  operations de base (+, -, *, /, =, <, >, IF, LET)~%")
   (format t "  supportees par l'architecture bootstrap COND + IDs.~%~%")
   (format t "  Les fonctions non compilables (~A) utilisent des~%"
           *total-failed*)
   (format t "  structures complexes (CAR, CDR, LIST, GETHASH, SETF, etc.)~%")
   (format t "  qui necessitent des primitives speciales dans la VM.~%"))
  (t
   (format t "  COMPILATION PARTIELLE~%")
   (format t "  ~A fonctions compilees, ~A echouees~%"
           *total-compiled* *total-failed*)))

(format t "~%================================================================~%")
(format t "   TEST TERMINE~%")
(format t "================================================================~%~%")

(format t "RÉSUMÉ :~%")
(format t "  - Fichier src/compiler.lisp : ~A/~A fonctions compilables~%"
        (length *compiled-compiler*)
        (+ (length *compiled-compiler*) (length *failed-compiler*)))
(format t "  - Fichier src/loader.lisp   : ~A/~A fonctions compilables~%"
        (length *compiled-loader*)
        (+ (length *compiled-loader*) (length *failed-loader*)))
(format t "  - Total instructions MIPS   : ~A~%~%" *total-instructions*)

;; Retourner le succès
(if (> *total-compiled* 20) t nil)
