#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST: Comparaison FIBO - Compilateur Natif vs Compilé
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "utils-bootstrap.lisp")

(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords"
  (cond
    ((null code) nil)
    ((numberp code)
     (let ((sym-name (symbol-name-from-id code)))
       (if (and sym-name
                (or (char= (char sym-name 0) #\$)
                    (find sym-name '("LI" "LW" "SW" "ADD" "SUB" "ADDI" "LIST" "MOVE" 
                                     "JAL" "J" "JR" "HALT" "GLOBAL-GET" "GLOBAL-SET"
                                     "BEQ" "BNE" "BLT" "BGT" "LABEL" "MUL" "DIV")
                          :test #'string=)))
           (intern sym-name :keyword)
           code)))
    ((listp code)
     (mapcar #'reconstruct-symbols code))
    (t code)))

(defun pretty-print-code (code)
  "Affiche le code MIPS de manière lisible"
  (dolist (instr code)
    (if (and (listp instr) (eq (first instr) :LABEL))
        (format t "~%~A:~%" (second instr))
        (format t "    ~A~%" instr))))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║         COMPARAISON: FIBO Natif vs Compilé                    ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *fibo-def*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

;;; ============================================================================
;;; ÉTAPE 1: Compilation avec le compilateur NATIF
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1: Compilation avec le compilateur NATIF~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Expression: ~A~%" *fibo-def*)

(defparameter *native-code* 
  (compile-lisp-to-mips-simplified *fibo-def*))

(format t "~%✓ Code généré: ~A instructions~%" (length *native-code*))

(format t "~%CODE NATIF (brut avec IDs):~%")
(format t "~A~%~%" *native-code*)

(format t "CODE NATIF (reconstruit avec symboles):~%")
(defparameter *native-code-readable* (reconstruct-symbols *native-code*))
(pretty-print-code *native-code-readable*)

;;; ============================================================================
;;; ÉTAPE 2: Compiler le compilateur et le charger dans la VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2: Compilation du compilateur lui-même~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defun read-file-as-sexps (filename)
  "Lit un fichier et retourne toutes les S-expressions"
  (with-open-file (stream filename :direction :input)
    (let ((sexps nil))
      (handler-case
          (loop
            (let ((sexp (read stream nil :eof)))
              (if (eq sexp :eof)
                  (return (nreverse sexps))
                  (when (listp sexp)
                    (push sexp sexps)))))
        (end-of-file () (nreverse sexps))))))

(defparameter *compiler-sexps* (read-file-as-sexps "src/compiler-simplified.lisp"))
(defparameter *compiler-defuns* 
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) 
                        (eq (first sexp) 'defun)))
                 *compiler-sexps*))

;; Sélectionner les fonctions nécessaires
(defparameter *selected-defuns*
  (remove-if-not 
    (lambda (sexp)
      (let ((fname (second sexp)))
        (or (eq fname 'compile-lisp-to-mips-simplified)
            (eq fname 'compile-constant-simplified)
            (eq fname 'compile-variable-simplified)
            (eq fname 'compile-if-simplified)
            (eq fname 'compile-funcall-simplified)
            (eq fname 'compile-defun-simplified)
            (eq fname 'compile-lambda-simplified)
            (eq fname 'compile-list-simplified)
            (eq fname 'compile-cond-simplified)
            (eq fname 'compile-arithmetic-simplified))))
    *compiler-defuns*))

(format t "~%Compilation de ~A fonctions du compilateur...~%" (length *selected-defuns*))

(defparameter *compiled-compiler-code* nil)
(dolist (defun-form *selected-defuns*)
  (handler-case
      (let ((code (compile-lisp-to-mips-simplified defun-form)))
        (setf *compiled-compiler-code* (append *compiled-compiler-code* code)))
    (error (e)
      (format t "  Erreur sur ~A: ~A~%" (second defun-form) e))))

(format t "✓ Compilateur compilé: ~A instructions~%" (length *compiled-compiler-code*))

;; Charger dans une VM
(defparameter *bootstrap-vm* (make-new-vm :verbose nil))
(load-code *bootstrap-vm* *compiled-compiler-code*)

(format t "✓ Compilateur chargé dans la VM~%")

;;; ============================================================================
;;; ÉTAPE 3: Construire FIBO en mémoire et compiler avec le compilateur compilé
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3: Compilation avec le compilateur COMPILÉ dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Construction de FIBO en mémoire de la VM...~%")
(defparameter *fibo-handle* (build-expression-in-vm *fibo-def*))
(format t "✓ FIBO construite, handle: ~A~%" *fibo-handle*)

;; Vérifier la reconstruction
(defparameter *fibo-reconstructed* (read-expression-from-vm *fibo-handle*))
(format t "✓ Expression reconstruite: ~A~%" *fibo-reconstructed*)

(format t "~%Appel du compilateur dans la VM avec le handle ~A...~%" *fibo-handle*)

(let* ((code-start (calculate-code-start *bootstrap-vm*))
       (fn-addr (+ code-start 1))  ; Après le J initial
       (wrapper `(
         ;; Préparer l'argument: handle de l'expression FIBO
         (:LI ,*fibo-handle* :$A0)
         ;; Appeler compile-lisp-to-mips-simplified
         (:JAL ,fn-addr)
         ;; Résultat dans $V0
         (:HALT)))
       (wrapper-start (+ code-start (length *compiled-compiler-code*))))
  
  ;; Charger le wrapper
  (dotimes (i (length wrapper))
    (mem-write *bootstrap-vm* (+ wrapper-start i) (nth i wrapper)))
  
  ;; Exécuter
  (set-register *bootstrap-vm* (get-reg :pc) wrapper-start)
  (run-vm *bootstrap-vm*)
  
  ;; Récupérer le résultat
  (defparameter *result-handle* (get-register *bootstrap-vm* (get-reg :v0)))
  (defparameter *vm-code* (gethash *result-handle* *vm-lisp-objects*))
  
  (format t "~%Handle résultat: ~A~%" *result-handle*)
  
  (if *vm-code*
      (progn
        (format t "✓ Code généré: ~A instruction(s)~%" (length *vm-code*))
        
        (format t "~%CODE COMPILÉ DANS LA VM (brut avec IDs):~%")
        (format t "~A~%~%" *vm-code*)
        
        (format t "CODE COMPILÉ DANS LA VM (reconstruit avec symboles):~%")
        (defparameter *vm-code-readable* (reconstruct-symbols *vm-code*))
        (pretty-print-code *vm-code-readable*))
      (format t "✗ Pas de code généré~%")))

;;; ============================================================================
;;; ÉTAPE 4: Comparaison
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4: Comparaison des résultats~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Longueur code natif:   ~A instructions~%" (length *native-code*))
(format t "Longueur code VM:      ~A instruction(s)~%" (if *vm-code* (length *vm-code*) 0))

(if (and *vm-code* (equal *native-code* *vm-code*))
    (format t "~%✅ IDENTIQUE! Les deux compilateurs génèrent le même code!~%")
    (format t "~%❌ DIFFÉRENT! Les codes ne sont pas identiques.~%"))

(format t "~%ANALYSE:~%")
(if (and *vm-code* (= (length *vm-code*) 1))
    (format t "Le compilateur compilé retourne juste ((LI ~A $V0)) car il traite~%le handle comme une constante. Pour compiler réellement l'expression,~%il faudrait implémenter le déréférencement des handles.~%" *fibo-handle*)
    (if *vm-code*
        (format t "Le compilateur dans la VM a généré du code!~%")
        (format t "Pas de code généré par le compilateur dans la VM.~%")))

(format t "~%════════════════════════════════════════════════════════════════~%")
