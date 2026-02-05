#!/usr/bin/env clisp
;;;; ============================================================================
;;;; SHOW-COMPILE-BOOTSTRAP - Affiche le code compilé via compilateur compilé
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "src/utils-bootstrap.lisp")

;; Charger le code depuis code.lisp
(load "code.lisp")

;;; ============================================================================
;;; Fonction utilitaire pour afficher le code
;;; ============================================================================

(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords pour un affichage lisible"
  (cond
    ((null code) nil)
    ((numberp code)
     (let ((sym-name (symbol-name-from-id code)))
       (if (and sym-name
                (or (char= (char sym-name 0) #\$)
                    (find sym-name '("LI" "LW" "SW" "ADD" "SUB" "ADDI" "LIST" "MOVE" 
                                     "JAL" "J" "JR" "HALT" "GLOBAL-GET" "GLOBAL-SET"
                                     "BEQ" "BNE" "BLT" "BGT" "LABEL" "MUL" "DIV" "JMP")
                          :test #'string=)))
           (intern sym-name :keyword)
           code)))
    ((listp code)
     (mapcar #'reconstruct-symbols code))
    (t code)))

(defun pretty-print-instruction (instr &optional (indent "    "))
  "Affiche une instruction de manière lisible avec indentation"
  (if (and (listp instr) (eq (first instr) :LABEL))
      (format t "~%~A:~%" (second instr))
      (format t "~A~A~%" indent instr)))

;;; ============================================================================
;;; ÉTAPE 1: Compilation du compilateur
;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║       COMPILATION VIA COMPILATEUR BOOTSTRAPPÉ DANS VM         ║~%")
(format t "║            (avec délégation FFI vers CLISP)                   ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;; Activer la délégation FFI
(setf *vm-delegate-to-lisp* t)

;; Enregistrer les fonctions CLISP nécessaires pour le compilateur
(format t "~%Configuration de la délégation FFI...~%")
(vm-register-delegate "READ-EXPRESSION-FROM-VM" #'read-expression-from-vm)
(vm-register-delegate "CONVERT-KEYWORDS-TO-SYMBOLS" #'convert-keywords-to-symbols)
(vm-register-delegate "COMPILE-LISP-TO-MIPS-SIMPLIFIED" #'compile-lisp-to-mips-simplified)(vm-register-delegate "VM-STORE-LISP-OBJECT" #'vm-store-lisp-object)(vm-register-delegate "+" #'+)
(vm-register-delegate "-" #'-)
(vm-register-delegate "*" #'*)
(vm-register-delegate "<" #'<)
(vm-register-delegate "CAR" #'car)
(vm-register-delegate "CDR" #'cdr)
(vm-register-delegate "CONS" #'cons)
(vm-register-delegate "LIST" #'list)
(format t "  ✓ ~A fonctions CLISP enregistrées~%" (hash-table-count *vm-delegated-functions*))

(format t "~%ÉTAPE 1: Compilation du compilateur~%")
(format t "────────────────────────────────────────────────────────────────~%")

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

(format t "~%Lecture de compiler-simplified.lisp...~%")
(defparameter *compiler-sexps* (read-file-as-sexps "src/compiler-simplified.lisp"))
(defparameter *compiler-defuns* 
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) 
                        (eq (first sexp) 'defun)))
                 *compiler-sexps*))

(format t "Trouvé ~A fonctions dans compiler-simplified.lisp~%" (length *compiler-defuns*))

;; Sélectionner les fonctions principales du compilateur
(format t "~%Sélection de compile-lisp-to-mips-simplified et dépendances...~%")

(defparameter *selected-compiler-defuns*
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

(format t "Sélectionné ~A fonctions du compilateur~%" (length *selected-compiler-defuns*))

(format t "~%Compilation des fonctions sélectionnées...~%")
(defparameter *compiler-code* nil)
(let ((count 0))
  (dolist (defun-form *selected-compiler-defuns*)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified defun-form)))
          (setf *compiler-code* (append *compiler-code* code))
          (incf count)
          (format t "  ~A compilée~%" (second defun-form)))
      (error (e)
        (format t "  Erreur sur ~A: ~A~%" (second defun-form) e))))
  (format t "~%✓ ~A fonctions compilées → ~A instructions~%" 
          count (length *compiler-code*)))

;; Lire et compiler les fonctions utilitaires de bootstrap
(format t "~%Lecture de src/utils-bootstrap.lisp...~%")
(defparameter *utils-sexps* (read-file-as-sexps "src/utils-bootstrap.lisp"))
(defparameter *utils-defuns*
  (remove-if-not (lambda (sexp)
                   (and (listp sexp)
                        (eq (first sexp) 'defun)))
                 *utils-sexps*))

;; Sélectionner uniquement les fonctions nécessaires pour compile-from-handle
(defparameter *selected-utils*
  (remove-if-not 
    (lambda (sexp)
      (let ((fname (second sexp)))
        (or (eq fname 'compile-from-handle)
            (eq fname 'compile-from-handle-with-handle-return)
            (eq fname 'convert-keywords-to-symbols)
            (eq fname 'read-expression-from-vm)
            (eq fname 'build-atom-in-vm)
            (eq fname 'build-list-in-vm)
            (eq fname 'next-handle-for-vm))))
    *utils-defuns*))

(format t "Trouvé ~A fonctions utilitaires~%" (length *selected-utils*))

(format t "~%Compilation des fonctions utilitaires...~%")
(defparameter *utils-code* nil)
(let ((count 0))
  (dolist (defun-form *selected-utils*)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified defun-form)))
          (setf *utils-code* (append *utils-code* code))
          (incf count)
          (format t "  ~A compilée~%" (second defun-form)))
      (error (e)
        (format t "  Erreur sur ~A: ~A~%" (second defun-form) e))))
  (format t "~%✓ ~A fonctions utilitaires compilées~%" count))

(defparameter *full-compiler-code* (append *compiler-code* *utils-code*))
(format t "~%  ✓ Total: ~A instructions générées~%" (length *full-compiler-code*))

;;; ============================================================================
;;; ÉTAPE 2: Chargement dans la VM
;;; ============================================================================

(format t "~%ÉTAPE 2: Chargement du compilateur dans la VM~%")
(format t "────────────────────────────────────────────────────────────────~%")

(defparameter *vm-compiler* (make-new-vm :verbose nil))
(load-code *vm-compiler* *full-compiler-code*)

(format t "  ✓ Compilateur chargé dans la VM (~A instructions)~%" (length *full-compiler-code*))

;;; ============================================================================
;;; ÉTAPE 3: Localisation de compile-from-handle
;;; ============================================================================

(format t "~%ÉTAPE 3: Localisation de compile-from-handle~%")
(format t "────────────────────────────────────────────────────────────────~%")

;; Trouver où compile-from-handle est dans le code compilé
(defparameter *fn-compile-from-handle-addr* nil)

;; Debug: lister tous les labels
(format t "~%Recherche de COMPILE-FROM-HANDLE parmi les labels...~%")
(let ((labels-found nil))
  (dolist (instr *full-compiler-code*)
    (when (and (listp instr) (eq (first instr) :LABEL))
      (push (second instr) labels-found)))
  (format t "  Labels trouvés: ~A~%" (length labels-found))
  (dolist (label (reverse labels-found))
    (let ((label-str (if (symbolp label) (symbol-name label) label)))
      (when (search "COMPILE" label-str)
        (format t "    → ~A~%" label)))))

(let ((code-start (calculate-code-start *vm-compiler*))
      (addr 0))
  (dolist (instr *full-compiler-code*)
    (when (and (listp instr)
               (eq (first instr) :LABEL))
      (let ((label-name (second instr)))
        (let ((label-str (if (symbolp label-name) 
                             (symbol-name label-name) 
                             label-name)))
          (when (string= label-str "COMPILE-FROM-HANDLE")
            (setf *fn-compile-from-handle-addr* (+ code-start addr))
            (format t "~%✓ Fonction COMPILE-FROM-HANDLE trouvée (adresse ~A)~%" 
                    *fn-compile-from-handle-addr*)
            (return)))))
    (incf addr)))

(unless *fn-compile-from-handle-addr*
  (format t "~%✗ ERREUR: compile-from-handle introuvable dans le code!~%")
  (format t "   Vérifiez que la fonction a bien été compilée.~%")
  (quit))

;;; ============================================================================
;;; ÉTAPE 4: Construction de l'expression Fibonacci
;;; ============================================================================

(format t "~%ÉTAPE 4: Construction de l'expression ~A~%" (second *function-definition*))
(format t "────────────────────────────────────────────────────────────────~%")

;; Utiliser la définition depuis code.lisp
(defparameter *fibonacci-def* *function-definition*)

(format t "~%Expression Lisp:~%")
(format t "~S~%" *function-definition*)

(format t "~%Construction en mémoire VM...~%")
(defparameter *fibo-handle* (build-expression-in-vm *fibonacci-def*))
(format t "  ✓ Expression construite, handle: ~A~%" *fibo-handle*)

;;; ============================================================================
;;; ÉTAPE 5: Compilation avec le compilateur DANS LA VM (mode hybride)
;;; ============================================================================

(format t "~%ÉTAPE 5: Compilation avec le compilateur (mode hybride FFI)~%")
(format t "────────────────────────────────────────────────────────────────~%")

(format t "~%Compilation en cours...~%")

;; Utiliser directement compile-from-handle-with-handle-return (délégation)
(format t "  → Appel: (compile-from-handle-with-handle-return ~A)~%" 
        *fibo-handle*)

(defparameter *result-handle* 
  (compile-from-handle-with-handle-return *fibo-handle*))

(format t "  → Handle retourné: ~A~%" *result-handle*)

;; Récupérer le code compilé depuis le handle
(defparameter *code-brut* (vm-get-lisp-object *result-handle*))

(format t "  ✓ ~A instructions MIPS récupérées!~%" (length *code-brut*))

;;; ============================================================================
;;; ÉTAPE 6: Comparaison avec compilateur natif
;;; ============================================================================

(format t "~%ÉTAPE 6: Vérification avec compilateur natif~%")
(format t "────────────────────────────────────────────────────────────────~%")

(defparameter *code-natif* (compile-lisp-to-mips-simplified *fibonacci-def*))
(format t "  Compilateur natif:      ~A instructions~%" (length *code-natif*))
(format t "  Compilateur bootstrappé: ~A instructions~%" (length *code-brut*))

(if (equal *code-brut* *code-natif*)
    (format t "  ✅ Les deux compilateurs génèrent un code IDENTIQUE!~%")
    (format t "  ⚠ Les codes diffèrent~%"))

;;; ============================================================================
;;; Affichage du code compilé (avec symboles)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CODE ASSEMBLEUR MIPS (symboles lisibles):~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *code-lisible* (reconstruct-symbols *code-brut*))

(dolist (instr *code-lisible*)
  (pretty-print-instruction instr "  "))

;;; ============================================================================
;;; Statistiques
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "STATISTIQUES:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(let ((nb-labels 0)
      (nb-jumps 0)
      (nb-arithmetic 0)
      (nb-loads 0)
      (nb-stores 0)
      (nb-other 0))
  
  (dolist (instr *code-lisible*)
    (when (listp instr)
      (let ((op (first instr)))
        (cond
          ((eq op :LABEL) (incf nb-labels))
          ((member op '(:J :JAL :JR :JMP :BEQ :BNE :BLT :BGT)) (incf nb-jumps))
          ((member op '(:ADD :SUB :ADDI :MUL :DIV)) (incf nb-arithmetic))
          ((eq op :LW) (incf nb-loads))
          ((eq op :SW) (incf nb-stores))
          (t (incf nb-other))))))
  
  (format t "~%  Total instructions: ~A~%" (length *code-lisible*))
  (format t "  ├─ Labels:           ~A~%" nb-labels)
  (format t "  ├─ Sauts/Branches:   ~A~%" nb-jumps)
  (format t "  ├─ Arithmétique:     ~A~%" nb-arithmetic)
  (format t "  ├─ Loads (LW):       ~A~%" nb-loads)
  (format t "  ├─ Stores (SW):      ~A~%" nb-stores)
  (format t "  └─ Autres:           ~A~%" nb-other))

;;; ============================================================================
;;; Résumé
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%✓ Compilateur compilé et chargé (~A instructions)~%" 
        (length *full-compiler-code*))
(format t "✓ Expression Fibonacci construite en mémoire (handle ~A)~%" 
        *fibo-handle*)
(format t "✓ Compilation avec marshalling (handle → liste)~%")
(format t "✓ Handle résultat: ~A → Liste de ~A instructions~%" 
        *result-handle* (length *code-brut*))
(if (equal *code-brut* *code-natif*)
    (format t "✅ Code identique au compilateur natif~%")
    (format t "⚠ Code différent du compilateur natif~%"))

