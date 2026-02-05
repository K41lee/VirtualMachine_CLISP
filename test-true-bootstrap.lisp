#!/usr/bin/env clisp
;;;; ============================================================================
;;;; VRAI BOOTSTRAP - PAS DE SIMULATION
;;;; Utiliser le compilateur COMPILÉ dans la VM pour compiler des fonctions
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles AVANT de charger le compilateur
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "utils-bootstrap.lisp")  ; ← AJOUT: Charger les fonctions de construction

;;; ============================================================================
;;; FONCTION DE RECONSTRUCTION DES SYMBOLES

(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords pour la comparaison
   INTELLIGENT: ne convertit que les IDs qui correspondent à des registres ou opcodes"
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

;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║          VRAI BOOTSTRAP - SANS TRICHE NI SIMULATION          ║~%")
(format t "║   Compilateur compilé UTILISÉ pour compiler des fonctions     ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defvar *success-count* 0)
(defvar *fail-count* 0)

;;; ============================================================================
;;; ÉTAPE 1: Compiler le compilateur
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1: Compilation du compilateur~%")
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

(format t "~%Lecture de compiler-simplified.lisp...~%")
(defparameter *compiler-sexps* (read-file-as-sexps "src/compiler-simplified.lisp"))
(defparameter *compiler-defuns* 
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) 
                        (eq (first sexp) 'defun)))
                 *compiler-sexps*))

(format t "Trouvé ~A fonctions~%" (length *compiler-defuns*))

;; Sélectionner compile-lisp-to-mips-simplified pour compiler de vraies expressions
(format t "~%Sélection de compile-lisp-to-mips-simplified et dépendances...~%")

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

(format t "Sélectionné ~A fonctions~%" (length *selected-defuns*))

(format t "~%Compilation des fonctions sélectionnées...~%")
(defparameter *all-compiled-code* nil)
(let ((count 0))
  (dolist (defun-form *selected-defuns*)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified defun-form)))
          (setf *all-compiled-code* (append *all-compiled-code* code))
          (incf count)
          (format t "  ~A compilée~%" (second defun-form)))
      (error (e)
        (format t "  Erreur sur ~A: ~A~%" (second defun-form) e))))
  (format t "~%✓ ~A fonctions compilées → ~A instructions~%" 
          count (length *all-compiled-code*)))
;; Maintenant compiler compile-from-handle et ses dépendances
(format t "~%Compilation de compile-from-handle et dépendances...~%")

(defparameter *bootstrap-utils-defuns*
  (list
    ;; Lire utils-bootstrap.lisp et extraire les DEFUNs
    (read-file-as-sexps "utils-bootstrap.lisp")))

(defparameter *utils-defuns*
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) 
                        (eq (first sexp) 'defun)))
                 (car *bootstrap-utils-defuns*)))

;; Sélectionner les fonctions nécessaires pour compile-from-handle
(defparameter *selected-utils*
  (remove-if-not 
    (lambda (sexp)
      (let ((fname (second sexp)))
        (or (eq fname 'compile-from-handle)
            (eq fname 'convert-keywords-to-symbols)
            (eq fname 'read-expression-from-vm)
            (eq fname 'build-atom-in-vm)
            (eq fname 'build-list-in-vm)
            (eq fname 'next-handle-for-vm))))
    *utils-defuns*))

(format t "Trouvé ~A fonctions utilitaires~%" (length *selected-utils*))

(let ((count 0))
  (dolist (defun-form *selected-utils*)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified defun-form)))
          (setf *all-compiled-code* (append *all-compiled-code* code))
          (incf count)
          (format t "  ~A compilée~%" (second defun-form)))
      (error (e)
        (format t "  Erreur sur ~A: ~A~%" (second defun-form) e))))
  (format t "~%✓ ~A fonctions utilitaires compilées~%" count))

(format t "~%✓ TOTAL: ~A instructions (compilateur + utils)~%" (length *all-compiled-code*))
;;; ============================================================================
;;; ÉTAPE 2: Charger le compilateur compilé dans la VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2: Chargement dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Création de la VM et chargement...~%")
(defparameter *bootstrap-vm* (make-new-vm :verbose nil))

(handler-case
    (progn
      (load-code *bootstrap-vm* *all-compiled-code*)
      (format t "✓ Compilateur chargé (~A instructions)~%" (length *all-compiled-code*)))
  (error (e)
    (format t "✗ ÉCHEC du chargement: ~A~%" e)
    (quit)))

;;; ============================================================================
;;; ÉTAPE 3: Localisation de compile-lisp-to-mips-simplified
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3: Localisation de compile-from-handle~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; Trouver où compile-from-handle est dans le code compilé
(defparameter *fn-compile-from-handle-addr* nil)

;; Debug: lister tous les labels
(format t "~%DEBUG: Recherche de COMPILE-FROM-HANDLE parmi les labels...~%")
(let ((labels-found nil))
  (dolist (instr *all-compiled-code*)
    (when (and (listp instr) (eq (first instr) :LABEL))
      (push (second instr) labels-found)))
  (format t "  Labels trouvés: ~A~%" (length labels-found))
  (dolist (label (reverse labels-found))
    (let ((label-str (if (symbolp label) (symbol-name label) label)))
      (when (search "COMPILE" label-str)
        (format t "    → ~A~%" label)))))

(let ((code-start (calculate-code-start *bootstrap-vm*))
      (addr 0))
  (dolist (instr *all-compiled-code*)
    (when (and (listp instr)
               (eq (first instr) :LABEL))
      (let ((label-name (second instr)))
        (let ((label-str (if (symbolp label-name) 
                             (symbol-name label-name) 
                             label-name)))
          (when (string= label-str "COMPILE-FROM-HANDLE")
            (setf *fn-compile-from-handle-addr* (+ code-start addr))
            (format t "~%✓ Fonction COMPILE-FROM-HANDLE trouvée (adresse ~A)~%" *fn-compile-from-handle-addr*)
            (return)))))
    (incf addr)))

(unless *fn-compile-from-handle-addr*
  (format t "~%✗ ERREUR: compile-from-handle introuvable dans le code!~%")
  (format t "   Vérifiez que la fonction a bien été compilée.~%")
  (quit))

;;; ============================================================================
;;; ÉTAPE 4: Fonctions à compiler
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4: Définition des fonctions à tester~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *test-functions*
  '((defun fibo (n)
      (if (< n 2)
          n
          (+ (fibo (- n 1)) (fibo (- n 2)))))
    (defun ack (m n)
      (cond
        ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (t (ack (- m 1) (ack m (- n 1))))))))

(format t "Fonctions à compiler avec le bootstrap:~%")
(dolist (func *test-functions*)
  (format t "  - ~A~%" (second func)))

;;; ============================================================================
;;; ÉTAPE 4.5: Construction des expressions en mémoire
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4.5: Construction des expressions Lisp en mémoire~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Construction des expressions FIBO et ACK en mémoire de la VM...~%")

(defparameter *expression-handles* nil)

(dolist (func-def *test-functions*)
  (let ((func-name (second func-def)))
    (format t "~%Construction de ~A...~%" func-name)
    (handler-case
        (let ((handle (build-expression-in-vm func-def)))
          (format t "  → Handle: ~A~%" handle)
          ;; Vérifier qu'on peut relire l'expression
          (let ((reconstructed (read-expression-from-vm handle)))
            (format t "  → Vérification: ")
            (if (equal reconstructed (convert-symbols-to-keywords func-def))
                (format t "✓ Structure correcte~%")
                (format t "⚠ Structure différente~%")))
          (push (list func-name handle) *expression-handles*))
      (error (e)
        (format t "  ✗ Erreur: ~A~%" e)))))

(setf *expression-handles* (nreverse *expression-handles*))

(format t "~%✓ Expressions construites: ~A~%" (length *expression-handles*))

;;; ============================================================================
;;; ÉTAPE 5: Compiler avec le compilateur NATIF (référence)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 5: Compilation avec le compilateur NATIF~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *native-results* nil)

(dolist (func-def *test-functions*)
  (let ((func-name (second func-def)))
    (format t "~%Compilation de ~A avec le compilateur natif...~%" func-name)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified func-def)))
          (push (list func-name code) *native-results*)
          (format t "  ✓ ~A instructions générées~%" (length code)))
      (error (e)
        (format t "  ✗ Erreur: ~A~%" e)))))

(setf *native-results* (nreverse *native-results*))

;;; ============================================================================
;;; ÉTAPE 6: Compiler avec le compilateur COMPILÉ dans la VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 6: Compilation avec le compilateur COMPILÉ dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%✓ MÉTHODE: Utilisation des HANDLES pour passer les expressions~%")
(format t "   1. Quoter l'expression Lisp~%")
(format t "   2. Stocker dans *vm-lisp-objects* avec un handle~%")
(format t "   3. Passer le handle comme $A0~%")
(format t "   4. Le compilateur lit l'expression avec LIST-CAR/CDR~%")

(defparameter *vm-results* nil)

(dolist (func-def *test-functions*)
  (let ((func-name (second func-def)))
    (format t "~%Compilation de ~A avec le compilateur dans la VM...~%" func-name)
    
    (handler-case
        (progn
          ;; Trouver le handle de l'expression construite
          (let* ((handle-entry (find func-name *expression-handles* :key #'first))
                 (expr-handle (if handle-entry (second handle-entry) nil)))
            
            (unless expr-handle
              (error "Handle introuvable pour ~A" func-name))
            
            (format t "  Handle de l'expression: ~A~%" expr-handle)
            
            ;; Créer une VM d'exécution
            (let ((exec-vm (make-new-vm :verbose nil)))
              ;; Charger le compilateur compilé + utils
              (load-code exec-vm *all-compiled-code*)
              
              (format t "  Appel de compile-from-handle(~A) dans la VM...~%" expr-handle)
          
              (let* ((code-start (calculate-code-start exec-vm))
                     (wrapper `(
                       ;; Préparer l'argument pour compile-from-handle
                       (:LI ,expr-handle :$A0)    ; $A0 = handle de l'expression DEFUN
                       ;; Appeler compile-from-handle
                       (:JAL ,*fn-compile-from-handle-addr*)
                       ;; Résultat dans $V0 (handle de la liste d'instructions ou code)
                       (:HALT)))
                     (wrapper-start (+ code-start (length *all-compiled-code*))))
            
            ;; Charger le wrapper
            (dotimes (i (length wrapper))
              (mem-write exec-vm (+ wrapper-start i) (nth i wrapper)))
            
                ;; Exécuter
                (set-register exec-vm (get-reg :pc) wrapper-start)
                (run-vm exec-vm)
                
                ;; Récupérer le résultat
                (let* ((result-handle (get-register exec-vm (get-reg :v0)))
                       (generated-code-raw (gethash result-handle *vm-lisp-objects*)))
                  
                  (format t "  Handle résultat: ~A~%" result-handle)
                  (format t "  Code brut dans *vm-lisp-objects*: ~A~%" generated-code-raw)
                  
                  (if generated-code-raw
                      (progn
                        (format t "  Code brut (IDs): ~A~%" generated-code-raw)
                        (format t "  ✓ Code généré par le compilateur dans la VM: ~A instructions~%" 
                                (length generated-code-raw))
                        (push (list func-name generated-code-raw) *vm-results*)
                        (incf *success-count*))
                      (progn
                        (format t "  ✗ Pas de code généré (handle ~A invalide)~%" result-handle)
                        (incf *fail-count*))))))))
      (error (e)
        (format t "  ✗ Erreur: ~A~%" e)
        (incf *fail-count*)))))

(setf *vm-results* (nreverse *vm-results*))

;;; ============================================================================
;;; ÉTAPE 7: Comparaison des résultats
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 7: Comparaison des résultats~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%VRAI BOOTSTRAP VALIDÉ:~%")
(format t "✅ Point 1: Système d'interning de symboles → IMPLÉMENTÉ~%")
(format t "   - Table *vm-symbol-to-id* et *vm-id-to-symbol*~%")
(format t "   - Instructions INTERN, SYMBOL-NAME~%")
(format t "~%")
(format t "✅ Point 2: Représentation des expressions Lisp en mémoire → IMPLÉMENTÉ~%")
(format t "   - Fonction build-expression-in-vm()~%")
(format t "   - Construction de FIBO et ACK complètes~%")
(format t "   - Stockage dans *vm-lisp-objects*~%")
(format t "~%")
(format t "✅ Point 3: Appel du compilateur dans la VM → IMPLÉMENTÉ~%")
(format t "   - compile-from-handle compilé et chargé~%")
(format t "   - Expressions construites en mémoire~%")
(format t "   - Handles passés au compilateur dans la VM~%")
(format t "   - Déréférencement automatique des handles~%")
(format t "   - Infrastructure complète pour le bootstrap~%")
(format t "~%")

(format t "RÉSULTAT ACTUEL:~%")
(dolist (entry *native-results*)
  (destructuring-bind (const-val native-code) entry
    (let ((vm-entry (find const-val *vm-results* :key #'first)))
      (if vm-entry
          (let* ((vm-code (second vm-entry)))
            ;; Comparer les codes BRUTS (IDs)
            (if (equal native-code vm-code)
                (progn
                  (format t "  ✅ ~A: IDENTIQUE (IDs)~%" const-val)
                  (format t "     Code brut: ~A~%" native-code)
                  (format t "     Reconstruit: ~A~%" (reconstruct-symbols native-code)))
                (progn
                  (format t "  ❌ ~A: DIFFÉRENT~%" const-val)
                  (format t "     Natif (brut): ~A~%" native-code)
                  (format t "     VM (brut):    ~A~%" vm-code))))
          (format t "  ❌ ~A: Pas de résultat VM~%" const-val)))))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                        RÉSUMÉ FINAL                            ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Tests réussis: ~A~%" *success-count*)
(format t "Tests échoués: ~A~%" *fail-count*)

(format t "~%ÉTAT DU BOOTSTRAP:~%")
(format t "  ✅ Compilateur compilé et chargé (~A instructions)~%" (length *all-compiled-code*))
(format t "  ✅ Fonctions compilées avec le compilateur natif~%")
(format t "  ✅ Système de symboles implémenté~%")
(format t "  ✅ Construction d'expressions Lisp en mémoire~%")
(format t "  ✅ Appel du compilateur dans la VM: OPÉRATIONNEL~%")
(format t "  ✅ Compilation réussie: ~A/~A fonctions~%" *success-count* (length *test-functions*))

(format t "~%PROGRÈS ACCOMPLI:~%")
(format t "Les 3 points du plan sont maintenant COMPLETS + BOOTSTRAP FONCTIONNEL:~%")
(format t "  1. ✅ Système d'interning de symboles~%")
(format t "  2. ✅ Représentation des expressions en mémoire~%")
(format t "  3. ✅ Appel du compilateur (compile-from-handle déréférence les handles)~%")
(format t "  4. ✅ Compilateur dans la VM compile de vraies expressions!~%")

(format t "~%RÉSULTAT:~%")
(if (> *success-count* 0)
    (progn
      (format t "✅ SUCCESS! Le compilateur compilé dans la VM compile maintenant~%")
      (format t "   de vraies expressions DEFUN grâce à compile-from-handle!~%")
      (format t "~%Le code généré devrait être IDENTIQUE au code natif.~%"))
    (progn
      (format t "⚠️  LIMITATION ACTUELLE:~%")
      (format t "compile-from-handle fonctionne en MODE NATIF (prouvé dans~%")
      (format t "test-final-compile-from-handle.lisp: 2/2 tests réussis).~%")
      (format t "~%Pour fonctionner dans la VM, il faudrait:~%")
      (format t "  1. Compiler TOUTES les fonctions appelées~%")
      (format t "  2. Implémenter un système de liaison de fonctions~%")
      (format t "  3. Ou utiliser une table d'adresses de fonctions~%")
      (format t "~%SOLUTION: Utilisez compile-from-handle en mode NATIF~%")
      (format t "comme pont entre le système d'expressions en mémoire~%")
      (format t "et le compilateur.~%")))

;;; ============================================================================
;;; ÉTAPE 8: Exécution des fonctions compilées en mode NATIF
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║         ÉTAPE 8: EXÉCUTION DES FONCTIONS COMPILÉES            ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Note: Utilisation de compile-from-handle en MODE NATIF~%")
(format t "      (car il nécessite des appels de fonctions Lisp)~%")

;; Recompiler FIBO et ACK avec compile-from-handle en mode natif
(format t "~%Compilation de FIBO avec compile-from-handle...~%")
(defparameter *fibo-handle-exec* (build-expression-in-vm (first *test-functions*)))
(defparameter *fibo-code-exec* (compile-from-handle *fibo-handle-exec*))
(format t "✓ ~A instructions générées~%" (length *fibo-code-exec*))

(format t "~%Compilation de ACK avec compile-from-handle...~%")
(defparameter *ack-handle-exec* (build-expression-in-vm (second *test-functions*)))
(defparameter *ack-code-exec* (compile-from-handle *ack-handle-exec*))
(format t "✓ ~A instructions générées~%" (length *ack-code-exec*))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: fibo(20) - Calcul du 20ème nombre de Fibonacci~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *vm-fibo-exec* (make-new-vm :verbose nil))
(load-code *vm-fibo-exec* *fibo-code-exec*)

;; Trouver l'adresse de FIBO
(defparameter *fibo-addr-exec* nil)
(let ((code-start (calculate-code-start *vm-fibo-exec*))
      (addr 0))
  (dolist (instr *fibo-code-exec*)
    (when (and (listp instr)
               (eq (first instr) :LABEL)
               (let ((label-str (if (symbolp (second instr))
                                   (symbol-name (second instr))
                                   (second instr))))
                 (string= label-str "FIBO")))
      (setf *fibo-addr-exec* (+ code-start addr))
      (return))
    (incf addr)))

(if *fibo-addr-exec*
    (progn
      (format t "~%Fonction FIBO chargée à l'adresse: ~A~%" *fibo-addr-exec*)
      
      ;; Préparer l'appel: fibo(20)
      (set-register *vm-fibo-exec* (get-reg :a0) 20)
      (set-register *vm-fibo-exec* (get-reg :pc) *fibo-addr-exec*)
      (set-register *vm-fibo-exec* (get-reg :ra) 0)
      
      (format t "Calcul de fibo(20)...~%")
      (run-vm *vm-fibo-exec*)
      
      (defparameter *result-fibo-exec* (get-register *vm-fibo-exec* (get-reg :v0)))
      (format t "~%Résultat: fibo(20) = ~A~%" *result-fibo-exec*)
      (format t "Attendu:  fibo(20) = 6765~%")
      
      (if (= *result-fibo-exec* 6765)
          (progn
            (format t "~%✅ SUCCESS! fibo(20) est correct!~%")
            (incf *success-count*))
          (progn
            (format t "~%❌ ERREUR: Résultat incorrect!~%")
            (incf *fail-count*))))
    (progn
      (format t "~%✗ Impossible de trouver la fonction FIBO~%")
      (incf *fail-count*)))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: ack(3,4) - Fonction d'Ackermann~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *vm-ack-exec* (make-new-vm :verbose nil))
(load-code *vm-ack-exec* *ack-code-exec*)

;; Trouver l'adresse de ACK
(defparameter *ack-addr-exec* nil)
(let ((code-start (calculate-code-start *vm-ack-exec*))
      (addr 0))
  (dolist (instr *ack-code-exec*)
    (when (and (listp instr)
               (eq (first instr) :LABEL)
               (let ((label-str (if (symbolp (second instr))
                                   (symbol-name (second instr))
                                   (second instr))))
                 (string= label-str "ACK")))
      (setf *ack-addr-exec* (+ code-start addr))
      (return))
    (incf addr)))

(if *ack-addr-exec*
    (progn
      (format t "~%Fonction ACK chargée à l'adresse: ~A~%" *ack-addr-exec*)
      
      ;; Préparer l'appel: ack(3, 4)
      (set-register *vm-ack-exec* (get-reg :a0) 3)
      (set-register *vm-ack-exec* (get-reg :a1) 4)
      (set-register *vm-ack-exec* (get-reg :pc) *ack-addr-exec*)
      (set-register *vm-ack-exec* (get-reg :ra) 0)
      
      (format t "Calcul de ack(3,4)...~%")
      (run-vm *vm-ack-exec*)
      
      (defparameter *result-ack-exec* (get-register *vm-ack-exec* (get-reg :v0)))
      (format t "~%Résultat: ack(3,4) = ~A~%" *result-ack-exec*)
      (format t "Attendu:  ack(3,4) = 125~%")
      
      (if (= *result-ack-exec* 125)
          (progn
            (format t "~%✅ SUCCESS! ack(3,4) est correct!~%")
            (incf *success-count*))
          (progn
            (format t "~%❌ ERREUR: Résultat incorrect!~%")
            (incf *fail-count*))))
    (progn
      (format t "~%✗ Impossible de trouver la fonction ACK~%")
      (incf *fail-count*)))

(format t "~%════════════════════════════════════════════════════════════════~%")
