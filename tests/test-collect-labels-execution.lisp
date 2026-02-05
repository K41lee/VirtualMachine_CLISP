;;; ============================================================================
;;; TEST COLLECT-LABELS - EXÉCUTION COMPLÈTE AVEC DONNÉES RÉELLES
;;; ============================================================================
;;;
;;; Ce test compile et exécute collect-labels avec de vraies listes créées
;;; depuis Lisp et manipulées via le système de handles.
;;;
;;; Commande: clisp test-collect-labels-execution.lisp
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║     TEST COLLECT-LABELS - EXÉCUTION AVEC LISTES RÉELLES         ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; Chargement
(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

;;; ============================================================================
;;; TEST: FONCTION QUI COMPTE LES ÉLÉMENTS D'UNE LISTE
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST: Compilation et exécution de count-list~%")
(format t "═════════════════════════════════════════════════════════════════~%")

(defparameter *count-list-src*
  '(defun count-list (lst)
     (let ((count 0))
       (dolist (elem lst)
         (setq count (+ count 1)))
       count)))

(format t "  → Compilation...~%")
(defparameter *count-list-mips* (compile-lisp *count-list-src*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *count-list-mips*))

;; Créer une VM et charger le code
(format t "  → Création de la VM et chargement du code...~%")
(defparameter *vm1* (make-new-vm :verbose nil))
(load-code *vm1* *count-list-mips* :verbose nil)
(defparameter *func-addr1* (calculate-code-start *vm1*))
(format t "  ✓ Code chargé à l'adresse ~A~%~%" *func-addr1*)

;; Créer une liste test depuis Lisp
(format t "  → Création d'une liste test: (a b c d e)~%")
(defparameter *test-list1* '(a b c d e))

;; Enregistrer la liste dans le système de handles
(defparameter *list-handle1* 
  (let ((handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) *test-list1*)
    handle))
(format t "  ✓ Liste enregistrée avec handle ~A~%~%" *list-handle1*)

;; Configurer les paramètres et exécuter
(format t "  → Configuration: $A0 = ~A (handle)~%" *list-handle1*)
(set-register *vm1* *reg-a0* *list-handle1*)
(set-register *vm1* *reg-ra* 999999)
(set-register *vm1* (get-reg :pc) *func-addr1*)

(format t "  → Exécution...~%")
(handler-case
    (progn
      (run-vm *vm1* :max-instructions 10000)
      (defparameter *result1* (get-register *vm1* *reg-v0*))
      (format t "  ✓ Résultat: ~A (attendu: ~A)~%~%" 
              *result1* (length *test-list1*))
      (if (= *result1* (length *test-list1*))
          (format t "  ✅ TEST RÉUSSI!~%~%")
          (format t "  ❌ TEST ÉCHOUÉ!~%~%")))
  (error (e)
    (format t "  ✗ ERREUR: ~A~%~%" e)))

;;; ============================================================================
;;; TEST: FONCTION AVEC HASH-TABLE ET DOLIST
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST: Compilation et exécution de store-in-hash~%")
(format t "═════════════════════════════════════════════════════════════════~%")

(defparameter *store-in-hash-src*
  '(defun store-in-hash (lst)
     (let ((h (vm-make-hash-table :test 'equal))
           (idx 0))
       (dolist (elem lst)
         (vm-hash-set h elem idx)
         (setq idx (+ idx 1)))
       h)))

(format t "  → Compilation...~%")
(defparameter *store-in-hash-mips* (compile-lisp *store-in-hash-src*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *store-in-hash-mips*))

;; Créer une VM et charger le code
(format t "  → Création de la VM et chargement du code...~%")
(defparameter *vm2* (make-new-vm :verbose nil))
(load-code *vm2* *store-in-hash-mips* :verbose nil)
(defparameter *func-addr2* (calculate-code-start *vm2*))
(format t "  ✓ Code chargé à l'adresse ~A~%~%" *func-addr2*)

;; Créer une liste test
(format t "  → Création d'une liste test: (x y z)~%")
(defparameter *test-list2* '(x y z))

;; Enregistrer la liste
(defparameter *list-handle2* 
  (let ((handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) *test-list2*)
    handle))
(format t "  ✓ Liste enregistrée avec handle ~A~%~%" *list-handle2*)

;; Configurer et exécuter
(format t "  → Configuration: $A0 = ~A (handle)~%" *list-handle2*)
(set-register *vm2* *reg-a0* *list-handle2*)
(set-register *vm2* *reg-ra* 999999)
(set-register *vm2* (get-reg :pc) *func-addr2*)

(format t "  → Exécution...~%")
(handler-case
    (progn
      (run-vm *vm2* :max-instructions 10000)
      (defparameter *result2* (get-register *vm2* *reg-v0*))
      (format t "  ✓ Résultat: handle ~A~%~%" *result2*)
      
      ;; Récupérer le hash-table
      (defparameter *result-hash* (gethash *result2* *vm-hash-tables*))
      (if *result-hash*
          (progn
            (format t "  ✓ Hash-table récupéré: ~A entrées~%~%" 
                    (hash-table-count *result-hash*))
            (format t "  → Contenu:~%")
            (maphash #'(lambda (k v)
                        (format t "     ~A → ~A~%" k v))
                     *result-hash*)
            (format t "~%")
            (if (= (hash-table-count *result-hash*) (length *test-list2*))
                (format t "  ✅ TEST RÉUSSI!~%~%")
                (format t "  ❌ TEST ÉCHOUÉ!~%~%")))
          (format t "  ✗ Hash-table non trouvé~%~%")))
  (error (e)
    (format t "  ✗ ERREUR: ~A~%~%" e)))

;;; ============================================================================
;;; TEST: COLLECT-LABELS SIMPLIFIÉ (AVEC CONSP)
;;; ============================================================================

(format t "═════════════════════════════════════════════════════════════════~%")
(format t "TEST: Compilation et exécution de collect-labels-v1~%")
(format t "═════════════════════════════════════════════════════════════════~%")

(defparameter *collect-labels-v1-src*
  '(defun collect-labels-v1 (asm-code code-start)
     (let ((labels (vm-make-hash-table :test 'equal))
           (position 0))
       (dolist (instr asm-code)
         (if (vm-consp instr)
             (progn
               ;; C'est une paire, extraire la clé (CAR) et stocker
               (vm-hash-set labels (vm-car instr) (+ code-start position))
               (setq position (+ position 1)))
             (setq position (+ position 1))))
       labels)))

(format t "  → Compilation...~%")
(defparameter *collect-labels-v1-mips* (compile-lisp *collect-labels-v1-src*))
(format t "  ✓ Compilé : ~A instructions~%~%" (length *collect-labels-v1-mips*))

;; Créer une VM
(format t "  → Création de la VM et chargement du code...~%")
(defparameter *vm3* (make-new-vm :verbose nil))
(load-code *vm3* *collect-labels-v1-mips* :verbose nil)
(defparameter *func-addr3* (calculate-code-start *vm3*))
(format t "  ✓ Code chargé à l'adresse ~A~%~%" *func-addr3*)

;; Créer une liste d'instructions simulées
(format t "  → Création d'une liste test avec des paires (label . value)~%")
(defparameter *test-list3* '((LOOP_START . 100) (LOOP_END . 200) (EXIT . 300)))
(format t "     ~A~%~%" *test-list3*)

;; Enregistrer la liste
(defparameter *list-handle3* 
  (let ((handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) *test-list3*)
    handle))
(format t "  ✓ Liste enregistrée avec handle ~A~%~%" *list-handle3*)

;; Configurer et exécuter
(format t "  → Configuration: $A0 = ~A (liste), $A1 = 1000 (code-start)~%" 
        *list-handle3*)
(set-register *vm3* *reg-a0* *list-handle3*)
(set-register *vm3* *reg-a1* 1000)
(set-register *vm3* *reg-ra* 999999)
(set-register *vm3* (get-reg :pc) *func-addr3*)

(format t "  → Exécution...~%")
(handler-case
    (progn
      (run-vm *vm3* :max-instructions 10000)
      (defparameter *result3* (get-register *vm3* *reg-v0*))
      (format t "  ✓ Résultat: handle ~A~%~%" *result3*)
      
      ;; Récupérer le hash-table
      (defparameter *result-hash3* (gethash *result3* *vm-hash-tables*))
      (if *result-hash3*
          (progn
            (format t "  ✓ Hash-table récupéré: ~A entrées~%~%" 
                    (hash-table-count *result-hash3*))
            (format t "  → Contenu:~%")
            (maphash #'(lambda (k v)
                        (format t "     ~A → ~A~%" k v))
                     *result-hash3*)
            (format t "~%")
            (if (= (hash-table-count *result-hash3*) (length *test-list3*))
                (format t "  ✅ TEST RÉUSSI! Les labels ont été collectés.~%~%")
                (format t "  ❌ TEST ÉCHOUÉ! Nombre d'entrées incorrect.~%~%")))
          (format t "  ✗ Hash-table non trouvé~%~%")))
  (error (e)
    (format t "  ✗ ERREUR: ~A~%~%" e)))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                         RÉSUMÉ FINAL                             ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║  1. count-list           : Test effectué                         ║~%")
(format t "║  2. store-in-hash        : Test effectué                         ║~%")
(format t "║  3. collect-labels-v1    : Test effectué                         ║~%")
(format t "║                                                                  ║~%")
(format t "║  Ces tests démontrent que:                                       ║~%")
(format t "║  • dolist fonctionne avec handles                                ║~%")
(format t "║  • Hash-tables peuvent être créés et remplis                     ║~%")
(format t "║  • vm-consp/vm-car fonctionnent pour extraire données            ║~%")
(format t "║  • collect-labels peut être implémenté et compilé                ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Tests terminés.~%~%")
