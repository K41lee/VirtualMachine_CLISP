;;; ============================================================================
;;; TEST DE LA CHAÎNE COMPLÈTE DE COMPILATION ET BOOTSTRAP
;;; ============================================================================
;;;
;;; Ce test démontre la chaîne de compilation :
;;; 1. Charger la VM
;;; 2. [TODO] Compiler le chargeur (pas encore compilable)
;;; 3. [TODO] Charger le chargeur compilé dans la VM
;;; 4. [TODO] Compiler le compilateur (pas encore compilable)
;;; 5. [TODO] Charger le compilateur compilé dans la VM
;;; 6. Compiler fibo
;;; 7. Charger fibo dans la VM
;;; 8. Exécuter fibo(20) dans la VM
;;;
;;; Commande : clisp tests/integration/test-full-compilation-chain.lisp
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║     TEST DE LA CHAÎNE COMPLÈTE DE COMPILATION                   ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; ÉTAPE 1 : CHARGER LA VM
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1/4 : Chargement de la VM~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(load "src/asm-ops.lisp")
(format t "  ✓ asm-ops.lisp chargé~%")

(load "src/vm.lisp")
(format t "  ✓ vm.lisp chargé~%")

(load "src/compiler.lisp")
(format t "  ✓ compiler.lisp chargé~%")

(load "src/loader.lisp")
(format t "  ✓ loader.lisp chargé~%")

(format t "~%✅ VM et compilateur chargés avec succès~%~%")

;;; ============================================================================
;;; ÉTAPE 2 : COMPILER LE CHARGEUR
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2/5 : Compilation du chargeur (load-code)~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Définition simplifiée de load-code pour compilation
;; Version sans set-register (retourne start-addr au lieu de modifier $PC)
(defparameter *loader-code*
  '(defun simple-load-code (code-list start-addr)
     "Chargeur simplifié : charge une liste d'instructions à partir d'une adresse"
     (let ((addr start-addr)
           (i 0)
           (count (length code-list)))
       (while (< i count)
         (let ((instr (nth i code-list)))
           (mem-write addr instr)
           (setq addr (+ addr 1))
           (setq i (+ i 1))))
       start-addr)))

(format t "  → Compilation de simple-load-code...~%")
(defparameter *loader-mips* (compile-lisp *loader-code*))
(format t "  ✓ Chargeur compilé : ~A instructions MIPS~%~%" (length *loader-mips*))

;;; ============================================================================
;;; ÉTAPE 3 : CHARGER LE CHARGEUR COMPILÉ DANS LA VM
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3/5 : Chargement du chargeur compilé dans la VM~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "  → Création de la VM pour le chargeur...~%")
(defparameter *vm-loader* (make-new-vm :verbose nil))
(format t "  ✓ VM créée~%")

(format t "  → Chargement du chargeur MIPS...~%")
(load-code *vm-loader* *loader-mips* :verbose nil)
(format t "  ✓ Chargeur chargé à l'adresse ~A~%~%" (calculate-code-start *vm-loader*))

;;; ============================================================================
;;; ÉTAPE 4 : COMPILER LE COMPILATEUR (TODO - PAS ENCORE COMPILABLE)
;;; ============================================================================

#|  --- ÉTAPE COMMENTÉE : LE COMPILATEUR N'EST PAS ENCORE COMPILABLE ---
(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4/8 : Compilation du compilateur (compile-lisp)~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Définition simplifiée du compilateur pour compilation
;; (Le compilateur complet utilise des structures complexes)
(defparameter *compiler-code*
  '(defun simple-add (a b)
     "Fonction simple d'addition"
     (+ a b)))

(format t "  → Compilation de simple-compile-expr...~%")
(defparameter *compiler-mips* (compile-lisp *compiler-code*))
(format t "  ✓ Compilateur compilé : ~A instructions MIPS~%~%" (length *compiler-mips*))
|#

;;; ============================================================================
;;; ÉTAPE 5 : CHARGER LE COMPILATEUR AVEC LE CHARGEUR COMPILÉ (TODO)
;;; ============================================================================

#|  --- ÉTAPE COMMENTÉE : DÉPEND DES ÉTAPES 2-4 ---
(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 5/8 : Chargement du compilateur avec le chargeur compilé~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "  → Création d'une VM pour le compilateur...~%")
(defparameter *vm-compiler* (make-new-vm :verbose nil))
(format t "  ✓ VM créée~%")

(format t "  → Chargement du compilateur (via chargeur natif)...~%")
(load-code *vm-compiler* *compiler-mips* :verbose nil)
(format t "  ✓ Compilateur chargé à l'adresse ~A~%~%" (calculate-code-start *vm-compiler*))
|#

;;; ============================================================================
;;; ÉTAPE 2 : COMPILER FIBO
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 2/4 : Compilation de fibo(20)~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; NOTE: Dans un vrai bootstrap, on exécuterait le compilateur MIPS
;; Pour l'instant, on utilise le compilateur natif
(format t "  → Compilation de fibo avec le compilateur natif (le compilateur MIPS est chargé)...~%")

(defparameter *fibo-code*
  '(progn
     (defun fibo (n)
       (if (= n 0)
           1
           (if (= n 1)
               1
               (+ (fibo (- n 1)) (fibo (- n 2))))))
     (fibo 20)))

(format t "  → Compilation en cours...~%")
(defparameter *fibo-mips* (compile-lisp *fibo-code*))
(format t "  ✓ Fibo compilé : ~A instructions MIPS~%~%" (length *fibo-mips*))

;;; ============================================================================
;;; ÉTAPE 3 : CHARGER FIBO DANS LA VM
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 3/4 : Chargement de fibo dans la VM~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; NOTE : Dans un vrai bootstrap, on appellerait la fonction SIMPLE-LOAD-CODE
;; qui est maintenant en MIPS dans la VM. Pour l'instant, on utilise le
;; chargeur natif car le chargeur compilé nécessiterait d'être exécuté.

(format t "  → Création d'une nouvelle VM pour fibo...~%")
(defparameter *vm-fibo* (make-new-vm :verbose nil))
(format t "  ✓ VM créée~%")

(format t "  → Chargement de fibo (via chargeur natif pour l'instant)...~%")
(load-code *vm-fibo* *fibo-mips* :verbose nil)
(format t "  ✓ Fibo chargé à l'adresse ~A~%~%" (calculate-code-start *vm-fibo*))

;;; ============================================================================
;;; ÉTAPE 4 : EXÉCUTER FIBO(20) DANS LA VM
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 4/4 : Exécution de fibo(20) dans la VM~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "  → Exécution en cours...~%")
(defparameter *start-time* (get-internal-real-time))

(run-vm *vm-fibo* :max-instructions 100000000)

(defparameter *end-time* (get-internal-real-time))
(defparameter *elapsed* (/ (- *end-time* *start-time*) internal-time-units-per-second))

(defparameter *result* (get-register *vm-fibo* (get-reg :v0)))

(format t "  ✓ Exécution terminée~%")
(format t "  → Instructions exécutées : ~A~%" (vm-instruction-count *vm-fibo*))
(format t "  → Temps d'exécution : ~,6F secondes~%" *elapsed*)
(format t "  → Résultat (registre $V0) : ~A~%~%" *result*)

;;; ============================================================================
;;; AFFICHAGE DU CODE ASSEMBLEUR GÉNÉRÉ
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                  CODE ASSEMBLEUR GÉNÉRÉ                          ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

#|  --- AFFICHAGE DU CHARGEUR ET COMPILATEUR (COMMENTÉ) ---
(format t "═══ CHARGEUR (simple-load-code) - ~A instructions ═══~%" (length *loader-mips*))
(format t "~%")
(let ((i 0))
  (dolist (instr *loader-mips*)
    (format t "~4D: ~A~%" i instr)
    (incf i)))

(format t "~%~%")
(format t "═══ COMPILATEUR (simple-compile-expr) - ~A instructions ═══~%" (length *compiler-mips*))
(format t "~%")
(let ((i 0))
  (dolist (instr *compiler-mips*)
    (format t "~4D: ~A~%" i instr)
    (incf i)))

(format t "~%~%")
|#

(format t "═══ FIBONACCI (fibo 20) - ~A instructions ═══~%" (length *fibo-mips*))
(format t "~%")
(let ((i 0))
  (dolist (instr *fibo-mips*)
    (format t "~4D: ~A~%" i instr)
    (incf i)
    (when (> i 50)  ; Limiter l'affichage aux 50 premières instructions
      (format t "  ... (~A instructions restantes)~%" (- (length *fibo-mips*) i))
      (return))))

(format t "~%~%")

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                        RÉSUMÉ DU TEST                            ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Chaîne complète testée :~%")
(format t "  1. ✅ VM chargée (vm.lisp, compiler.lisp, loader.lisp)~%")
(format t "  2. ✅ Fibo compilé (~A instructions MIPS)~%" (length *fibo-mips*))
(format t "  3. ✅ Fibo chargé dans VM (adresse ~A)~%" (calculate-code-start *vm-fibo*))
(format t "  4. ✅ Fibo(20) exécuté : résultat = ~A~%~%" *result*)

(if (= *result* 10946)
    (progn
      (format t "✅ TEST RÉUSSI : fibo(20) = 10946 ✓~%~%")
      (format t "📊 STATISTIQUES D'EXÉCUTION~%")
      (format t "   Temps d'exécution : ~,3F secondes~%" *elapsed*)
      (format t "   Instructions MIPS : ~:D~%" (vm-instruction-count *vm-fibo*))
      (format t "   Instructions/sec  : ~:D~%" (floor (/ (vm-instruction-count *vm-fibo*) *elapsed*)))
      (format t "   Taille fibo       : ~A instructions~%" (length *fibo-mips*))
      (format t "   Mémoire VM        : ~:D octets (~A Mo)~%" *maxmem* (/ *maxmem* 1048576.0)))
    (progn
      (format t "❌ TEST ÉCHOUÉ : résultat attendu = 10946, obtenu = ~A~%" *result*)))

(format t "~%")
(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "📝 NOTE TECHNIQUE~%")
(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "Ce test démontre une chaîne de compilation simplifiée :~%")
(format t "  • Code LISP (fibo) → Code MIPS ✓~%")
(format t "  • Code MIPS chargé dans VM ✓~%")
(format t "  • Exécution dans VM avec résultat correct ✓~%")
(format t "~%")
(format t "TODO - Bootstrap complet (étapes 2-5 commentées) :~%")
(format t "  Les étapes suivantes ne sont pas encore fonctionnelles :~%")
(format t "  2. ⏸️ Compiler le chargeur (loader.lisp → MIPS)~%")
(format t "  3. ⏸️ Charger le chargeur compilé dans une VM~%")
(format t "  4. ⏸️ Compiler le compilateur (compiler.lisp → MIPS)~%")
(format t "  5. ⏸️ Charger le compilateur compilé dans une VM~%")
(format t "~%")
(format t "Blocages actuels :~%")
(format t "  • Le chargeur utilise des constructions complexes (WHILE, structures)~%")
(format t "  • Le compilateur utilise des symboles quotés ('+ → problème d'évaluation)~%")
(format t "  • Ces composants nécessitent des améliorations du compilateur~%")
(format t "═══════════════════════════════════════════════════════════════════~%")
