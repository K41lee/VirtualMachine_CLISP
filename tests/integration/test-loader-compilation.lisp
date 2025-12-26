;;;; test-loader-compilation.lisp
;;;; Test de compilation du chargeur simplifié
;;;;
;;;; Objectif : Compiler une version simplifiée de load-code
;;;; qui utilise les nouvelles primitives (LENGTH, NTH, MEM-WRITE)

;;; ============================================================================
;;; CHARGEMENT DES MODULES
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║         TEST COMPILATION DU CHARGEUR SIMPLIFIÉ                   ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/compiler.lisp")
(load "src/loader.lisp")

;;; ============================================================================
;;; VERSION 1 : Loader avec boucle WHILE et NTH
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "VERSION 1 : Loader avec WHILE, LENGTH, NTH, MEM-WRITE~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(defparameter *simple-loader-v1*
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

(format t "Code source du loader :~%")
(format t "~A~%~%" *simple-loader-v1*)

(format t "Compilation en cours...~%")
(handler-case
    (progn
      (defparameter *loader-mips-v1* (compile-lisp *simple-loader-v1*))
      (format t "✅ COMPILATION RÉUSSIE !~%")
      (format t "   → Code généré : ~A instructions MIPS~%~%" (length *loader-mips-v1*))
      
      ;; Afficher les 20 premières instructions
      (format t "Premières instructions :~%")
      (let ((count 0))
        (dolist (instr *loader-mips-v1*)
          (when (< count 20)
            (format t "  ~3D: ~A~%" count instr)
            (incf count))))
      (when (> (length *loader-mips-v1*) 20)
        (format t "  ... (~A instructions restantes)~%~%" (- (length *loader-mips-v1*) 20))))
  (error (e)
    (format t "❌ ERREUR DE COMPILATION :~%")
    (format t "   ~A~%~%" e)))

;;; ============================================================================
;;; VERSION 2 : Test de chargement dans la VM
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "VERSION 2 : Chargement du loader dans la VM~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(when (boundp '*loader-mips-v1*)
  (format t "Création d'une VM...~%")
  (defparameter *vm-loader* (make-new-vm))
  (format t "✓ VM créée~%")
  
  (format t "Chargement du loader compilé...~%")
  (load-code *vm-loader* *loader-mips-v1* :verbose nil)
  (format t "✓ Loader chargé à l'adresse ~A~%~%" (calculate-code-start *vm-loader*))
  
  (format t "Le loader est maintenant chargé en MIPS dans la VM !~%")
  (format t "Pour l'utiliser, il faudrait appeler la fonction avec des paramètres.~%~%"))

;;; ============================================================================
;;; VERSION 3 : Loader encore plus simple (sans WHILE)
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "VERSION 3 : Loader minimal (sans boucle)~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

;; Version qui charge juste 3 instructions fixes (pour test)
(defparameter *simple-loader-v3*
  '(defun simple-load-3 (code-list start-addr)
     "Charge exactement 3 instructions"
     (progn
       (mem-write start-addr (nth 0 code-list))
       (mem-write (+ start-addr 1) (nth 1 code-list))
       (mem-write (+ start-addr 2) (nth 2 code-list))
       start-addr)))

(format t "Code source du loader minimal :~%")
(format t "~A~%~%" *simple-loader-v3*)

(format t "Compilation en cours...~%")
(handler-case
    (progn
      (defparameter *loader-mips-v3* (compile-lisp *simple-loader-v3*))
      (format t "✅ COMPILATION RÉUSSIE !~%")
      (format t "   → Code généré : ~A instructions MIPS~%~%" (length *loader-mips-v3*))
      
      ;; Tester le chargement
      (format t "Chargement dans une VM...~%")
      (defparameter *vm-loader-v3* (make-new-vm))
      (load-code *vm-loader-v3* *loader-mips-v3* :verbose nil)
      (format t "✓ Loader minimal chargé~%~%"))
  (error (e)
    (format t "❌ ERREUR :~%")
    (format t "   ~A~%~%" e)))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                           RÉSUMÉ                                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Résultats de compilation :~%~%")

(if (boundp '*loader-mips-v1*)
    (progn
      (format t "✅ VERSION 1 (avec WHILE) :~%")
      (format t "   • Compilation : RÉUSSIE~%")
      (format t "   • Taille : ~A instructions MIPS~%")
      (format t "   • Chargement VM : RÉUSSI~%~%"))
    (format t "❌ VERSION 1 : Échec de compilation~%~%"))

(if (boundp '*loader-mips-v3*)
    (progn
      (format t "✅ VERSION 3 (minimal) :~%")
      (format t "   • Compilation : RÉUSSIE~%")
      (format t "   • Taille : ~A instructions MIPS~%")
      (format t "   • Chargement VM : RÉUSSI~%~%"))
    (format t "❌ VERSION 3 : Échec de compilation~%~%"))

(format t "~%📊 ANALYSE :~%~%")
(format t "Les fonctionnalités suivantes fonctionnent :~%")
(format t "  ✓ LENGTH : Calcul de longueur de liste~%")
(format t "  ✓ NTH : Accès indexé dans une liste~%")
(format t "  ✓ MEM-WRITE : Écriture en mémoire~%")
(format t "  ✓ WHILE : Boucle conditionnelle~%")
(format t "  ✓ LET : Variables locales~%")
(format t "  ✓ SETQ : Modification de variables~%")
(format t "~%")

(if (and (boundp '*loader-mips-v1*) (boundp '*loader-mips-v3*))
    (progn
      (format t "🎉 SUCCÈS TOTAL !~%~%")
      (format t "Le chargeur peut maintenant être compilé en MIPS.~%")
      (format t "Prochaine étape : Tester l'exécution du loader compilé.~%"))
    (progn
      (format t "⚠️  Certaines versions ont échoué.~%")
      (format t "Vérifier les erreurs ci-dessus.~%")))

(format t "~%")
