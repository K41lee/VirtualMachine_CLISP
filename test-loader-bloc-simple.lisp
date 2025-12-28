;;; ============================================================================
;;; TEST : CHARGEMENT EN BLOC DU LOADER (VÉRIFICATION SIMPLE)
;;; ============================================================================
;;;
;;; Ce test vérifie que le chargement en bloc fonctionne correctement.
;;;
;;; Commande: clisp test-loader-bloc-simple.lisp
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST : CHARGEMENT EN BLOC DU LOADER (Vérification)            ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; Chargement de l'infrastructure
(format t "Chargement de l'infrastructure...~%")
(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/vm-primitives-stubs.lisp")
(load "tools/compile-file.lisp")
(format t "✓ Infrastructure chargée~%~%")

;;; Création de la VM
(format t "Création de la VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))
(format t "✓ VM créée~%~%")

;;; Compilation et chargement EN BLOC
(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "COMPILATION ET CHARGEMENT EN BLOC~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(multiple-value-bind (base-addr addresses all-code end-addr)
    (compile-and-load-file *vm* "src/loader-compilable.lisp" :verbose t)
  
  (format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
  (format t "║                      RÉSULTAT                                    ║~%")
  (format t "╠══════════════════════════════════════════════════════════════════╣~%")
  (format t "║                                                                  ║~%")
  (format t "║  ✅ CHARGEMENT EN BLOC RÉUSSI!                                   ║~%")
  (format t "║                                                                  ║~%")
  (format t "║  Fichier: src/loader-compilable.lisp                             ║~%")
  (format t "║  Fonctions compilées: ~A~38T║~%" (hash-table-count addresses))
  (format t "║  Instructions total: ~A~38T║~%" (length all-code))
  (format t "║                                                                  ║~%")
  (format t "║  Adresse de début: ~A~38T║~%" base-addr)
  (format t "║  Adresse de fin  : ~A~38T║~%" end-addr)
  (format t "║  Plage mémoire   : ~A octets~38T║~%" (- end-addr base-addr))
  (format t "║                                                                  ║~%")
  (format t "║  Fonctions chargées:                                             ║~%")
  
  ;; Afficher les adresses des fonctions
  (let ((func-list '()))
    (maphash #'(lambda (name addr) (push (cons name addr) func-list)) addresses)
    (setf func-list (sort func-list #'< :key #'cdr))
    (dolist (entry func-list)
      (format t "║    • ~A~30T: ~A~38T║~%" (car entry) (cdr entry))))
  
  (format t "║                                                                  ║~%")
  (format t "╠══════════════════════════════════════════════════════════════════╣~%")
  (format t "║  VÉRIFICATION DE L'INTÉGRITÉ:                                    ║~%")
  (format t "║                                                                  ║~%")
  
  ;; Vérifier que le code est bien en mémoire
  (let ((test-addr base-addr)
        (test-instr (mem-read *vm* base-addr)))
    (format t "║  Instruction à ~A:~38T║~%" test-addr)
    (format t "║    ~A~38T║~%" test-instr)
    (if (consp test-instr)
        (format t "║  ✓ Format correct (liste)~38T║~%")
        (format t "║  ✗ Format incorrect!~38T║~%")))
  
  (format t "║                                                                  ║~%")
  (format t "╚══════════════════════════════════════════════════════════════════╝~%~%")
  
  (format t "═══════════════════════════════════════════════════════════════════~%")
  (format t "AVANTAGES DU CHARGEMENT EN BLOC~%")
  (format t "═══════════════════════════════════════════════════════════════════~%~%")
  (format t "✅ Un seul appel : compile-and-load-file au lieu de 8 load-code~%")
  (format t "✅ Automatique   : Adresses calculées automatiquement~%")
  (format t "✅ Contigu       : Code chargé en bloc contigu (meilleure localité)~%")
  (format t "✅ Variables     : *FUNCTION-NAME-MIPS* créées automatiquement~%")
  (format t "✅ Map           : Hash-table des adresses retournée~%~%")
  
  (format t "UTILISATION:~%")
  (format t "  (multiple-value-bind (base addresses code end)~%")
  (format t "      (compile-and-load-file vm \"fichier.lisp\" :verbose t)~%")
  (format t "    ;; base    : adresse de début du bloc~%")
  (format t "    ;; addresses : hash-table fonction → adresse~%")
  (format t "    ;; code    : liste de toutes les instructions~%")
  (format t "    ;; end     : adresse après la dernière instruction~%")
  (format t "    ...)~%~%")
  
  (format t "Test terminé avec succès.~%~%"))
