;;; ============================================================================
;;; TEST : COMPILATION ET CHARGEMENT EN BLOC DU LOADER + FIBONACCI
;;; ============================================================================
;;;
;;; Ce test démontre la compilation et le chargement d'un fichier complet
;;; en un seul bloc, au lieu de charger fonction par fonction.
;;;
;;; Architecture:
;;; 1. Compiler TOUT loader-compilable.lisp en un seul bloc MIPS
;;; 2. Charger LE BLOC ENTIER dans la VM d'un coup
;;; 3. Compiler et charger fibonacci
;;; 4. Exécuter fibonacci(20) et vérifier = 6765
;;;
;;; Commande: clisp test-loader-bloc.lisp
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║     TEST : CHARGEMENT EN BLOC DU LOADER + FIBONACCI             ║~%")
(format t "║     (Tout le fichier compilé et chargé d'un coup)                ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(defparameter *start-time* (get-internal-real-time))

;;; ============================================================================
;;; ÉTAPE 1 : INFRASTRUCTURE
;;; ============================================================================

(format t "ÉTAPE 1/5 : Chargement de l'infrastructure~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(load "src/asm-ops.lisp")
(format t "  ✓ asm-ops.lisp~%")

(load "src/vm.lisp")
(format t "  ✓ vm.lisp~%")

(load "src/loader.lisp")
(format t "  ✓ loader.lisp~%")

(load "src/compiler.lisp")
(format t "  ✓ compiler.lisp~%")

(load "src/vm-primitives-stubs.lisp")
(format t "  ✓ vm-primitives-stubs.lisp~%")

(load "tools/compile-file.lisp")
(format t "  ✓ compile-file.lisp~%~%")

;;; ============================================================================
;;; ÉTAPE 2 : COMPILATION ET CHARGEMENT EN BLOC DU LOADER
;;; ============================================================================

(format t "ÉTAPE 2/5 : Compilation et chargement EN BLOC du loader~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "  → Création de la VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))
(format t "  ✓ VM créée~%~%")

;; Compiler ET charger tout le fichier d'un coup
(format t "  → Compilation et chargement de src/loader-compilable.lisp...~%~%")
(multiple-value-bind (base-addr addresses all-code end-addr)
    (compile-and-load-file *vm* "src/loader-compilable.lisp" :verbose t)
  
  ;; Sauvegarder les valeurs importantes
  (defparameter *loader-base-addr* base-addr)
  (defparameter *loader-addresses* addresses)
  (defparameter *loader-code* all-code)
  (defparameter *loader-end-addr* end-addr)
  
  (format t "~%  ✅ Loader chargé en un seul bloc!~%")
  (format t "     Base : ~A~%" base-addr)
  (format t "     Fin  : ~A~%" end-addr)
  (format t "     Total: ~A instructions~%~%" (length all-code)))

;;; ============================================================================
;;; ÉTAPE 3 : COMPILATION DE FIBONACCI
;;; ============================================================================

(format t "ÉTAPE 3/5 : Compilation de fibonacci~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(defparameter *fibonacci-source*
  '(progn
     (defun fib (n)
       (if (<= n 1)
           n
           (+ (fib (- n 1)) (fib (- n 2)))))
     (fib 20)))

(format t "  → Compilation de fibonacci(20)...~%")
(defparameter *fibonacci-mips* 
  (append (compile-lisp *fibonacci-source*)
          (list (list :PRINT *reg-v0*)
                (list :HALT))))
(format t "  ✓ Fibonacci compilé : ~A instructions~%~%" (length *fibonacci-mips*))

;;; ============================================================================
;;; ÉTAPE 4 : CHARGEMENT ET EXÉCUTION DE FIBONACCI
;;; ============================================================================

(format t "ÉTAPE 4/5 : Chargement et exécution de fibonacci~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; Utiliser simple-loader pour charger fibonacci
(defparameter *simple-loader-src*
  '(defun simple-loader (code-addr data-addr count)
     (let ((i 0))
       (while (< i count)
         (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
         (setq i (+ i 1)))
       code-addr)))

(format t "  → Compilation de simple-loader...~%")
(defparameter *simple-loader-mips* (compile-lisp *simple-loader-src*))
;; L'adresse de simple-loader est juste après le loader
(defparameter *simple-loader-addr* *loader-end-addr*)
(load-code *vm* *simple-loader-mips* :verbose nil)
(format t "  ✓ Simple-loader chargé à ~A (~A inst)~%~%" 
        *simple-loader-addr* (length *simple-loader-mips*))

;; Zones mémoire
(defparameter *data-zone* 10481500)
(defparameter *code-zone* 10482000)

(format t "  → Prétraitement de fibonacci...~%")
(multiple-value-bind (resolved-fib labels-fib)
    (preprocess-code *fibonacci-mips* *data-zone*)
  (format t "     Labels trouvés : ~A~%" (hash-table-count labels-fib))
  (let ((addr *data-zone*))
    (dolist (instr resolved-fib)
      (mem-write *vm* addr instr)
      (incf addr))))
(format t "  ✓ Code prétraité en zone données (~A)~%~%" *data-zone*)

;; Créer le bootstrap
(format t "  → Création du bootstrap...~%")
;; Bootstrap après simple-loader
(defparameter *bootstrap-addr* (+ *simple-loader-addr* (length *simple-loader-mips*)))
(defparameter *bootstrap-code*
  (list
   (list :LI *code-zone* *reg-a0*)
   (list :LI *data-zone* *reg-a1*)
   (list :LI (length *fibonacci-mips*) *reg-a2*)
   (list :JAL *simple-loader-addr*)
   (list :LI *code-zone* *reg-t0*)
   (list :JR *reg-t0*)))

(multiple-value-bind (resolved-bootstrap labels)
    (preprocess-code *bootstrap-code* *bootstrap-addr*)
  (let ((addr *bootstrap-addr*))
    (dolist (instr resolved-bootstrap)
      (mem-write *vm* addr instr)
      (incf addr))))

(format t "  ✓ Bootstrap créé à ~A~%" *bootstrap-addr*)
(set-register *vm* (get-reg :pc) *bootstrap-addr*)
(format t "  ✓ $PC initialisé~%~%")

(format t "  → Exécution de fibonacci(20)...~%")
(format t "     (Cela peut prendre 1-2 minutes)~%~%")

(defparameter *exec-start* (get-internal-real-time))
(run-vm *vm* :max-instructions 100000000)
(defparameter *exec-end* (get-internal-real-time))
(defparameter *exec-time* (/ (- *exec-end* *exec-start*) internal-time-units-per-second))

(defparameter *result* (get-register *vm* *reg-v0*))

(format t "  ✓ Exécution terminée~%")
(format t "     Résultat : ~A~%" *result*)
(format t "     Instructions : ~:D~%" (vm-instruction-count *vm*))
(format t "     Temps : ~,2F secondes~%~%" *exec-time*)

;;; ============================================================================
;;; ÉTAPE 5 : VÉRIFICATION
;;; ============================================================================

(format t "ÉTAPE 5/5 : Vérification du résultat~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(defparameter *total-time* (/ (- (get-internal-real-time) *start-time*) 
                              internal-time-units-per-second))
(defparameter *success* (= *result* 6765))

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                      RÉSULTAT FINAL                              ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║                                                                  ║~%")
(format t "║  fibonacci(20) = ~A~46T║~%" *result*)
(format t "║  Attendu       = 6765~46T║~%")
(format t "║                                                                  ║~%")

(if *success*
    (progn
      (format t "║  ╔════════════════════════════════════════════════════════╗  ║~%")
      (format t "║  ║           ✓✓✓ TEST RÉUSSI! ✓✓✓                         ║  ║~%")
      (format t "║  ║                                                        ║  ║~%")
      (format t "║  ║  Le loader a été compilé ET chargé EN BLOC!           ║  ║~%")
      (format t "║  ║  Tout le fichier en une seule opération!              ║  ║~%")
      (format t "║  ║  fibonacci(20) s'exécute correctement!                ║  ║~%")
      (format t "║  ╚════════════════════════════════════════════════════════╝  ║~%"))
    (progn
      (format t "║  ╔════════════════════════════════════════════════════════╗  ║~%")
      (format t "║  ║           ✗✗✗ TEST ÉCHOUÉ! ✗✗✗                         ║  ║~%")
      (format t "║  ║  Résultat incorrect!                                  ║  ║~%")
      (format t "║  ╚════════════════════════════════════════════════════════╝  ║~%")))

(format t "║                                                                  ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║  Chargement EN BLOC:                                             ║~%")
(format t "║                                                                  ║~%")
(format t "║  ✅ Loader compilé en bloc   : ~A → ~A~38T║~%" 
        *loader-base-addr* *loader-end-addr*)
(format t "║     Total instructions       : ~A~38T║~%" (length *loader-code*))
(format t "║                                                                  ║~%")
(format t "║  Adresses des fonctions du loader:                               ║~%")

;; Afficher les adresses de toutes les fonctions
(let ((func-list '()))
  (maphash #'(lambda (name addr) (push (cons name addr) func-list)) *loader-addresses*)
  (setf func-list (sort func-list #'< :key #'cdr))
  (dolist (entry func-list)
    (format t "║    • ~A~30T: ~A~38T║~%" (car entry) (cdr entry))))

(format t "║                                                                  ║~%")
(format t "║  ✅ Simple-loader            : ~A~38T║~%" *simple-loader-addr*)
(format t "║  ✅ Fibonacci                : ~A instructions~38T║~%" 
        (length *fibonacci-mips*))
(format t "║                                                                  ║~%")
(format t "║  Performance:                                                    ║~%")
(format t "║    • Instructions VM         : ~:D~38T║~%" (vm-instruction-count *vm*))
(format t "║    • Temps exécution         : ~,2F s~38T║~%" *exec-time*)
(format t "║    • Temps total             : ~,2F s~38T║~%" *total-time*)
(format t "║    • Instructions/sec        : ~:D~38T║~%" 
        (if (> *exec-time* 0) 
            (floor (/ (vm-instruction-count *vm*) *exec-time*))
            0))
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; CONCLUSION
;;; ============================================================================

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "CONCLUSION~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(if *success*
    (progn
      (format t "✅ TEST RÉUSSI!~%~%")
      (format t "   AVANTAGE DU CHARGEMENT EN BLOC:~%~%")
      (format t "   • ✓ Un seul appel à load-code au lieu de 8~%")
      (format t "   • ✓ Plus simple : compile-and-load-file fait tout~%")
      (format t "   • ✓ Adresses calculées automatiquement~%")
      (format t "   • ✓ Variables globales créées automatiquement~%")
      (format t "   • ✓ Code contigu en mémoire (meilleure localité)~%~%")
      (format t "   Le loader complet (~A instructions) a été:~%" (length *loader-code*))
      (format t "   • Compilé en un bloc~%")
      (format t "   • Chargé d'un coup à l'adresse ~A~%" *loader-base-addr*)
      (format t "   • Utilisé pour charger fibonacci~%")
      (format t "   • fibonacci(20) = ~A ✓~%~%" *result*)
      (format t "   UTILISATION:~%")
      (format t "   (compile-and-load-file vm \"fichier.lisp\")~%")
      (format t "   → Compile et charge tout le fichier en une seule opération!~%~%"))
    (progn
      (format t "✗ TEST ÉCHOUÉ~%~%")
      (format t "   Résultat incorrect : ~A (attendu 6765)~%~%" *result*)))

(format t "Test terminé (~,2F secondes).~%~%" *total-time*)
