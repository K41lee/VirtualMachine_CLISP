;;; ============================================================================
;;; EXEMPLE : COMPILATION D'UN FICHIER LISP COMPLET
;;; ============================================================================
;;;
;;; Cet exemple montre comment compiler un fichier Lisp complet en MIPS
;;; en utilisant l'utilitaire compile-file.lisp
;;;
;;; Commande: clisp exemple-compile-file.lisp
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║        EXEMPLE : COMPILATION DE FICHIER COMPLET                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; ÉTAPE 1 : CHARGER L'INFRASTRUCTURE
;;; ============================================================================

(format t "Étape 1: Chargement de l'infrastructure...~%")
(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(format t "  ✓ Infrastructure chargée~%~%")

;;; ============================================================================
;;; ÉTAPE 2 : CHARGER LES STUBS ET L'UTILITAIRE
;;; ============================================================================

(format t "Étape 2: Chargement des outils de compilation de fichiers...~%")
(load "src/vm-primitives-stubs.lisp")
(format t "  ✓ Stubs de primitives VM chargés~%")
(load "tools/compile-file.lisp")
(format t "  ✓ Utilitaire de compilation chargé~%~%")

;;; ============================================================================
;;; ÉTAPE 3 : COMPILER LE FICHIER
;;; ============================================================================

(format t "Étape 3: Compilation de src/loader-compilable.lisp...~%~%")

;; Méthode 1: Compilation et sauvegarde automatique dans des variables
(defparameter *compiled* 
  (compile-and-save-file "src/loader-compilable.lisp" :verbose t))

;; Les variables suivantes sont maintenant disponibles:
;; *COLLECT-LABELS-MIPS*
;; *RESOLVE-LABELS-MIPS*
;; *RESOLVE-INSTRUCTION-MIPS*
;; *REVERSE-LIST-MIPS*
;; *NORMALIZE-INSTRUCTION-MIPS*
;; *NORMALIZE-CODE-MIPS*

;;; ============================================================================
;;; ÉTAPE 4 : UTILISER LES FONCTIONS COMPILÉES
;;; ============================================================================

(format t "~%Étape 4: Utilisation du code compilé...~%~%")

(format t "  Fonctions disponibles:~%")
(format t "    • collect-labels      : ~A instructions (~A)~%" 
        (length *COLLECT-LABELS-MIPS*)
        (if *COLLECT-LABELS-MIPS* "✓" "✗"))
(format t "    • resolve-labels      : ~A instructions (~A)~%" 
        (length *RESOLVE-LABELS-MIPS*)
        (if *RESOLVE-LABELS-MIPS* "✓" "✗"))
(format t "    • resolve-instruction : ~A instructions (~A)~%" 
        (length *RESOLVE-INSTRUCTION-MIPS*)
        (if *RESOLVE-INSTRUCTION-MIPS* "✓" "✗"))
(format t "    • reverse-list        : ~A instructions (~A)~%" 
        (length *REVERSE-LIST-MIPS*)
        (if *REVERSE-LIST-MIPS* "✓" "✗"))

;;; ============================================================================
;;; ÉTAPE 5 : EXEMPLE D'UTILISATION - CHARGER DANS UNE VM
;;; ============================================================================

(format t "~%Étape 5: Test - Charger collect-labels dans une VM...~%")

(defparameter *vm* (make-new-vm :verbose nil))
(load-code *vm* *COLLECT-LABELS-MIPS* :verbose nil)
(defparameter *func-addr* (calculate-code-start *vm*))

(format t "  ✓ collect-labels chargé à l'adresse ~A~%" *func-addr*)
(format t "  ✓ Prêt à être exécuté!~%")

;;; ============================================================================
;;; STATISTIQUES FINALES
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                    STATISTIQUES FINALES                          ║~%")
(format t "╠══════════════════════════════════════════════════════════════════╣~%")
(format t "║                                                                  ║~%")
(format t "║  Fichier compilé : src/loader-compilable.lisp                    ║~%")
(format t "║  Fonctions       : ~A réussies~38T║~%" (hash-table-count *compiled*))

(let ((total 0))
  (maphash #'(lambda (name code) 
               (declare (ignore name))
               (incf total (length code)))
           *compiled*)
  (format t "║  Instructions    : ~A total~38T║~%" total))

(format t "║                                                                  ║~%")
(format t "║  Variables créées automatiquement:                               ║~%")
(maphash #'(lambda (name code)
             (let ((var-name (format nil "*~A-MIPS*" (string-upcase (symbol-name name)))))
               (format t "║    • ~A~38T║~%" var-name)))
         *compiled*)
(format t "║                                                                  ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "✅ Exemple terminé avec succès!~%~%")

(format t "USAGE ULTÉRIEUR:~%")
(format t "  Pour compiler un autre fichier:~%")
(format t "    (compile-and-save-file \"mon-fichier.lisp\")~%~%")
(format t "  Pour récupérer une fonction compilée:~%")
(format t "    (get-compiled-function *compiled* 'nom-fonction)~%~%")
(format t "  Pour charger dans une VM:~%")
(format t "    (load-code vm *NOM-FONCTION-MIPS*)~%~%")
