;;;; ============================================================================
;;;; MAIN.LISP - Point d'entrée principal du compilateur LISP → MIPS
;;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════╗~%")
(format t "║   Compilateur LISP → MIPS (VirtualMachine_CLISP)  ║~%")
(format t "╚════════════════════════════════════════════════════╝~%~%")

;; Charger les composants dans le bon ordre
(format t "Chargement des composants...~%")

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%✓ Compilateur chargé avec succès!~%~%")
(format t "Fonctions disponibles:~%")
(format t "  - (compile-lisp expr)~%")
(format t "  - (compile-and-run expr)~%")
(format t "~%")
(format t "Pour les tests: (load \"tests/unit/test-xxx.lisp\")~%")
(format t "~%")
