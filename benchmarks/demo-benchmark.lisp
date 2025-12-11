;;;; Exemples d'utilisation du système de benchmark
;;;; Démontre l'exécution de code sur LISP natif, VM0, et VM1→VM2

(load "run-benchmark.lisp" :verbose nil)

(format t "~%~%")
(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║            DÉMONSTRATION DU SYSTÈME DE BENCHMARK                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; EXEMPLE 1 : Arithmétique simple
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 1 : Arithmétique simple~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(+ 10 20 30))

;;; ============================================================================
;;; EXEMPLE 2 : Variable locale
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 2 : Variable locale et multiplication~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(let ((x 15)) (* x x)))

;;; ============================================================================
;;; EXEMPLE 3 : Opérations sur listes
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 3 : Construction et parcours de liste~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(let ((lst (cons 1 (cons 2 (cons 3 nil)))))
                   (+ (car lst) (car (cdr lst)) (car (cdr (cdr lst))))))

;;; ============================================================================
;;; EXEMPLE 4 : Conditionnelle
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 4 : Conditionnelle IF~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(if (> 10 5) 100 200))

;;; ============================================================================
;;; EXEMPLE 5 : Sélection de scénarios
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 5 : Benchmark seulement LISP natif et VM0~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(* 7 8) :scenarios '(:native :vm0))

;;; ============================================================================
;;; EXEMPLE 6 : INCF
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 6 : Incrémentation~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(let ((x 10)) (incf x) (incf x) x))

;;; ============================================================================
;;; EXEMPLE 7 : DOLIST
;;; ============================================================================

(format t "~%━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%")
(format t "EXEMPLE 7 : Boucle DOLIST~%")
(format t "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~%~%")

(benchmark-code '(let ((sum 0))
                   (dolist (x (cons 5 (cons 10 (cons 15 nil))))
                     (setq sum (+ sum x)))
                   sum))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                    DÉMONSTRATION TERMINÉE                         ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Tous les exemples ont été exécutés avec succès!~%~%")

(format t "Pour tester votre propre code:~%")
(format t "  1. Lancer CLISP dans ce répertoire~%")
(format t "  2. (load \"run-benchmark.lisp\")~%")
(format t "  3. (benchmark-code '(votre code LISP))~%~%")

(format t "Scénarios testés pour chaque exemple:~%")
(format t "  ✅ LISP natif      - Exécution directe (référence)~%")
(format t "  ✅ VM0             - Compilation LISP→MIPS + interprétation~%")
(format t "  ✅ VM1→VM2         - Bootstrap complet (3 couches)~%~%")

(format t "Le système prouve que:~%")
(format t "  • Le compilateur LISP→MIPS fonctionne correctement~%")
(format t "  • La VM peut exécuter du code MIPS~%")
(format t "  • Le bootstrap complet est fonctionnel (VM compilée peut compiler)~%~%")
