;;;; main.lisp
;;;; Point d'entrée principal du projet

(load "tests.lisp")

;;; ============================================================================
;;; FONCTION PRINCIPALE
;;; ============================================================================

(defun main ()
  "Point d'entrée principal"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "            PROJET: COMPILATEUR LISP -> ASM + MACHINE VIRTUELLE~%")
  (format t "================================================================================~%")
  (format t "~%")
  
  ;; Exécuter les tests
  (format t "Lancement des tests...~%")
  (run-all-vm-tests)
  
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                         PHASE 1 COMPLÉTÉE~%")
  (format t "================================================================================~%")
  (format t "~%Composants implémentés:~%")
  (format t "  ✓ Structure de base de la VM~%")
  (format t "  ✓ Gestion de la mémoire (registres, pile, tas)~%")
  (format t "  ✓ Jeu d'instructions de base (arithmétique, pile, sauts)~%")
  (format t "  ✓ Chargeur de code assembleur~%")
  (format t "  ✓ Résolution des labels~%")
  (format t "  ✓ Outils de visualisation et de débogage~%")
  (format t "  ✓ Suite de tests~%")
  (format t "~%")
  (format t "Prochaines étapes:~%")
  (format t "  - Implémenter le compilateur LISP -> ASM~%")
  (format t "  - Ajouter le support des fonctions et appels~%")
  (format t "  - Implémenter les structures de contrôle~%")
  (format t "  - Tester avec fibonacci(20)~%")
  (format t "~%"))

;;; ============================================================================
;;; DÉMARRAGE
;;; ============================================================================

;; Lancer le programme
(main)
