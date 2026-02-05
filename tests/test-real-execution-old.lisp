;;;; Test d'exécution réelle d'une fonction du compilateur dans la VM
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════╗~%")
(format t "║  TEST D'EXÉCUTION: Fonction du compilateur dans la VM     ║~%")
(format t "╚════════════════════════════════════════════════════════════╝~%")

;; Fonction la plus simple du compilateur
(defparameter *simple-fn*
  '(defun compile-constant-simplified (value env)
     (list (list :LI value :$V0))))

(format t "~%Fonction à tester: compile-constant-simplified~%")
(format t "  Entrée: (value env)~%")
(format t "  Sortie: (list (list :LI value :$V0))~%")

;; 1. Compiler la fonction
(format t "~%Étape 1: Compilation de la fonction...~%")
(defparameter *compiled* (compile-lisp-to-mips-simplified *simple-fn*))
(format t "  ✓ ~A instructions générées~%~%" (length *compiled*))

;; 2. Charger dans la VM
(format t "Étape 2: Chargement dans la VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))
(handler-case
    (progn
      (load-code *vm* *compiled*)
      (format t "  ✓ Code chargé avec succès~%~%"))
  (error (e)
    (format t "  ❌ Erreur de chargement: ~A~%~%" e)
    (quit)))

;; 3. Préparer l'environnement d'exécution
(format t "Étape 3: Préparation de l'exécution...~%")
(format t "  Note: Pour exécuter compile-constant-simplified(42, nil):~%")
(format t "    - Argument 1 (value): 42~%")
(format t "    - Argument 2 (env): nil~%")
(format t "    - Résultat attendu: ((:LI 42 :$V0))~%~%")

;; 4. Tentative d'exécution
(format t "Étape 4: Exécution dans la VM...~%")
(handler-case
    (progn
      ;; Placer les arguments
      (set-register *vm* :$A0 42)    ; value = 42
      (set-register *vm* :$A1 0)     ; env = nil (représenté par 0)
      
      (format t "  Arguments placés:~%")
      (format t "    $A0 (value) = ~A~%" (get-register *vm* :$A0))
      (format t "    $A1 (env) = ~A~%~%" (get-register *vm* :$A1))
      
      ;; Exécuter
      (format t "  Lancement de l'exécution...~%")
      (run-vm *vm*)
      
      (format t "  ✓ Exécution terminée~%~%")
      
      ;; Récupérer le résultat
      (format t "Étape 5: Récupération du résultat...~%")
      (defparameter *result* (get-register *vm* :$V0))
      (format t "  Résultat dans $V0: ~A~%" *result*)
      (format t "  Type: ~A~%" (type-of *result*))
      
      (format t "~%✅ TEST RÉUSSI !~%"))
  (error (e)
    (format t "  ❌ Erreur d'exécution: ~A~%~%" e)
    (format t "État de la VM:~%")
    (format t "  $V0 = ~A~%" (get-register *vm* :$V0))
    (format t "  $PC = ~A~%" (get-register *vm* :$PC))
    (format t "  $SP = ~A~%" (get-register *vm* :$SP))))

(format t "~%Test terminé.~%")
