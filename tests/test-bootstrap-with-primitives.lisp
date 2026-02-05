;;;; Bootstrap avec primitives Lisp chargées en premier
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════╗~%")
(format t "║  BOOTSTRAP AVEC PRIMITIVES LISP                            ║~%")
(format t "╚════════════════════════════════════════════════════════════╝~%")

;; Définir les primitives Lisp nécessaires
(defparameter *lisp-primitives*
  '((defun my-list (&rest items) items)
    
    (defun my-cons (a b) (cons a b))
    
    (defun my-car (lst) (first lst))
    
    (defun my-cdr (lst) (rest lst))
    
    (defun my-append (list1 list2)
      (if (null list1)
          list2
          (cons (first list1)
                (my-append (rest list1) list2))))))

(format t "~%Étape 1: Compilation des primitives Lisp...~%")
(defparameter *primitives-code* nil)
(dolist (prim *lisp-primitives*)
  (handler-case
      (let ((code (compile-lisp-to-mips-simplified prim)))
        (setf *primitives-code* (append *primitives-code* code))
        (format t "  ✓ ~A: ~A instructions~%" (second prim) (length code)))
    (error (e)
      (format t "  ✗ ~A: ~A~%" (second prim) e))))

(format t "~%  Total primitives: ~A instructions~%~%" (length *primitives-code*))

;; Compiler une fonction simple du compilateur
(defparameter *compiler-fn*
  '(defun compile-constant-simplified (value env)
     (my-list (my-list :LI value :$V0))))

(format t "Étape 2: Compilation de la fonction du compilateur...~%")
(defparameter *compiler-code* (compile-lisp-to-mips-simplified *compiler-fn*))
(format t "  ✓ compile-constant-simplified: ~A instructions~%~%" (length *compiler-code*))

;; Combiner tout le code
(defparameter *all-code* (append *primitives-code* *compiler-code*))
(format t "Étape 3: Code total: ~A instructions~%~%" (length *all-code*))

;; Charger dans la VM
(format t "Étape 4: Chargement dans la VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))
(handler-case
    (progn
      (load-code *vm* *all-code*)
      (format t "  ✅ Code chargé avec succès~%~%"))
  (error (e)
    (format t "  ❌ Erreur: ~A~%~%" e)
    (quit)))

;; Tester l'exécution
(format t "Étape 5: Test d'exécution...~%")
(format t "  Appel: compile-constant-simplified(42, nil)~%")
(format t "  Résultat attendu: ((:LI 42 :$V0))~%~%")

(handler-case
    (progn
      ;; Mettre les arguments
      (set-register *vm* :$A0 42)
      (set-register *vm* :$A1 0)
      
      ;; Chercher l'adresse de compile-constant-simplified
      ;; (elle est après les primitives)
      (let ((fn-addr (+ (calculate-code-start *vm*) (length *primitives-code*))))
        (format t "  Adresse de la fonction: ~A~%" fn-addr)
        (set-register *vm* :$PC fn-addr)
        
        ;; Exécuter
        (run-vm *vm*)
        
        (format t "  ✅ Exécution terminée~%")
        (format t "  Résultat $V0: ~A~%" (get-register *vm* :$V0))))
  (error (e)
    (format t "  ❌ Erreur: ~A~%~%" e)
    (format t "  État VM:~%")
    (format t "    $PC = ~A~%" (get-register *vm* :$PC))
    (format t "    $V0 = ~A~%" (get-register *vm* :$V0))))

(format t "~%Test terminé.~%")
