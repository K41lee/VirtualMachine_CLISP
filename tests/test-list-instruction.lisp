;;;; Test de LIST avec la VM
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%Test de l'instruction LIST~%")
(format t "═══════════════════════════════~%~%")

;; Fonction simple qui crée une liste
(defparameter *test-fn* '(defun make-list () (list 1 2 3)))

(format t "Compilation de: ~A~%" *test-fn*)
(defparameter *compiled* (compile-lisp-to-mips-simplified *test-fn*))
(format t "→ ~A instructions~%~%" (length *compiled*))

;; Charger et exécuter
(defparameter *vm* (make-new-vm :verbose t))
(load-code *vm* *compiled*)

(format t "~%Exécution...~%~%")
(run-vm *vm*)

(format t "~%Résultat:~%")
(defparameter *result-handle* (get-register *vm* :$V0))
(format t "  Handle dans $V0: ~A~%" *result-handle*)

;; Récupérer la liste
(defparameter *result-list* (gethash *result-handle* *vm-lisp-objects*))
(format t "  Liste Lisp: ~A~%" *result-list*)
(format t "  Type: ~A~%" (type-of *result-list*))

(if (equal *result-list* '(1 2 3))
    (format t "~%✅ TEST RÉUSSI !~%")
    (format t "~%❌ Résultat incorrect~%"))
