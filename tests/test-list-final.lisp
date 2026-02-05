#!/usr/bin/env clisp
;;; Test validation finale de l'instruction LIST

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║          TEST VALIDATION INSTRUCTION LIST                     ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defun test-list-instruction ()
  "Teste l'instruction LIST avec différentes tailles de listes"
  (let ((tests-passed 0)
        (tests-total 0))
    
    ;; Test 1: Liste vide
    (format t "~%Test 1: Liste vide~%")
    (incf tests-total)
    (let* ((code '((:LIST 0) (:HALT)))
           (vm (make-new-vm)))
      (handler-case
          (progn
            (load-code vm code)
            (run-vm vm)
            (let* ((handle (get-value vm :$v0))
                   (list-val (gethash handle *vm-lisp-objects*)))
              (if (null list-val)
                  (progn
                    (format t "  ✅ Liste vide créée: ~A~%" list-val)
                    (incf tests-passed))
                  (format t "  ❌ Attendu NIL, reçu: ~A~%" list-val))))
        (error (e)
          (format t "  ❌ Erreur: ~A~%" e))))
    
    ;; Test 2: Liste avec 2 éléments
    (format t "~%Test 2: Liste (42 43)~%")
    (incf tests-total)
    (let* ((code '((:LI 42 :$V0) (:PUSH :$V0)
                   (:LI 43 :$V0) (:PUSH :$V0)
                   (:LIST 2) (:HALT)))
           (vm (make-new-vm)))
      (handler-case
          (progn
            (load-code vm code)
            (run-vm vm)
            (let* ((handle (get-value vm :$v0))
                   (list-val (gethash handle *vm-lisp-objects*)))
              (if (equal list-val '(42 43))
                  (progn
                    (format t "  ✅ Liste créée: ~A (handle: ~A)~%" list-val handle)
                    (incf tests-passed))
                  (format t "  ❌ Attendu (42 43), reçu: ~A~%" list-val))))
        (error (e)
          (format t "  ❌ Erreur: ~A~%" e))))
    
    ;; Test 3: Liste avec 5 éléments
    (format t "~%Test 3: Liste (1 2 3 4 5)~%")
    (incf tests-total)
    (let* ((code '((:LI 1 :$V0) (:PUSH :$V0)
                   (:LI 2 :$V0) (:PUSH :$V0)
                   (:LI 3 :$V0) (:PUSH :$V0)
                   (:LI 4 :$V0) (:PUSH :$V0)
                   (:LI 5 :$V0) (:PUSH :$V0)
                   (:LIST 5) (:HALT)))
           (vm (make-new-vm)))
      (handler-case
          (progn
            (load-code vm code)
            (run-vm vm)
            (let* ((handle (get-value vm :$v0))
                   (list-val (gethash handle *vm-lisp-objects*)))
              (if (equal list-val '(1 2 3 4 5))
                  (progn
                    (format t "  ✅ Liste créée: ~A (handle: ~A)~%" list-val handle)
                    (incf tests-passed))
                  (format t "  ❌ Attendu (1 2 3 4 5), reçu: ~A~%" list-val))))
        (error (e)
          (format t "  ❌ Erreur: ~A~%" e))))
    
    ;; Test 4: Multiples handles (isolation)
    (format t "~%Test 4: Multiples handles (isolation)~%")
    (incf tests-total)
    (let* ((code '(;; Première liste: (10 20)
                   (:LI 10 :$V0) (:PUSH :$V0)
                   (:LI 20 :$V0) (:PUSH :$V0)
                   (:LIST 2)
                   (:MOVE :$V0 :$T0)  ; Sauver handle 1
                   ;; Deuxième liste: (30 40)
                   (:LI 30 :$V0) (:PUSH :$V0)
                   (:LI 40 :$V0) (:PUSH :$V0)
                   (:LIST 2)
                   (:MOVE :$V0 :$T1)  ; Sauver handle 2
                   (:HALT)))
           (vm (make-new-vm)))
      (handler-case
          (progn
            (load-code vm code)
            (run-vm vm)
            (let* ((handle1 (get-value vm :$t0))
                   (handle2 (get-value vm :$t1))
                   (list1 (gethash handle1 *vm-lisp-objects*))
                   (list2 (gethash handle2 *vm-lisp-objects*)))
              (if (and (equal list1 '(10 20))
                       (equal list2 '(30 40))
                       (not (= handle1 handle2)))
                  (progn
                    (format t "  ✅ Deux listes isolées:~%")
                    (format t "     Handle ~A: ~A~%" handle1 list1)
                    (format t "     Handle ~A: ~A~%" handle2 list2)
                    (incf tests-passed))
                  (format t "  ❌ Problème d'isolation~%"))))
        (error (e)
          (format t "  ❌ Erreur: ~A~%" e))))
    
    ;; Résumé
    (format t "~%╔════════════════════════════════════════════════════════════════╗~%")
    (format t "║                         RÉSUMÉ                                 ║~%")
    (format t "╚════════════════════════════════════════════════════════════════╝~%")
    (format t "Tests réussis: ~A/~A~%" tests-passed tests-total)
    (format t "Taux de succès: ~,1F%%~%" (* 100.0 (/ tests-passed tests-total)))
    (if (= tests-passed tests-total)
        (format t "~%🎉 TOUS LES TESTS RÉUSSIS! L'instruction LIST fonctionne parfaitement! 🎉~%")
        (format t "~%❌ Certains tests ont échoué~%"))
    
    (= tests-passed tests-total)))

(test-list-instruction)
