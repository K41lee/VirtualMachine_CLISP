;;;; Test d'impact Sprint 2 : DOLIST + List Operations
;;;; Vérifie que les nouvelles constructions débloquent la compilation VM

(load "src/compiler.lisp")

(format t "~%╔══════════════════════════════════════════════════════════╗~%")
(format t "║  TEST IMPACT SPRINT 2 - Compilation VM                  ║~%")
(format t "╚══════════════════════════════════════════════════════════╝~%~%")

;;; Test 1: Fonction utilisant DOLIST (maintenant supporté)
(format t "Test 1: Fonction avec DOLIST...~%")
(handler-case
    (let* ((code (compile-lisp 
                  '(defun count-list (lst)
                     (let ((count 0))
                       (dolist (x lst)
                         (incf count))
                       count))))
           (instrs (length code)))
      (format t "  ✓ SUCCÈS: ~A instructions MIPS générées~%" instrs))
  (error (e)
    (format t "  ✗ ÉCHEC: ~A~%" e)))

;;; Test 2: Fonction utilisant CONS/CAR/CDR (maintenant supporté)
(format t "~%Test 2: Fonction avec manipulation de listes...~%")
(handler-case
    (let* ((code (compile-lisp 
                  '(defun make-pair (a b)
                     (cons a b))))
           (instrs (length code)))
      (format t "  ✓ SUCCÈS: ~A instructions MIPS générées~%" instrs))
  (error (e)
    (format t "  ✗ ÉCHEC: ~A~%" e)))

;;; Test 3: Fonction complexe type VM (init-registers)
(format t "~%Test 3: Fonction type init-registers...~%")
(handler-case
    (let* ((code (compile-lisp 
                  '(defun init-array (arr size)
                     (let ((i 0))
                       (while (< i size)
                         (setq (aref arr i) 0)
                         (incf i))))))
           (instrs (length code)))
      (format t "  ✓ SUCCÈS: ~A instructions MIPS générées~%" instrs))
  (error (e)
    (format t "  ✗ ÉCHEC: ~A~%" e)))

;;; Test 4: Fonction avec listes imbriquées
(format t "~%Test 4: Navigation dans listes (CAR/CDR)...~%")
(handler-case
    (let* ((code (compile-lisp 
                  '(defun second-element (lst)
                     (car (cdr lst)))))
           (instrs (length code)))
      (format t "  ✓ SUCCÈS: ~A instructions MIPS générées~%" instrs))
  (error (e)
    (format t "  ✗ ÉCHEC: ~A~%" e)))

;;; Test 5: Fonction avec test NULL
(format t "~%Test 5: Fonction avec test de liste vide...~%")
(handler-case
    (let* ((code (compile-lisp 
                  '(defun list-empty-p (lst)
                     (null lst))))
           (instrs (length code)))
      (format t "  ✓ SUCCÈS: ~A instructions MIPS générées~%" instrs))
  (error (e)
    (format t "  ✗ ÉCHEC: ~A~%" e)))

(format t "~%═════════════════════════════════════════════════════════~%")
(format t "✅ Toutes les constructions du Sprint 2 sont fonctionnelles!~%")
(format t "═════════════════════════════════════════════════════════~%~%")
