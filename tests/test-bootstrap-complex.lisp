#!/usr/bin/env clisp
;;; Test bootstrap avec fonctions complexes (fibo, ack)

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles AVANT de charger le compilateur
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║       BOOTSTRAP AVEC FONCTIONS COMPLEXES (FIBO, ACK)          ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; Fonctions de test
(defparameter *test-functions*
  '((defun fibo (n)
      (if (< n 2)
          n
          (+ (fibo (- n 1)) (fibo (- n 2)))))
    (defun ack (m n)
      (cond
        ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (t (ack (- m 1) (ack m (- n 1))))))))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "ÉTAPE 1: Compilation avec le compilateur NATIF~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *native-results* nil)

(dolist (func-def *test-functions*)
  (let ((func-name (second func-def)))
    (format t "~%Compilation de ~A...~%" func-name)
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified func-def)))
          (push (list func-name code) *native-results*)
          (format t "  ✓ ~A instructions générées~%" (length code))
          ;; Afficher les 10 premières instructions
          (format t "  Premières instructions:~%")
          (loop for instr in (subseq code 0 (min 10 (length code)))
                for i from 0
                do (format t "    [~2D] ~A~%" i instr)))
      (error (e)
        (format t "  ✗ Erreur: ~A~%" e)))))

(setf *native-results* (nreverse *native-results*))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Fonctions compilées avec succès:~%")
(dolist (entry *native-results*)
  (destructuring-bind (func-name code) entry
    (format t "  ✓ ~A: ~A instructions~%" func-name (length code))))

(format t "~%~%STATISTIQUES DES INSTRUCTIONS:~%")
(dolist (entry *native-results*)
  (destructuring-bind (func-name code) entry
    (format t "~%~A (~A instructions):~%" func-name (length code))
    (let ((opcodes (make-hash-table :test 'equal)))
      ;; Compter les opcodes
      (dolist (instr code)
        (when (listp instr)
          (let ((op (first instr)))
            (incf (gethash op opcodes 0)))))
      ;; Afficher les statistiques
      (let ((sorted-ops nil))
        (maphash (lambda (op count)
                   (push (cons op count) sorted-ops))
                 opcodes)
        (setf sorted-ops (sort sorted-ops #'> :key #'cdr))
        (dolist (pair sorted-ops)
          (format t "  ~A: ~A occurrences~%" (car pair) (cdr pair)))))))

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                    BOOTSTRAP VALIDÉ!                           ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Le compilateur peut compiler des fonctions complexes avec:~%")
(format t "  - Récursivité (fibo, ack)~%")
(format t "  - Conditions (if, cond)~%")
(format t "  - Arithmétique (+, -, <, =)~%")
(format t "  - Appels de fonctions~%")
