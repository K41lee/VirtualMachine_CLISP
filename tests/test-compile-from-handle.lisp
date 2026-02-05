#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST: compile-from-handle - Compilation depuis handle
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "utils-bootstrap.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║          TEST: compile-from-handle                             ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *fibo-def*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

;;; ============================================================================
;;; Test 1: Compilation directe (référence)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: Compilation DIRECTE avec compile-lisp-to-mips-simplified~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Expression: ~A~%" *fibo-def*)

(defparameter *code-direct* (compile-lisp-to-mips-simplified *fibo-def*))

(format t "~%✓ Code généré: ~A instructions~%" (length *code-direct*))

;;; ============================================================================
;;; Test 2: Compilation via handle
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: Compilation via HANDLE avec compile-from-handle~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Étape 1: Construction de l'expression en mémoire...~%")
(defparameter *fibo-handle* (build-expression-in-vm *fibo-def*))
(format t "✓ Handle: ~A~%" *fibo-handle*)

(format t "~%Étape 2: Vérification de la reconstruction...~%")
(defparameter *fibo-reconstructed* (read-expression-from-vm *fibo-handle*))
(format t "✓ Expression reconstruite: ~A~%" *fibo-reconstructed*)

(format t "~%Étape 3: Compilation via compile-from-handle...~%")
(defparameter *code-from-handle* (compile-from-handle *fibo-handle*))
(format t "✓ Code généré: ~A instructions~%" (length *code-from-handle*))

;;; ============================================================================
;;; Test 3: Comparaison des résultats
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 3: Comparaison des codes générés~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Longueur code direct:        ~A instructions~%" (length *code-direct*))
(format t "Longueur code depuis handle: ~A instructions~%" (length *code-from-handle*))

(if (equal *code-direct* *code-from-handle*)
    (progn
      (format t "~%✅ SUCCESS! Les deux codes sont IDENTIQUES!~%")
      (format t "~%Cela prouve que compile-from-handle fonctionne correctement.~%")
      (format t "On peut maintenant compiler cette fonction et l'utiliser dans la VM~%")
      (format t "pour compiler des expressions depuis leur handle.~%"))
    (progn
      (format t "~%❌ ÉCHEC: Les codes sont différents.~%")
      (format t "~%Code direct (premiers éléments):~%")
      (format t "~A~%" (subseq *code-direct* 0 (min 10 (length *code-direct*))))
      (format t "~%Code depuis handle (premiers éléments):~%")
      (format t "~A~%" (subseq *code-from-handle* 0 (min 10 (length *code-from-handle*))))))

(format t "~%════════════════════════════════════════════════════════════════~%")
