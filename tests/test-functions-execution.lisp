#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST SIMPLE - Exécuter une fonction qui retourne 42
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST SIMPLE - Exécuter une fonction dans la VM             ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; Test 1: Fonction très simple qui retourne 42
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: Fonction (defun ret-42 () 42)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *fn1* '(defun ret-42 () 42))
(format t "~%1. Compilation...~%")
(defparameter *code1* (compile-lisp-to-mips-simplified *fn1*))
(format t "   ✓ ~A instructions~%" (length *code1*))

(format t "~%2. Chargement dans la VM...~%")
(defparameter *vm1* (make-new-vm))
(load-code *vm1* *code1*)
(format t "   ✓ Chargé~%")

(format t "~%3. Appel de la fonction...~%")
(defparameter *fn1-addr* (+ (calculate-code-start *vm1*) 1))
(format t "   Adresse: ~A~%" *fn1-addr*)

;; Créer un wrapper qui appelle ret-42
(defparameter *wrapper1*
  `((:JAL ,*fn1-addr*)
    (:HALT)))

(format t "~%4. Exécution du wrapper...~%")
(defparameter *vm1-exec* (make-new-vm))
(load-code *vm1-exec* *code1*)

;; Charger le wrapper après
(let ((wrapper-start (+ (calculate-code-start *vm1-exec*) (length *code1*))))
  (dotimes (i (length *wrapper1*))
    (mem-write *vm1-exec* (+ wrapper-start i) (nth i *wrapper1*)))
  (set-register *vm1-exec* (get-reg :pc) wrapper-start)
  
  (run-vm *vm1-exec*)
  
  (let ((result (get-value *vm1-exec* :$v0)))
    (format t "~%   Résultat dans $V0: ~A~%" result)
    (if (= result 42)
        (format t "   ✅ SUCCÈS! La fonction a retourné 42!~%")
        (format t "   ❌ Attendu 42, reçu ~A~%" result))))

;;; Test 2: Fonction avec paramètre
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: Fonction (defun double (x) (* x 2))~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *fn2* '(defun double (x) (* x 2)))
(format t "~%1. Compilation...~%")
(defparameter *code2* (compile-lisp-to-mips-simplified *fn2*))
(format t "   ✓ ~A instructions~%" (length *code2*))

(format t "~%2. Chargement dans la VM...~%")
(defparameter *vm2* (make-new-vm))
(load-code *vm2* *code2*)
(format t "   ✓ Chargé~%")

(format t "~%3. Appel de la fonction avec paramètre 21...~%")
(defparameter *fn2-addr* (+ (calculate-code-start *vm2*) 1))

;; Wrapper qui passe 21 en paramètre
(defparameter *wrapper2*
  `((:LI 21 :$A0)        ; Paramètre x = 21
    (:JAL ,*fn2-addr*)   ; Appel double(21)
    (:HALT)))

(format t "~%4. Exécution...~%")
(defparameter *vm2-exec* (make-new-vm))
(load-code *vm2-exec* *code2*)

(let ((wrapper-start (+ (calculate-code-start *vm2-exec*) (length *code2*))))
  (dotimes (i (length *wrapper2*))
    (mem-write *vm2-exec* (+ wrapper-start i) (nth i *wrapper2*)))
  (set-register *vm2-exec* (get-reg :pc) wrapper-start)
  
  (run-vm *vm2-exec*)
  
  (let ((result (get-value *vm2-exec* :$v0)))
    (format t "~%   Résultat dans $V0: ~A~%" result)
    (if (= result 42)
        (format t "   ✅ SUCCÈS! double(21) = 42!~%")
        (format t "   ❌ Attendu 42, reçu ~A~%" result))))

;;; Test 3: Fonction récursive simple
(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 3: Fonction récursive fact(5)~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *fn3* '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(format t "~%1. Compilation...~%")
(defparameter *code3* (compile-lisp-to-mips-simplified *fn3*))
(format t "   ✓ ~A instructions~%" (length *code3*))

(format t "~%2. Chargement dans la VM...~%")
(defparameter *vm3* (make-new-vm))
(load-code *vm3* *code3*)
(format t "   ✓ Chargé~%")

(format t "~%3. Appel de fact(5)...~%")
(defparameter *fn3-addr* (+ (calculate-code-start *vm3*) 1))

;; Wrapper qui passe 5 en paramètre
(defparameter *wrapper3*
  `((:LI 5 :$A0)         ; Paramètre n = 5
    (:JAL ,*fn3-addr*)   ; Appel fact(5)
    (:HALT)))

(format t "~%4. Exécution...~%")
(defparameter *vm3-exec* (make-new-vm))
(load-code *vm3-exec* *code3*)

(let ((wrapper-start (+ (calculate-code-start *vm3-exec*) (length *code3*))))
  (dotimes (i (length *wrapper3*))
    (mem-write *vm3-exec* (+ wrapper-start i) (nth i *wrapper3*)))
  (set-register *vm3-exec* (get-reg :pc) wrapper-start)
  
  (run-vm *vm3-exec*)
  
  (let ((result (get-value *vm3-exec* :$v0)))
    (format t "~%   Résultat dans $V0: ~A~%" result)
    (if (= result 120)
        (format t "   ✅ SUCCÈS! fact(5) = 120!~%")
        (format t "   ❌ Attendu 120, reçu ~A~%" result))))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ: Les fonctions compilées s'EXÉCUTENT dans la VM!~%")
(format t "════════════════════════════════════════════════════════════════~%")
