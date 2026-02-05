;;;; ============================================================================
;;;; CODE.LISP - Définition de la fonction à compiler et exécuter
;;;; ============================================================================
;;;;
;;;; Ce fichier est chargé par :
;;;;   - exec-code.lisp
;;;;   - exec-code-bootstrap.lisp
;;;;   - show-compile.lisp
;;;;   - show-compile-bootstrap.lisp
;;;;
;;;; Modifiez ce fichier pour tester d'autres fonctions !
;;;; ============================================================================

;;; ============================================================================
;;; Définition de la fonction (pour compilation)
;;; ============================================================================

(defparameter *function-definition*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2)))))
  "Définition de la fonction à compiler.
   Utilisé par show-compile.lisp et show-compile-bootstrap.lisp pour compilation.
   Utilisé par exec-code.lisp et exec-code-bootstrap.lisp pour compilation + exécution.")

;;; ============================================================================
;;; Paramètres d'exécution (pour exec-code.lisp et exec-code-bootstrap.lisp)
;;; ============================================================================

(defparameter *function-name* 'FIBO
  "Nom de la fonction à appeler (symbole en majuscules)")

(defparameter *function-args* '(20)
  "Arguments à passer à la fonction lors de l'exécution")

(defparameter *expected-result* 6765
  "Résultat attendu pour vérification")

;;; ============================================================================
;;; Exemples d'autres fonctions à tester
;;; ============================================================================

;; Pour tester une autre fonction, décommentez l'une des sections ci-dessous
;; et commentez la section fibonacci ci-dessus.

;; FIBONACCI
;;(defparameter *function-definition*
;;  '(defun fibo (n)
;;     (if (< n 2)
;;         n
;;         (+ (fibo (- n 1)) (fibo (- n 2)))))
;; (defparameter *function-name* 'FIBO
;;  "Nom de la fonction à appeler (symbole en majuscules)")
;;(defparameter *function-args* '(20)
;;  "Arguments à passer à la fonction lors de l'exécution")
;;(defparameter *expected-result* 6765
;;  "Résultat attendu pour vérification")

;; FACTORIELLE
;; (defparameter *function-definition*
;;   '(defun fact (n)
;;      (if (<= n 1)
;;          1
;;          (* n (fact (- n 1))))))
;; (defparameter *function-name* 'FACT)
;; (defparameter *function-args* '(10))
;; (defparameter *expected-result* 3628800)

;; ACKERMANN
;; (defparameter *function-definition*
;;   '(defun ack (m n)
;;      (cond
;;        ((= m 0) (+ n 1))
;;        ((= n 0) (ack (- m 1) 1))
;;        (t (ack (- m 1) (ack m (- n 1)))))))
;; (defparameter *function-name* 'ACK)
;; (defparameter *function-args* '(3 4))
;; (defparameter *expected-result* 125)

;; SOMME (simple, pas récursif)
;; (defparameter *function-definition*
;;   '(defun sum (a b c)
;;      (+ (+ a b) c)))
;; (defparameter *function-name* 'SUM)
;; (defparameter *function-args* '(10 20 30))
;; (defparameter *expected-result* 60)

;; PUISSANCE (récursif)
;; (defparameter *function-definition*
;;   '(defun power (base exp)
;;      (if (= exp 0)
;;          1
;;          (* base (power base (- exp 1))))))
;; (defparameter *function-name* 'POWER)
;; (defparameter *function-args* '(2 10))
;; (defparameter *expected-result* 1024)

;;; ============================================================================
;;; Affichage d'information
;;; ============================================================================

(format t "~%Code chargé depuis code.lisp:~%")
(format t "  Fonction: ~A~%" (second *function-definition*))
(format t "  Arguments: ~A~%" *function-args*)
(format t "  Résultat attendu: ~A~%~%" *expected-result*)
