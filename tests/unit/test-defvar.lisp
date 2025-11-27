;;; ============================================================================
;;; Tests unitaires pour DEFVAR
;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/compiler.lisp")

(format t "~%=== TEST DEFVAR ===~%~%")

;; Reset tables avant chaque série de tests
(reset-global-tables)

;;; ----------------------------------------------------------------------------
;;; Test 1 : Définir une variable globale simple
;;; ----------------------------------------------------------------------------
(format t "Test 1: Définir (defvar mystate 0)~%")
(let* ((expr '(defvar mystate 0))
       (env (make-compiler-env))
       (code (compile-expr expr env)))  ; compile-expr fait le parsing lui-même
  (format t "  Code généré: ~A instructions~%" (length code))
  (format t "  Adresse dans table: ~A~%" (gethash 'mystate *global-variables*))
  (if (and (numberp (gethash 'mystate *global-variables*))
           (>= (length code) 0))
      (format t "  ✓ PASS~%")
      (format t "  ✗ FAIL~%")))

;;; ----------------------------------------------------------------------------
;;; Test 2 : Utiliser une variable globale
;;; ----------------------------------------------------------------------------
(format t "~%Test 2: Référence à mystate dans expression~%")
(reset-global-tables)
;; Définir d'abord la variable
(compile-expr '(defvar mystate 42) (make-compiler-env))
;; Utiliser la variable dans une expression
(let* ((expr 'mystate)
       (env (make-compiler-env))
       (code (compile-expr expr env)))
  (format t "  Code généré: ~A instructions~%" (length code))
  (format t "  Type de première instruction: ~A~%" (if code (car (first code)) "AUCUNE"))
  (if (and code
           (eq (car (first code)) :LW))
      (format t "  ✓ PASS (génère LW)~%")
      (format t "  ✗ FAIL~%")))

;;; ----------------------------------------------------------------------------
;;; Test 3 : SETQ sur variable globale
;;; ----------------------------------------------------------------------------
(format t "~%Test 3: Modifier avec (setq mystate 100)~%")
(reset-global-tables)
;; Définir d'abord la variable
(compile-expr '(defvar mystate 0) (make-compiler-env))
;; Modifier la variable
(let* ((expr '(setq mystate 100))
       (env (make-compiler-env))
       (code (compile-expr expr env)))
  (format t "  Code généré: ~A instructions~%" (length code))
  (let ((last-instr (car (last code))))
    (format t "  Dernière instruction: ~A~%" (car last-instr))
    (if (eq (car last-instr) :SW)
        (format t "  ✓ PASS (génère SW)~%")
        (format t "  ✗ FAIL~%"))))

;;; ----------------------------------------------------------------------------
;;; Test 4 : Plusieurs variables globales
;;; ----------------------------------------------------------------------------
(format t "~%Test 4: Définir plusieurs variables~%")
(reset-global-tables)
(compile-expr '(defvar pc-reg 0) (make-compiler-env))
(compile-expr '(defvar sp-reg 1024) (make-compiler-env))
(compile-expr '(defvar state-reg 42) (make-compiler-env))
(let ((addr-pc (gethash 'pc-reg *global-variables*))
      (addr-sp (gethash 'sp-reg *global-variables*))
      (addr-state (gethash 'state-reg *global-variables*)))
  (format t "  pc-reg @ offset ~A~%" addr-pc)
  (format t "  sp-reg @ offset ~A~%" addr-sp)
  (format t "  state-reg @ offset ~A~%" addr-state)
  (if (and (= addr-pc 0) (= addr-sp 4) (= addr-state 8))
      (format t "  ✓ PASS (offsets corrects: 0, 4, 8)~%")
      (format t "  ✗ FAIL~%")))

;;; ----------------------------------------------------------------------------
;;; Test 5 : Fonction utilisant variable globale
;;; ----------------------------------------------------------------------------
(format t "~%Test 5: Fonction avec variable globale~%")
(reset-global-tables)
(compile-expr '(defvar counter 0) (make-compiler-env))
(let* ((expr '(defun increment () (setq counter (+ counter 1))))
       (env (make-compiler-env))
       (code (compile-expr expr env)))
  (format t "  Code généré: ~A instructions~%" (length code))
  (if (> (length code) 10)
      (format t "  ✓ PASS~%")
      (format t "  ✗ FAIL~%")))

(format t "~%=== FIN DES TESTS ===~%")
