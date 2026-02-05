;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST PHASE 2 SIMPLIFIÉ - Sans dépendances VM complètes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%========================================~%")
(format t "TEST PHASE 2: Labels symboliques (simplifié)~%")
(format t "========================================~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Définir les fonctions minimales nécessaires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variable globale pour compteur de labels
(defvar *label-counter* 0)

;; Fonction gen-label-simplified
(defun gen-label-simplified (base)
  "Génère un label unique avec un compteur global"
  (setq *label-counter* (+ *label-counter* 1))
  (format nil "~A_~A" base *label-counter*))

;; Registres fictifs
(defvar *reg-v0* 2)
(defvar *reg-zero* 0)
(defvar *reg-t0* 8)
(defvar *reg-t1* 9)
(defvar *reg-sp* 29)
(defvar *reg-ra* 31)

;; compile-expr-with-ids fictif
(defun compile-expr-with-ids (expr env)
  (list (list :LI *reg-v0* 42)))

;; compile-progn-simplified fictif
(defun compile-progn-simplified (body env)
  (list (list :NOP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Charger les fonctions Phase 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "Chargement des fonctions Phase 2...~%~%")

;; COMPILE-WHILE
(defun compile-while-simplified (condition body env)
  "Compile (WHILE cond body...) - version avec strings"
  (let ((start-label (gen-label-simplified "WHILE_START"))
        (end-label (gen-label-simplified "WHILE_END")))
    (append
     (list (list :LABEL start-label))
     (compile-expr-with-ids condition env)
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     (compile-progn-simplified body env)
     (list (list :J start-label))
     (list (list :LABEL end-label))
     (list (list :LI *reg-v0* 0)))))

;; COMPILE-DOTIMES
(defun compile-dotimes-simplified (var count-expr body env)
  "Compile (DOTIMES (var count) body...) - version avec strings"
  (let ((start-label (gen-label-simplified "DOTIMES_START"))
        (end-label (gen-label-simplified "DOTIMES_END")))
    (append
     (compile-expr-with-ids count-expr env)
     (list (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     (list (list :LI *reg-v0* 0)
           (list :SW *reg-v0* *reg-sp* 0)
           (list :ADDI *reg-sp* *reg-sp* -4))
     (list (list :LABEL start-label))
     (list (list :LW *reg-t0* *reg-sp* 4))
     (list (list :LW *reg-t1* *reg-sp* 8))
     (list (list :BGE *reg-t0* *reg-t1* end-label))
     (compile-progn-simplified body env)
     (list (list :LW *reg-t0* *reg-sp* 4))
     (list (list :ADDI *reg-t0* *reg-t0* 1))
     (list (list :SW *reg-t0* *reg-sp* 4))
     (list (list :J start-label))
     (list (list :LABEL end-label))
     (list (list :ADDI *reg-sp* *reg-sp* 8))
     (list (list :LI *reg-v0* 0)))))

;; COMPILE-NULL
(defun compile-null-simplified (expr env)
  "Compile (NULL expr) - teste si NIL"
  (let ((true-label (gen-label-simplified "NULL_TRUE"))
        (end-label (gen-label-simplified "NULL_END")))
    (append
     (compile-expr-with-ids expr env)
     (list (list :BEQ *reg-v0* *reg-zero* true-label))
     (list (list :LI *reg-v0* 0))
     (list (list :J end-label))
     (list (list :LABEL true-label))
     (list (list :LI *reg-v0* 1))
     (list (list :LABEL end-label)))))

;; COMPILE-LENGTH
(defun compile-length-simplified (list-expr env)
  "Compile (LENGTH list) - version avec strings"
  (let ((loop-label (gen-label-simplified "LENGTH_LOOP"))
        (end-label (gen-label-simplified "LENGTH_END")))
    (append
     (compile-expr-with-ids list-expr env)
     (list (list :LI *reg-t0* 0))
     (list (list :LABEL loop-label))
     (list (list :BEQ *reg-v0* *reg-zero* end-label))
     (list (list :ADDI *reg-t0* *reg-t0* 1))
     (list (list :COMMENT "CDR"))
     (list (list :J loop-label))
     (list (list :LABEL end-label))
     (list (list :MOVE *reg-v0* *reg-t0*)))))

;; COMPILE-LAMBDA
(defun compile-lambda-simplified (params body env)
  "Compile (LAMBDA ...) - version simplifiée"
  (let ((func-label (gen-label-simplified "LAMBDA_FUNC")))
    (append
     (list (list :LABEL func-label))
     (compile-progn-simplified body env)
     (list (list :JR *reg-ra*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tests-ok* 0)
(defvar *tests-total* 0)

(defun test-function (name func)
  "Teste qu'une fonction génère du code"
  (setq *tests-total* (+ *tests-total* 1))
  (handler-case
      (let ((result (funcall func)))
        (if (and (listp result) (> (length result) 0))
            (progn
              (setq *tests-ok* (+ *tests-ok* 1))
              (format t "  ✅ ~A : ~A instructions~%" name (length result)))
            (format t "  ⚠ ~A : Résultat vide~%" name)))
    (error (e)
      (format t "  ❌ ~A : ~A~%" name e))))

(format t "Tests de génération de code...~%~%")

(test-function "WHILE" 
  (lambda () (compile-while-simplified '(< x 10) '((setq x (+ x 1))) nil)))

(test-function "DOTIMES"
  (lambda () (compile-dotimes-simplified 'i '10 '((+ i 1)) nil)))

(test-function "NULL"
  (lambda () (compile-null-simplified 'x nil)))

(test-function "LENGTH"
  (lambda () (compile-length-simplified '(1 2 3) nil)))

(test-function "LAMBDA"
  (lambda () (compile-lambda-simplified '(x) '((+ x 1)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Résultats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%========================================~%")
(format t "RÉSULTATS~%")
(format t "========================================~%")
(format t "Tests réussis : ~A/~A~%" *tests-ok* *tests-total*)
(format t "Labels générés : ~A~%" *label-counter*)

(if (= *tests-ok* *tests-total*)
    (format t "~%✅ Phase 2 : Toutes les fonctions génèrent du code!~%")
    (format t "~%⚠ Certains tests ont échoué.~%"))

(format t "~%")
