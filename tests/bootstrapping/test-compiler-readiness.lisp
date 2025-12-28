;;;; test-compiler-readiness.lisp
;;;; Test de préparation au bootstrapping : vérifier que toutes les
;;;; constructions utilisées par compiler.lisp sont supportées

(load "src/vm.lisp")
(load "src/compiler.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "TEST DE PRÉPARATION AU BOOTSTRAPPING~%")
(format t "═══════════════════════════════════════════════════~%~%")

(format t "Objectif : Vérifier que le compilateur peut compiler~%")
(format t "toutes les constructions qu'il utilise lui-même.~%~%")

(defvar *tests-passed* 0)
(defvar *tests-total* 0)

(defun test-construct (name expr)
  "Teste si une construction peut être compilée"
  (incf *tests-total*)
  (format t "Test ~A : ~A~%" *tests-total* name)
  (handler-case
      (let* ((env (make-new-compiler-env))
             (code (compile-expr expr env)))
        (if (and code (> (length code) 0))
            (progn
              (incf *tests-passed*)
              (format t "  ✓ Compile (~A instructions)~%~%" (length code)))
            (format t "  ✗ Code vide~%~%")))
    (error (e)
      (format t "  ✗ Erreur : ~A~%~%" e))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DES CONSTRUCTIONS DE BASE
;;; ═══════════════════════════════════════════════════════════════════

(format t "──────────────────────────────────────────────────~%")
(format t "CONSTRUCTIONS DE BASE~%")
(format t "──────────────────────────────────────────────────~%~%")

(test-construct "Fonction simple"
                '(defun add (a b) (+ a b)))

(test-construct "IF"
                '(defun test-if (a b) (if (< a b) a b)))

(test-construct "COND"
                '(defun test-cond (a) (cond ((< a 0) 0) ((> a 10) 10) (t a))))

(test-construct "WHEN"
                '(defun test-when (x) (when (> x 0) (+ x 1))))

(test-construct "UNLESS"
                '(defun test-unless (x) (unless (= x 0) (/ 10 x))))

(test-construct "LET"
                '(let ((x 10) (y 20)) (+ x y)))

(test-construct "LET*"
                '(let* ((x 10) (y (+ x 5))) y))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DES PARAMÈTRES AVANCÉS
;;; ═══════════════════════════════════════════════════════════════════

(format t "──────────────────────────────────────────────────~%")
(format t "PARAMÈTRES AVANCÉS~%")
(format t "──────────────────────────────────────────────────~%~%")

(test-construct "&optional"
                '(defun foo (a &optional (b 10)) (+ a b)))

(test-construct "&rest"
                '(defun bar (a &rest r) (cons a r)))

(test-construct "&key"
                '(defun baz (a &key (x 1)) (+ a x)))

(test-construct "&optional + &rest"
                '(defun qux (a &optional b &rest r) (list a b r)))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DES FONCTIONS DE LISTE
;;; ═══════════════════════════════════════════════════════════════════

(format t "──────────────────────────────────────────────────~%")
(format t "FONCTIONS DE LISTE~%")
(format t "──────────────────────────────────────────────────~%~%")

(test-construct "CONS"
                '(cons 1 2))

(test-construct "CAR"
                '(defun test-car (x) (car x)))

(test-construct "CDR"
                '(defun test-cdr (x) (cdr x)))

(test-construct "LIST"
                '(defun test-list () (list 1 2 3)))

(test-construct "LENGTH"
                '(defun test-length (x) (length x)))

(test-construct "NTH"
                '(defun test-nth (x) (nth 0 x)))

(test-construct "MEMBER"
                '(defun test-member (lst) (member (quote x) lst)))

(test-construct "ASSOC"
                '(defun test-assoc (alist) (assoc (quote key) alist)))

(test-construct "APPEND"
                '(defun test-append (l1 l2) (append l1 l2)))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DES OPÉRATIONS LOGIQUES
;;; ═══════════════════════════════════════════════════════════════════

(format t "──────────────────────────────────────────────────~%")
(format t "OPÉRATIONS LOGIQUES~%")
(format t "──────────────────────────────────────────────────~%~%")

(test-construct "AND"
                '(defun test-and (x) (and (> x 0) (< x 10))))

(test-construct "OR"
                '(defun test-or (x) (or (null x) (= x 0))))

(test-construct "NOT"
                '(defun test-not (x) (not (null x))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DES BOUCLES
;;; ═══════════════════════════════════════════════════════════════════

(format t "──────────────────────────────────────────────────~%")
(format t "BOUCLES~%")
(format t "──────────────────────────────────────────────────~%~%")

(test-construct "LOOP simple"
                '(defun test-loop-while (x) (loop while (> x 0) do (setq x (- x 1)))))

(test-construct "LOOP FOR"
                '(defun test-loop-for () (loop for i from 0 to 10 do (print i))))

(test-construct "LOOP COLLECT"
                '(defun test-loop-collect (lst) (loop for x in lst collect (* x 2))))

;;; ═══════════════════════════════════════════════════════════════════
;;; TESTS DES STRUCTURES
;;; ═══════════════════════════════════════════════════════════════════

(format t "──────────────────────────────────────────────────~%")
(format t "STRUCTURES~%")
(format t "──────────────────────────────────────────────────~%~%")

(test-construct "DEFSTRUCT"
                '(defstruct point x y))

(test-construct "MAKE-STRUCT"
                '(make-point :x 10 :y 20))

(test-construct "ACCESSEUR"
                '(defun test-accessor (p) (point-x p)))

;;; ═══════════════════════════════════════════════════════════════════
;;; RÉSUMÉ
;;; ═══════════════════════════════════════════════════════════════════

(format t "~%═══════════════════════════════════════════════════~%")
(format t "RÉSUMÉ DES TESTS~%")
(format t "═══════════════════════════════════════════════════~%~%")

(format t "Tests réussis : ~A / ~A~%" *tests-passed* *tests-total*)
(format t "Taux de réussite : ~,1F%~%~%" 
        (* 100.0 (/ *tests-passed* *tests-total*)))

(if (= *tests-passed* *tests-total*)
    (format t "✅ TOUS LES TESTS PASSENT !~%~%Le compilateur est prêt pour le bootstrapping.~%")
    (format t "⚠️  ~A test(s) échoué(s)~%~%Corrections nécessaires avant bootstrapping.~%"
            (- *tests-total* *tests-passed*)))

(format t "~%Prochaine étape : Implémenter les constructions manquantes~%")
(format t "pour atteindre 100%% de support.~%~%")
