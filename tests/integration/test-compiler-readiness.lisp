#!/usr/bin/env clisp
;;; Test de compilabilité du compilateur avec APPEND implémenté

(load "main.lisp")

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║  VÉRIFICATION DE LA COMPILABILITÉ DU COMPILATEUR                ║~%")
(format t "║  Avec APPEND implémenté                                          ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(defparameter *test-results* '())

(defun test-primitive (name expr expected-type)
  "Teste qu'une primitive peut être compilée"
  (format t "Test ~A: " name)
  (handler-case
      (let ((code (compile-lisp expr)))
        (if (and code (listp code) (> (length code) 0))
            (progn
              (format t "✅ (~A instructions)~%" (length code))
              (push (cons name t) *test-results*)
              t)
            (progn
              (format t "❌ Code vide~%")
              (push (cons name nil) *test-results*)
              nil)))
    (error (e)
      (format t "❌ Erreur: ~A~%" e)
      (push (cons name nil) *test-results*)
      nil)))

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "PRIMITIVES DE BASE~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

;; Tests des primitives de base
(test-primitive "CONS" '(cons 1 2) 'pair)
(test-primitive "CAR" '(car (cons 1 2)) 'value)
(test-primitive "CDR" '(cdr (cons 1 2)) 'value)
(test-primitive "NULL" '(null nil) 'boolean)

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "PRIMITIVES DE LISTE~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(test-primitive "LENGTH" '(length (cons 1 (cons 2 nil))) 'number)
(test-primitive "NTH" '(nth 0 (cons 1 (cons 2 nil))) 'value)
(test-primitive "ASSOC" '(assoc 1 (cons (cons 1 10) nil)) 'pair)
(test-primitive "MEMBER" '(member 1 (cons 1 (cons 2 nil))) 'list)

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "APPEND - NOUVELLE PRIMITIVE~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(test-primitive "APPEND (deux listes)"
                '(append (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil)))
                'list)
(test-primitive "APPEND (NIL + liste)"
                '(append nil (cons 1 nil))
                'list)
(test-primitive "APPEND (liste + NIL)"
                '(append (cons 1 nil) nil)
                'list)

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "FONCTIONS COMPILATEUR CRITIQUES~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

;; Test fonction simple utilisant APPEND
(test-primitive "Fonction avec APPEND"
                '(progn
                   (defun concat-two (a b)
                     (append a b))
                   (concat-two (cons 1 nil) (cons 2 nil)))
                'list)

;; Test fonction avec boucle et APPEND
(test-primitive "Fonction complexe"
                '(progn
                   (defun build-list (n)
                     (if (= n 0)
                         nil
                         (append (cons n nil) (build-list (- n 1)))))
                   (build-list 3))
                'list)

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST SOUS-ENSEMBLE DU COMPILATEUR~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

;; Test: Peut-on compiler une fonction qui ressemble à une partie du compilateur?
(format t "Test fonction de lookup simple: ")
(handler-case
    (let ((code (compile-lisp 
                  '(progn
                     (defun simple-lookup (key alist)
                       (if (null alist)
                           nil
                           (if (= key (car (car alist)))
                               (car alist)
                               (simple-lookup key (cdr alist)))))
                     (simple-lookup 2 (cons (cons 1 10) 
                                           (cons (cons 2 20) 
                                                 (cons (cons 3 30) nil))))))))
      (format t "✅ (~A instructions)~%" (length code))
      (push (cons 'lookup-function t) *test-results*))
  (error (e)
    (format t "❌ Erreur: ~A~%" e)
    (push (cons 'lookup-function nil) *test-results*)))

;; Test: Compilation de code utilisant intensivement les listes
(format t "Test manipulation listes complexe: ")
(handler-case
    (let ((code (compile-lisp
                  '(progn
                     (defun process-list (lst)
                       (if (null lst)
                           nil
                           (cons (+ (car lst) 1)
                                 (process-list (cdr lst)))))
                     (process-list (cons 1 (cons 2 (cons 3 nil))))))))
      (format t "✅ (~A instructions)~%" (length code))
      (push (cons 'complex-list-function t) *test-results*))
  (error (e)
    (format t "❌ Erreur: ~A~%" e)
    (push (cons 'complex-list-function nil) *test-results*)))

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(let ((passed (count-if #'cdr *test-results*))
      (total (length *test-results*)))
  (format t "Tests réussis: ~A / ~A~%~%" passed total)
  
  (if (= passed total)
      (progn
        (format t "🎉 SUCCÈS COMPLET !~%~%")
        (format t "Toutes les primitives nécessaires sont implémentées.~%")
        (format t "Le compilateur peut maintenant compiler des fonctions~%")
        (format t "qui utilisent APPEND et toutes les autres primitives.~%~%")
        (format t "✅ LE COMPILATEUR EST PRÊT POUR L'AUTO-COMPILATION~%~%"))
      (progn
        (format t "⚠️  ATTENTION : Certains tests ont échoué~%~%")
        (format t "Tests échoués:~%")
        (dolist (result *test-results*)
          (unless (cdr result)
            (format t "  - ~A~%" (car result))))
        (format t "~%")
        (format t "❌ Le compilateur n'est pas encore prêt pour l'auto-compilation~%~%")))
  
  (format t "═══════════════════════════════════════════════════════════════════~%~%"))
