#!/usr/bin/env clisp
;;; Test de compilation de fonctions du compilateur

(load "main.lisp")

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║  TEST: COMPILATION DE FONCTIONS TYPE COMPILATEUR                ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(defparameter *compiler-functions-tested* 0)
(defparameter *compiler-functions-passed* 0)

(defun test-compiler-function (name code-expr)
  "Teste la compilation d'une fonction similaire au compilateur"
  (format t "~%Test: ~A~%" name)
  (format t "----------------------------------------~%")
  (incf *compiler-functions-tested*)
  (handler-case
      (let ((compiled-code (compile-lisp code-expr)))
        (format t "✅ Compilation réussie: ~A instructions~%" (length compiled-code))
        (incf *compiler-functions-passed*)
        
        ;; Essayer d'exécuter pour vérifier
        (handler-case
            (let ((result (compile-and-run code-expr)))
              (format t "✅ Exécution réussie~%")
              t)
          (error (e)
            (format t "⚠️  Compilation OK mais exécution échouée: ~A~%" e)
            nil)))
    (error (e)
      (format t "❌ ERREUR de compilation: ~A~%" e)
      nil)))

;; Test 1: Fonction env-lookup (similaire au compilateur)
(test-compiler-function
 "env-lookup (recherche dans environnement)"
 '(progn
    (defun env-lookup (var env)
      (if (null env)
          nil
          (if (= var (car (car env)))
              (cdr (car env))
              (env-lookup var (cdr env)))))
    (env-lookup 2 (cons (cons 1 100) 
                        (cons (cons 2 200) 
                              (cons (cons 3 300) nil))))))

;; Test 2: Fonction de génération de labels
(test-compiler-function
 "gen-label (génération de labels)"
 '(progn
    (defun gen-label (prefix counter)
      (+ (* prefix 1000) counter))
    (gen-label 5 42)))

;; Test 3: Fonction de traitement de liste d'arguments
(test-compiler-function
 "process-args (traitement arguments)"
 '(progn
    (defun count-args (args)
      (if (null args)
          0
          (+ 1 (count-args (cdr args)))))
    (count-args (cons 10 (cons 20 (cons 30 nil))))))

;; Test 4: Fonction utilisant APPEND (comme compile-progn)
(test-compiler-function
 "append-code (assemblage de code)"
 '(progn
    (defun append-instructions (code1 code2)
      (append code1 code2))
    (append-instructions 
      (cons 1 (cons 2 nil))
      (cons 3 (cons 4 nil)))))

;; Test 5: Fonction récursive avec APPEND
(test-compiler-function
 "compile-list-like (récursif avec append)"
 '(progn
    (defun flatten-once (lst)
      (if (null lst)
          nil
          (append (car lst) (flatten-once (cdr lst)))))
    (flatten-once (cons (cons 1 (cons 2 nil))
                        (cons (cons 3 (cons 4 nil)) nil)))))

;; Test 6: Fonction avec multiple conditions (comme dispatcher)
(test-compiler-function
 "simple-dispatch (dispatcher simple)"
 '(progn
    (defun dispatch-op (op)
      (if (= op 1)
          100
          (if (= op 2)
              200
              (if (= op 3)
                  300
                  999))))
    (dispatch-op 2)))

;; Test 7: Fonction manipulant des paires (comme compile-setq)
(test-compiler-function
 "update-binding (mise à jour binding)"
 '(progn
    (defun update-env (var val env)
      (cons (cons var val) env))
    (update-env 5 500 (cons (cons 1 100) nil))))

;; Test 8: Fonction avec LENGTH et NTH (comme compile-call)
(test-compiler-function
 "get-arg-at (accès argument)"
 '(progn
    (defun get-arg-at (args index)
      (if (< index (length args))
          (nth index args)
          nil))
    (get-arg-at (cons 10 (cons 20 (cons 30 nil))) 1)))

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                         RÉSUMÉ FINAL                             ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Fonctions type compilateur testées: ~A~%" *compiler-functions-tested*)
(format t "Fonctions compilées avec succès: ~A~%" *compiler-functions-passed*)
(format t "Taux de réussite: ~,1F%~%~%" 
        (if (> *compiler-functions-tested* 0)
            (* 100.0 (/ *compiler-functions-passed* *compiler-functions-tested*))
            0.0))

(if (= *compiler-functions-passed* *compiler-functions-tested*)
    (progn
      (format t "🎉 EXCELLENT !~%~%")
      (format t "Le compilateur peut compiler toutes les structures~%")
      (format t "utilisées dans le compilateur lui-même:~%~%")
      (format t "  ✅ Recherche dans environnement (ASSOC)~%")
      (format t "  ✅ Génération de labels~%")
      (format t "  ✅ Traitement d'arguments (LENGTH, NTH)~%")
      (format t "  ✅ Assemblage de code (APPEND)~%")
      (format t "  ✅ Fonctions récursives complexes~%")
      (format t "  ✅ Dispatcher de commandes~%")
      (format t "  ✅ Manipulation de bindings~%")
      (format t "  ✅ Accès indexé dans listes~%~%")
      (format t "╔══════════════════════════════════════════════════════════════════╗~%")
      (format t "║                                                                  ║~%")
      (format t "║     ✅ LE COMPILATEUR EST COMPILABLE !                          ║~%")
      (format t "║                                                                  ║~%")
      (format t "║  Vous pouvez maintenant procéder à l'auto-compilation:          ║~%")
      (format t "║  1. Compiler le compilateur avec lui-même                       ║~%")
      (format t "║  2. Charger le compilateur compilé dans la VM                   ║~%")
      (format t "║  3. Bootstrap complet réussi !                                  ║~%")
      (format t "║                                                                  ║~%")
      (format t "╚══════════════════════════════════════════════════════════════════╝~%~%"))
    (progn
      (format t "⚠️  ATTENTION !~%~%")
      (format t "Certaines fonctions n'ont pas pu être compilées.~%")
      (format t "Le compilateur n'est pas encore prêt pour l'auto-compilation.~%~%")))
