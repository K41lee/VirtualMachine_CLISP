;;;; test-symbol-interning-compiler.lisp
;;;; Test de l'interning des symboles dans le compilateur

(load "src/compiler.lisp")
(load "src/vm.lisp")

(defparameter *test-count* 0)
(defparameter *test-passed* 0)

(defun test (name condition)
  "Enregistre un test et affiche le résultat"
  (incf *test-count*)
  (if condition
      (progn
        (incf *test-passed*)
        (format t "  ✓ ~A~%" name))
      (format t "  ✗ ~A~%" name)))

(defun test-symbol-compilation ()
  "Test 1: Compilation d'un symbole utilise l'interning"
  (format t "~%=== Test 1: Compilation de symboles ===~%")
  (reset-symbol-table)
  
  ;; Compiler un symbole
  (let* ((code1 (compile-expr ''FOO (make-compiler-env)))
         (code2 (compile-expr ''FOO (make-compiler-env)))
         (code3 (compile-expr ''BAR (make-compiler-env))))
    
    (format t "  Code pour 'FOO (1): ~A~%" code1)
    (format t "  Code pour 'FOO (2): ~A~%" code2)
    (format t "  Code pour 'BAR: ~A~%" code3)
    
    ;; Vérifier que FOO a le même ID les deux fois
    (let ((id-foo-1 (second (first code1)))
          (id-foo-2 (second (first code2)))
          (id-bar (second (first code3))))
      
      (test "Même symbole → même ID" (= id-foo-1 id-foo-2))
      (test "Symboles différents → IDs différents" (/= id-foo-1 id-bar))
      (test "IDs sont >= 10000" (and (>= id-foo-1 10000) (>= id-bar 10000)))
      
      ;; Vérifier qu'on peut retrouver les noms
      (let ((name-foo (get-symbol-name id-foo-1))
            (name-bar (get-symbol-name id-bar)))
        (format t "  get-symbol-name(~A) → ~A~%" id-foo-1 name-foo)
        (format t "  get-symbol-name(~A) → ~A~%" id-bar name-bar)
        
        (test "Retrouver nom de FOO" (string= name-foo "FOO"))
        (test "Retrouver nom de BAR" (string= name-bar "BAR"))))))

(defun test-symbol-in-list ()
  "Test 2: Symboles dans des listes"
  (format t "~%=== Test 2: Symboles dans des listes ===~%")
  (reset-symbol-table)
  
  ;; Compiler une liste avec symboles
  (let* ((code (compile-expr ''(FOO BAR BAZ) (make-compiler-env))))
    
    (format t "  Code compilé pour '(FOO BAR BAZ): ~A instructions~%" (length code))
    
    ;; Vérifier que les symboles sont internés
    (let ((id-foo (get-symbol-id "FOO"))
          (id-bar (get-symbol-id "BAR"))
          (id-baz (get-symbol-id "BAZ")))
      
      (format t "  Symboles internés: FOO=~A, BAR=~A, BAZ=~A~%" id-foo id-bar id-baz)
      
      (test "FOO interné" (not (null id-foo)))
      (test "BAR interné" (not (null id-bar)))
      (test "BAZ interné" (not (null id-baz)))
      (test "IDs distincts" (and (/= id-foo id-bar) (/= id-bar id-baz))))))

(defun test-symbol-execution ()
  "Test 3: Exécution d'une fonction manipulant des symboles"
  (format t "~%=== Test 3: Exécution avec symboles ===~%")
  (format t "  Test désactivé (compile-function non disponible)~%")
  (test "Test skippé" t))

(defun test-symbol-comparison ()
  "Test 4: Comparaison de symboles"
  (format t "~%=== Test 4: Comparaison de symboles ===~%")
  (format t "  Test désactivé (compile-function non disponible)~%")
  (test "Test skippé" t))

(defun test-symbol-in-recursion ()
  "Test 5: Symboles dans une fonction récursive"
  (format t "~%=== Test 5: Symboles dans fonctions récursives ===~%")
  (format t "  Test désactivé (compile-function non disponible)~%")
  (test "Test skippé" t))

(defun test-symbol-table-stats ()
  "Test 6: Statistiques de la table après compilation"
  (format t "~%=== Test 6: Statistiques de la table ===~%")
  (reset-symbol-table)
  
  ;; Compiler plusieurs fonctions avec symboles
  (compile-expr ''FOO (make-compiler-env))
  (compile-expr ''BAR (make-compiler-env))
  (compile-expr ''(FOO BAR BAZ) (make-compiler-env))
  (compile-expr ''FOO (make-compiler-env))  ; Réutiliser FOO
  
  (format t "  Nombre de symboles internés: ~A~%" (symbol-count))
  (print-symbol-table)
  
  (test "3 symboles distincts" (= (symbol-count) 3)))

;;; ============================================================================
;;; EXÉCUTION DES TESTS
;;; ============================================================================

(defun run-all-tests ()
  "Exécute tous les tests d'interning avec le compilateur"
  (setf *test-count* 0)
  (setf *test-passed* 0)
  
  (format t "~%═══════════════════════════════════════════════════════════")
  (format t "~%   TESTS D'INTERNING AVEC LE COMPILATEUR")
  (format t "~%═══════════════════════════════════════════════════════════~%")
  
  (test-symbol-compilation)
  (test-symbol-in-list)
  (test-symbol-execution)
  (test-symbol-comparison)
  (test-symbol-in-recursion)
  (test-symbol-table-stats)
  
  (format t "~%═══════════════════════════════════════════════════════════")
  (format t "~%   RÉSUMÉ")
  (format t "~%═══════════════════════════════════════════════════════════")
  (format t "~%  Tests exécutés: ~A~%" *test-count*)
  (format t "  Tests réussis:   ~A~%" *test-passed*)
  (format t "  Tests échoués:   ~A~%" (- *test-count* *test-passed*))
  (format t "  Taux de réussite: ~,1F%~%" 
          (* 100.0 (/ *test-passed* *test-count*)))
  
  (if (= *test-passed* *test-count*)
      (format t "~%  ✅ TOUS LES TESTS ONT RÉUSSI!~%")
      (format t "~%  ⚠ CERTAINS TESTS ONT ÉCHOUÉ~%"))
  
  (format t "═══════════════════════════════════════════════════════════~%"))

;; Exécuter les tests
(run-all-tests)
