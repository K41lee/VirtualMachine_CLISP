;;;; test-logical.lisp
;;;; Tests pour les opérateurs logiques NOT, AND, OR

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(defun test-not-true ()
  "Test 1: NOT sur valeur vraie"
  (format t "~%=== Test 1: NOT sur vrai ===~%")
  (let* ((code '(not (< 3 5)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 0)~%" result)
    (if (= result 0)
        (progn (format t "✓ Test 1 PASSÉ~%") t)
        (progn (format t "✗ Test 1 ÉCHOUÉ~%") nil))))

(defun test-not-false ()
  "Test 2: NOT sur valeur fausse"
  (format t "~%=== Test 2: NOT sur faux ===~%")
  (let* ((code '(not (> 3 5)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 1)~%" result)
    (if (= result 1)
        (progn (format t "✓ Test 2 PASSÉ~%") t)
        (progn (format t "✗ Test 2 ÉCHOUÉ~%") nil))))

(defun test-and-all-true ()
  "Test 3: AND avec toutes valeurs vraies"
  (format t "~%=== Test 3: AND toutes vraies ===~%")
  (let* ((code '(and (< 2 5) (< 3 7) (< 4 10)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 1)~%" result)
    (if (= result 1)
        (progn (format t "✓ Test 3 PASSÉ~%") t)
        (progn (format t "✗ Test 3 ÉCHOUÉ~%") nil))))

(defun test-and-one-false ()
  "Test 4: AND avec une valeur fausse (court-circuit)"
  (format t "~%=== Test 4: AND avec un faux ===~%")
  (let* ((code '(and (< 2 5) (> 3 7) (< 4 10)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 0)~%" result)
    (if (= result 0)
        (progn (format t "✓ Test 4 PASSÉ~%") t)
        (progn (format t "✗ Test 4 ÉCHOUÉ~%") nil))))

(defun test-and-empty ()
  "Test 5: AND sans arguments"
  (format t "~%=== Test 5: AND vide ===~%")
  (let* ((code '(and))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 1)~%" result)
    (if (= result 1)
        (progn (format t "✓ Test 5 PASSÉ~%") t)
        (progn (format t "✗ Test 5 ÉCHOUÉ~%") nil))))

(defun test-or-all-false ()
  "Test 6: OR avec toutes valeurs fausses"
  (format t "~%=== Test 6: OR toutes fausses ===~%")
  (let* ((code '(or (> 2 5) (> 3 7) (> 4 10)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 0)~%" result)
    (if (= result 0)
        (progn (format t "✓ Test 6 PASSÉ~%") t)
        (progn (format t "✗ Test 6 ÉCHOUÉ~%") nil))))

(defun test-or-one-true ()
  "Test 7: OR avec une valeur vraie (court-circuit)"
  (format t "~%=== Test 7: OR avec un vrai ===~%")
  (let* ((code '(or (> 2 5) (< 3 7) (> 4 10)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 1)~%" result)
    (if (= result 1)
        (progn (format t "✓ Test 7 PASSÉ~%") t)
        (progn (format t "✗ Test 7 ÉCHOUÉ~%") nil))))

(defun test-or-empty ()
  "Test 8: OR sans arguments"
  (format t "~%=== Test 8: OR vide ===~%")
  (let* ((code '(or))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 0)~%" result)
    (if (= result 0)
        (progn (format t "✓ Test 8 PASSÉ~%") t)
        (progn (format t "✗ Test 8 ÉCHOUÉ~%") nil))))

(defun test-combined-logic ()
  "Test 9: Combinaison de NOT, AND, OR"
  (format t "~%=== Test 9: Logique combinée ===~%")
  (let* ((code '(and (not (> 5 10)) 
                     (or (< 3 5) (> 7 10))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 1)~%" result)
    (if (= result 1)
        (progn (format t "✓ Test 9 PASSÉ~%") t)
        (progn (format t "✗ Test 9 ÉCHOUÉ~%") nil))))

(defun test-logic-with-if ()
  "Test 10: Logique avec IF"
  (format t "~%=== Test 10: Logique avec IF ===~%")
  (let* ((code '(let ((x 7))
                  (if (and (< x 10) (> x 5))
                      100
                      200)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 100)~%" result)
    (if (= result 100)
        (progn (format t "✓ Test 10 PASSÉ~%") t)
        (progn (format t "✗ Test 10 ÉCHOUÉ~%") nil))))

(defun run-all-logical-tests ()
  "Exécute tous les tests logiques et affiche le récapitulatif"
  (format t "~%╔═══════════════════════════════════════╗~%")
  (format t "║   TESTS OPÉRATEURS LOGIQUES           ║~%")
  (format t "║   (NOT, AND, OR)                      ║~%")
  (format t "╚═══════════════════════════════════════╝~%")
  
  (let ((results (list
                  (test-not-true)
                  (test-not-false)
                  (test-and-all-true)
                  (test-and-one-false)
                  (test-and-empty)
                  (test-or-all-false)
                  (test-or-one-true)
                  (test-or-empty)
                  (test-combined-logic)
                  (test-logic-with-if))))
    
    (let ((passed (count t results))
          (total (length results)))
      (format t "~%╔═══════════════════════════════════════╗~%")
      (format t "║   RÉCAPITULATIF                       ║~%")
      (format t "╠═══════════════════════════════════════╣~%")
      (format t "║   Tests réussis: ~A/~A~18T║~%" passed total)
      (format t "║   Taux de réussite: ~A%~17T║~%" 
              (floor (* 100 (/ passed total))))
      (format t "╚═══════════════════════════════════════╝~%")
      
      (= passed total))))

;; Lancer les tests
(run-all-logical-tests)
