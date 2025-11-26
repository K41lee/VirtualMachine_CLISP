;;;; test-when-unless.lisp
;;;; Tests pour les structures WHEN et UNLESS

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(defun test-when-true ()
  "Test 1: WHEN avec condition vraie"
  (format t "~%=== Test 1: WHEN condition vraie ===~%")
  (let* ((code '(when (< 3 5) 100))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 100)~%" result)
    (if (= result 100)
        (progn (format t "✓ Test 1 PASSÉ~%") t)
        (progn (format t "✗ Test 1 ÉCHOUÉ~%") nil))))

(defun test-when-false ()
  "Test 2: WHEN avec condition fausse (devrait donner 0/nil)"
  (format t "~%=== Test 2: WHEN condition fausse ===~%")
  (let* ((code '(when (> 3 5) 100))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 0 ou valeur non définie)~%" result)
    ;; WHEN avec test faux ne devrait pas évaluer le body
    ;; Le registre garde sa valeur précédente (test = 0)
    (if (= result 0)
        (progn (format t "✓ Test 2 PASSÉ~%") t)
        (progn (format t "✗ Test 2 ÉCHOUÉ~%") nil))))

(defun test-when-multiple-exprs ()
  "Test 3: WHEN avec plusieurs expressions dans le body"
  (format t "~%=== Test 3: WHEN avec plusieurs expressions ===~%")
  (let* ((code '(let ((x 5))
                  (when (< x 10)
                    (setq x (* x 2))
                    (+ x 100))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 110)~%" result)
    (if (= result 110)
        (progn (format t "✓ Test 3 PASSÉ~%") t)
        (progn (format t "✗ Test 3 ÉCHOUÉ~%") nil))))

(defun test-unless-true ()
  "Test 4: UNLESS avec condition vraie (ne devrait pas exécuter)"
  (format t "~%=== Test 4: UNLESS condition vraie ===~%")
  (let* ((code '(unless (< 3 5) 200))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 1 - valeur du test)~%" result)
    ;; UNLESS avec test vrai ne devrait pas exécuter le body
    (if (= result 1)
        (progn (format t "✓ Test 4 PASSÉ~%") t)
        (progn (format t "✗ Test 4 ÉCHOUÉ~%") nil))))

(defun test-unless-false ()
  "Test 5: UNLESS avec condition fausse (devrait exécuter)"
  (format t "~%=== Test 5: UNLESS condition fausse ===~%")
  (let* ((code '(unless (> 3 5) 300))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 300)~%" result)
    (if (= result 300)
        (progn (format t "✓ Test 5 PASSÉ~%") t)
        (progn (format t "✗ Test 5 ÉCHOUÉ~%") nil))))

(defun test-unless-multiple-exprs ()
  "Test 6: UNLESS avec plusieurs expressions"
  (format t "~%=== Test 6: UNLESS avec plusieurs expressions ===~%")
  (let* ((code '(let ((y 8))
                  (unless (> y 10)
                    (setq y (+ y 5))
                    (* y 10))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 130)~%" result)
    (if (= result 130)
        (progn (format t "✓ Test 6 PASSÉ~%") t)
        (progn (format t "✗ Test 6 ÉCHOUÉ~%") nil))))

(defun test-when-unless-nested ()
  "Test 7: WHEN et UNLESS imbriqués"
  (format t "~%=== Test 7: WHEN et UNLESS imbriqués ===~%")
  (let* ((code '(let ((n 7))
                  (when (< n 10)
                    (unless (< n 5)
                      (+ n 50)))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 57)~%" result)
    (if (= result 57)
        (progn (format t "✓ Test 7 PASSÉ~%") t)
        (progn (format t "✗ Test 7 ÉCHOUÉ~%") nil))))

(defun run-all-when-unless-tests ()
  "Exécute tous les tests WHEN/UNLESS et affiche le récapitulatif"
  (format t "~%╔═══════════════════════════════════════╗~%")
  (format t "║   TESTS WHEN/UNLESS                   ║~%")
  (format t "╚═══════════════════════════════════════╝~%")
  
  (let ((results (list
                  (test-when-true)
                  (test-when-false)
                  (test-when-multiple-exprs)
                  (test-unless-true)
                  (test-unless-false)
                  (test-unless-multiple-exprs)
                  (test-when-unless-nested))))
    
    (let ((passed (count t results))
          (total (length results)))
      (format t "~%╔═══════════════════════════════════════╗~%")
      (format t "║   RÉCAPITULATIF                       ║~%")
      (format t "╠═══════════════════════════════════════╣~%")
      (format t "║   Tests réussis: ~A/~A~19T║~%" passed total)
      (format t "║   Taux de réussite: ~A%~17T║~%" 
              (floor (* 100 (/ passed total))))
      (format t "╚═══════════════════════════════════════╝~%")
      
      (= passed total))))

;; Lancer les tests
(run-all-when-unless-tests)
