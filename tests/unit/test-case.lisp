;;;; test-case.lisp
;;;; Tests pour la structure CASE (pattern matching)

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(defun test-case-simple ()
  "Test 1: CASE simple avec valeurs uniques"
  (format t "~%=== Test 1: CASE simple ===~%")
  (let* ((code '(let ((x 2))
                  (case x
                    (1 10)
                    (2 20)
                    (3 30))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 20)~%" result)
    (if (= result 20)
        (progn (format t "✓ Test 1 PASSÉ~%") t)
        (progn (format t "✗ Test 1 ÉCHOUÉ~%") nil))))

(defun test-case-list-keys ()
  "Test 2: CASE avec liste de keys"
  (format t "~%=== Test 2: CASE avec liste de keys ===~%")
  (let* ((code '(let ((n 4))
                  (case n
                    (1 100)
                    ((2 3 4) 200)
                    (5 300))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 200)~%" result)
    (if (= result 200)
        (progn (format t "✓ Test 2 PASSÉ~%") t)
        (progn (format t "✗ Test 2 ÉCHOUÉ~%") nil))))

(defun test-case-otherwise ()
  "Test 3: CASE avec clause otherwise"
  (format t "~%=== Test 3: CASE avec otherwise ===~%")
  (let* ((code '(let ((val 99))
                  (case val
                    (1 10)
                    (2 20)
                    (otherwise 999))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 999)~%" result)
    (if (= result 999)
        (progn (format t "✓ Test 3 PASSÉ~%") t)
        (progn (format t "✗ Test 3 ÉCHOUÉ~%") nil))))

(defun test-case-first-match ()
  "Test 4: CASE prend la première correspondance"
  (format t "~%=== Test 4: CASE première correspondance ===~%")
  (let* ((code '(let ((x 3))
                  (case x
                    ((1 2 3) 111)
                    ((3 4 5) 222)
                    (otherwise 333))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 111)~%" result)
    (if (= result 111)
        (progn (format t "✓ Test 4 PASSÉ~%") t)
        (progn (format t "✗ Test 4 ÉCHOUÉ~%") nil))))

(defun test-case-with-arithmetic ()
  "Test 5: CASE avec expressions arithmétiques"
  (format t "~%=== Test 5: CASE avec arithmétique ===~%")
  (let* ((code '(let ((day 3))
                  (case day
                    (1 (* 100 1))
                    (2 (* 100 2))
                    (3 (* 100 3))
                    (otherwise 0))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 300)~%" result)
    (if (= result 300)
        (progn (format t "✓ Test 5 PASSÉ~%") t)
        (progn (format t "✗ Test 5 ÉCHOUÉ~%") nil))))

(defun test-case-nested ()
  "Test 6: CASE imbriqué"
  (format t "~%=== Test 6: CASE imbriqué ===~%")
  (let* ((code '(let ((x 2) (y 1))
                  (case x
                    (1 10)
                    (2 (case y
                         (1 21)
                         (2 22)
                         (otherwise 29)))
                    (otherwise 99))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 21)~%" result)
    (if (= result 21)
        (progn (format t "✓ Test 6 PASSÉ~%") t)
        (progn (format t "✗ Test 6 ÉCHOUÉ~%") nil))))

(defun test-case-zero ()
  "Test 7: CASE avec 0 comme key"
  (format t "~%=== Test 7: CASE avec 0 ===~%")
  (let* ((code '(let ((n 0))
                  (case n
                    (0 777)
                    (1 888)
                    (otherwise 999))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 777)~%" result)
    (if (= result 777)
        (progn (format t "✓ Test 7 PASSÉ~%") t)
        (progn (format t "✗ Test 7 ÉCHOUÉ~%") nil))))

(defun test-case-negative ()
  "Test 8: CASE avec nombres négatifs"
  (format t "~%=== Test 8: CASE avec négatifs ===~%")
  (let* ((code '(let ((temp -5))
                  (case temp
                    (-10 1)
                    (-5 2)
                    (0 3)
                    (5 4)
                    (otherwise 0))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 2)~%" result)
    (if (= result 2)
        (progn (format t "✓ Test 8 PASSÉ~%") t)
        (progn (format t "✗ Test 8 ÉCHOUÉ~%") nil))))

(defun run-all-case-tests ()
  "Exécute tous les tests CASE et affiche le récapitulatif"
  (format t "~%╔═══════════════════════════════════════╗~%")
  (format t "║   TESTS STRUCTURE CASE                ║~%")
  (format t "║   (Pattern Matching)                  ║~%")
  (format t "╚═══════════════════════════════════════╝~%")
  
  (let ((results (list
                  (test-case-simple)
                  (test-case-list-keys)
                  (test-case-otherwise)
                  (test-case-first-match)
                  (test-case-with-arithmetic)
                  (test-case-nested)
                  (test-case-zero)
                  (test-case-negative))))
    
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
(run-all-case-tests)
