;;;; test-cond.lisp
;;;; Tests pour la structure COND

(load "loader.lisp")
(load "compiler.lisp")
(load "vm.lisp")

(defun test-cond-simple ()
  "Test 1: COND simple avec 3 cas"
  (format t "~%=== Test 1: COND simple ===~%")
  (let* ((code '(cond
                  ((< 5 3) 10)
                  ((< 5 7) 20)
                  (t 30)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 20)~%" result)
    (if (= result 20)
        (progn (format t "✓ Test 1 PASSÉ~%") t)
        (progn (format t "✗ Test 1 ÉCHOUÉ~%") nil))))

(defun test-cond-default ()
  "Test 2: COND avec clause par défaut (t)"
  (format t "~%=== Test 2: COND avec clause par défaut ===~%")
  (let* ((code '(cond
                  ((< 10 5) 100)
                  ((< 10 7) 200)
                  (t 300)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 300)~%" result)
    (if (= result 300)
        (progn (format t "✓ Test 2 PASSÉ~%") t)
        (progn (format t "✗ Test 2 ÉCHOUÉ~%") nil))))

(defun test-cond-first-match ()
  "Test 3: COND prend la première clause vraie"
  (format t "~%=== Test 3: COND première clause vraie ===~%")
  (let* ((code '(cond
                  ((< 2 5) 111)
                  ((< 3 5) 222)
                  ((< 4 5) 333)))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 111)~%" result)
    (if (= result 111)
        (progn (format t "✓ Test 3 PASSÉ~%") t)
        (progn (format t "✗ Test 3 ÉCHOUÉ~%") nil))))

(defun test-cond-with-arithmetic ()
  "Test 4: COND avec expressions arithmétiques"
  (format t "~%=== Test 4: COND avec arithmétique ===~%")
  (let* ((code '(let ((x 7))
                  (cond
                    ((< x 5) (* x 2))
                    ((< x 10) (+ x 100))
                    (t (- x 50)))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 107)~%" result)
    (if (= result 107)
        (progn (format t "✓ Test 4 PASSÉ~%") t)
        (progn (format t "✗ Test 4 ÉCHOUÉ~%") nil))))

(defun test-cond-nested ()
  "Test 5: COND imbriqué dans IF"
  (format t "~%=== Test 5: COND imbriqué ===~%")
  (let* ((code '(if (< 5 10)
                    (cond
                      ((< 3 2) 1)
                      ((< 4 3) 2)
                      (t 3))
                    999))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 3)~%" result)
    (if (= result 3)
        (progn (format t "✓ Test 5 PASSÉ~%") t)
        (progn (format t "✗ Test 5 ÉCHOUÉ~%") nil))))

(defun test-cond-equality ()
  "Test 6: COND avec tests d'égalité"
  (format t "~%=== Test 6: COND avec égalité ===~%")
  (let* ((code '(let ((n 5))
                  (cond
                    ((= n 3) 30)
                    ((= n 4) 40)
                    ((= n 5) 50)
                    (t 0))))
         (vm (compile-and-run code))
         (result (get-register vm *reg-v0*)))
    (format t "Expression: ~A~%" code)
    (format t "Résultat: ~A (attendu: 50)~%" result)
    (if (= result 50)
        (progn (format t "✓ Test 6 PASSÉ~%") t)
        (progn (format t "✗ Test 6 ÉCHOUÉ~%") nil))))

(defun run-all-cond-tests ()
  "Exécute tous les tests COND et affiche le récapitulatif"
  (format t "~%╔═══════════════════════════════════════╗~%")
  (format t "║   TESTS STRUCTURE COND                ║~%")
  (format t "╚═══════════════════════════════════════╝~%")
  
  (let ((results (list
                  (test-cond-simple)
                  (test-cond-default)
                  (test-cond-first-match)
                  (test-cond-with-arithmetic)
                  (test-cond-nested)
                  (test-cond-equality))))
    
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
(run-all-cond-tests)
