;;;; test-arrays.lisp
;;;; Tests unitaires pour les ARRAYS (PHASE 11)
;;;; 
;;;; Teste make-array, aref, et setf aref

(load "main.lisp")

(defparameter *test-count* 0)
(defparameter *test-passed* 0)

(defun test-array (test-name code expected-result)
  "Exécute un test de code compilé LISP et vérifie le résultat"
  (incf *test-count*)
  (format t "~%Test ~A: ~A~%" *test-count* test-name)
  (format t "  Code: ~S~%" code)
  (format t "  Résultat attendu: ~A~%" expected-result)
  
  (handler-case
      (let* ((vm (make-new-vm))
             (asm-code (compile-lisp code))
             (asm-with-halt (append asm-code (list (list :HALT)))))
        ;; Réinitialiser heap (variable globale ET registre $gp)
        (reset-heap)
        (set-register vm (get-reg :gp) +heap-start+)
        
        ;; Exécuter
        (load-and-run vm asm-with-halt)
        (let ((result (get-register vm *reg-v0*)))
          (format t "  Résultat obtenu: ~A~%" result)
          
          (if (= result expected-result)
              (progn
                (incf *test-passed*)
                (format t "  ✓ PASSÉ~%"))
              (format t "  ✗ ÉCHOUÉ~%"))))
    (error (e)
      (format t "  ✗ ERREUR: ~A~%" e))))

(format t "~%")
(format t "========================================~%")
(format t "TESTS ARRAYS - PHASE 11~%")
(format t "========================================~%")

;; Test 1: Créer array simple
(test-array "Créer array de taille 5"
            '(let ((arr (make-array 5)))
               arr)
            ;; Devrait retourner l'adresse (debut du heap)
            +heap-start+)  ; Dépend de la configuration mémoire

;; Test 2: Créer et lire élément (valeur par défaut 0)
(test-array "Lire élément par défaut (sans init)"
            '(let ((arr (make-array 5)))
               (aref arr 0))
            0)

;; Test 3: Créer avec :initial-element
(test-array "Créer array avec :initial-element 42"
            '(let ((arr (make-array 3 :initial-element 42)))
               (aref arr 0))
            42)

;; Test 4: Lire différents indices avec :initial-element
(test-array "Lire différents indices (init 99)"
            '(let ((arr (make-array 4 :initial-element 99)))
               (aref arr 2))
            99)

;; Test 5: Écrire avec setf aref
(test-array "Écrire avec setf aref"
            '(let ((arr (make-array 5)))
               (setq (aref arr 0) 123)
               (aref arr 0))
            123)

;; Test 6: Écrire et lire différents indices
(test-array "Écrire index 3, lire index 3"
            '(let ((arr (make-array 10)))
               (setq (aref arr 3) 456)
               (aref arr 3))
            456)

;; Test 7: Vérifier indépendance des éléments
(test-array "Éléments indépendants"
            '(let ((arr (make-array 5)))
               (setq (aref arr 0) 10)
               (setq (aref arr 1) 20)
               (setq (aref arr 2) 30)
               (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))
            60)

;; Test 8: Boucle WHILE avec array
(test-array "Boucle WHILE remplissage array"
            '(let ((arr (make-array 5)))
               (let ((i 0))
                 (while (< i 5)
                   (progn
                     (setq (aref arr i) (* i 10))
                     (setq i (+ i 1)))))
               (aref arr 3))
            30)

;; Test 9: Accumuler valeurs d'un array
(test-array "Accumuler valeurs array avec WHILE"
            '(let ((arr (make-array 4 :initial-element 5)))
               (let ((i 0) (sum 0))
                 (while (< i 4)
                   (progn
                     (setq sum (+ sum (aref arr i)))
                     (setq i (+ i 1))))
                 sum))
            20)

;; Test 10: Array avec calculs complexes
(test-array "Array avec calculs (carrés)"
            '(let ((arr (make-array 4)))
               (let ((i 0))
                 (while (< i 4)
                   (progn
                     (setq (aref arr i) (* i i))
                     (setq i (+ i 1)))))
               ;; Somme des carrés: 0 + 1 + 4 + 9 = 14
               (let ((sum 0) (j 0))
                 (while (< j 4)
                   (progn
                     (setq sum (+ sum (aref arr j)))
                     (setq j (+ j 1))))
                 sum))
            14)

;; Test 11: Plusieurs arrays
(test-array "Créer plusieurs arrays indépendants"
            '(let ((arr1 (make-array 2 :initial-element 10)))
               (let ((arr2 (make-array 2 :initial-element 20)))
                 (+ (aref arr1 0) (aref arr2 0))))
            30)

;; Test 12: setf retourne la valeur
(test-array "SETF AREF retourne la valeur assignée"
            '(let ((arr (make-array 3)))
               (setq (aref arr 1) 777))
            777)

(format t "~%")
(format t "========================================~%")
(format t "RÉSULTATS: ~A/~A tests passés~%" *test-passed* *test-count*)
(format t "========================================~%")

(if (= *test-passed* *test-count*)
    (format t "~%✅ TOUS LES TESTS ARRAYS PASSENT ! 🎉~%~%")
    (format t "~%❌ Certains tests ont échoué.~%~%"))
