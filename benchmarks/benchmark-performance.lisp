;;;; ============================================================================
;;;; BENCHMARK DE PERFORMANCE - Comparaison CLISP vs VM0 vs VM1+VM2
;;;; ============================================================================
;;;; Compare 3 modes d'exécution :
;;;; 1. CLISP natif (interpréteur CLISP)
;;;; 2. VM0 (code MIPS interprété sur VM écrite en LISP)
;;;; 3. VM1+VM2 (code MIPS compilé puis exécuté sur VM compilée)

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(defparameter *benchmark-results* '())

;;; ============================================================================
;;; UTILITAIRES DE TIMING
;;; ============================================================================

(defun measure-time (fn)
  "Mesure le temps d'exécution d'une fonction (en secondes)"
  (let ((start (get-internal-real-time)))
    (funcall fn)
    (let ((end (get-internal-real-time)))
      (/ (- end start) internal-time-units-per-second))))

(defun format-time (seconds)
  "Formate le temps en ms ou s selon la magnitude"
  (if (< seconds 0.001)
      (format nil "~,3f µs" (* seconds 1000000))
      (if (< seconds 1.0)
          (format nil "~,3f ms" (* seconds 1000))
          (format nil "~,3f s" seconds))))

;;; ============================================================================
;;; BENCHMARKS - Définitions
;;; ============================================================================

(defparameter *benchmarks* 
  '(
    ;; Test 1: Fibonacci récursif (complexité exponentielle)
    (:name "Fibonacci(20)"
     :description "Calcul récursif de Fibonacci(20)"
     :lisp-expr (progn
                  (defun fib (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1)) (fib (- n 2)))))
                  (fib 20))
     :expected 6765)
    
    ;; Test 2: Fibonacci plus petit (pour VM qui est lente)
    (:name "Fibonacci(15)"
     :description "Calcul récursif de Fibonacci(15)"
     :lisp-expr (progn
                  (defun fib (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1)) (fib (- n 2)))))
                  (fib 15))
     :expected 610)
    
    ;; Test 3: Factorielle récursive
    (:name "Factorielle(10)"
     :description "Calcul récursif de 10!"
     :lisp-expr (progn
                  (defun fact (n)
                    (if (<= n 1)
                        1
                        (* n (fact (- n 1)))))
                  (fact 10))
     :expected 3628800)
    
    ;; Test 4: Somme itérative (boucle WHILE)
    (:name "Somme(1..1000)"
     :description "Somme des entiers de 1 à 1000"
     :lisp-expr (let ((sum 0)
                      (i 1))
                  (while (<= i 1000)
                    (setq sum (+ sum i))
                    (setq i (+ i 1)))
                  sum)
     :expected 500500)
    
    ;; Test 5: Puissance itérative
    (:name "Puissance(2^20)"
     :description "Calcul de 2^20 par multiplication itérative"
     :lisp-expr (let ((result 1)
                      (i 0))
                  (while (< i 20)
                    (setq result (* result 2))
                    (setq i (+ i 1)))
                  result)
     :expected 1048576)
    
    ;; Test 6: Ackermann (très complexe)
    (:name "Ackermann(3,4)"
     :description "Fonction d'Ackermann A(3,4)"
     :lisp-expr (progn
                  (defun ack (m n)
                    (cond
                      ((= m 0) (+ n 1))
                      ((= n 0) (ack (- m 1) 1))
                      (t (ack (- m 1) (ack m (- n 1))))))
                  (ack 3 4))
     :expected 125)
    
    ;; Test 7: Manipulation de tableaux
    (:name "Array-Sum(100)"
     :description "Création et somme d'un tableau de 100 éléments"
     :lisp-expr (let ((arr (make-array 100 :initial-element 0))
                      (sum 0)
                      (i 0))
                  ;; Remplir le tableau
                  (while (< i 100)
                    (setq (aref arr i) i)
                    (setq i (+ i 1)))
                  ;; Calculer la somme
                  (setq i 0)
                  (while (< i 100)
                    (setq sum (+ sum (aref arr i)))
                    (setq i (+ i 1)))
                  sum)
     :expected 4950)
    
    ;; Test 8: Arithmétique complexe
    (:name "Arithmétique"
     :description "Opérations arithmétiques complexes"
     :lisp-expr (let ((a 123)
                      (b 456)
                      (c 789))
                  (+ (* a b) (- c (* a 2))))
     :expected 56635)
    ))

;;; ============================================================================
;;; EXECUTION - Mode 1: CLISP Natif
;;; ============================================================================

(defun run-benchmark-clisp (expr expected)
  "Exécute un benchmark en CLISP natif"
  (let ((result nil)
        (time nil))
    (handler-case
        (progn
          (setf time (measure-time (lambda () (setf result (eval expr)))))
          (if (= result expected)
              (list :success t :result result :time time)
              (list :success nil :result result :expected expected :time time)))
      (error (e)
        (list :success nil :error (format nil "~A" e) :time 0)))))

;;; ============================================================================
;;; EXECUTION - Mode 2: VM0 (interprétée)
;;; ============================================================================

(defun run-benchmark-vm0 (expr expected)
  "Exécute un benchmark sur VM0 (code MIPS interprété)"
  (let ((result nil)
        (time nil))
    (handler-case
        (let ((code (compile-lisp expr))
              (vm nil))
          (setf time (measure-time 
                      (lambda ()
                        (setf vm (make-new-vm :verbose nil))
                        (load-code vm (append code (list (list :HALT))))
                        (run-vm vm :max-instructions 10000000))))
          (setf result (get-register vm :$V0))
          (if (= result expected)
              (list :success t :result result :time time :instructions (gethash :instruction-count vm))
              (list :success nil :result result :expected expected :time time)))
      (error (e)
        (list :success nil :error (format nil "~A" e) :time 0)))))

;;; ============================================================================
;;; EXECUTION - Mode 3: VM1+VM2 (compilée)
;;; ============================================================================
;;; Note: Pour l'instant, on n'a pas VM1 compilée, donc ce mode sera similaire à VM0
;;; mais préparé pour l'avenir

(defun run-benchmark-vm1-vm2 (expr expected)
  "Exécute un benchmark sur VM1+VM2 (code MIPS compilé sur VM compilée)"
  ;; Pour l'instant, identique à VM0 (en attendant d'avoir VM1 compilée)
  ;; TODO: Compiler le code de la VM elle-même et l'exécuter
  (run-benchmark-vm0 expr expected))

;;; ============================================================================
;;; AFFICHAGE DES RESULTATS
;;; ============================================================================

(defun print-benchmark-header ()
  (format t "~%╔════════════════════════════════════════════════════════════════════════════╗~%")
  (format t "║  BENCHMARK DE PERFORMANCE - CLISP vs VM0 vs VM1+VM2                       ║~%")
  (format t "╚════════════════════════════════════════════════════════════════════════════╝~%~%"))

(defun print-benchmark-result (name description result-clisp result-vm0 result-vm1)
  (format t "~%─────────────────────────────────────────────────────────────────────────────~%")
  (format t "Test: ~A~%" name)
  (format t "Description: ~A~%" description)
  (format t "─────────────────────────────────────────────────────────────────────────────~%")
  
  ;; CLISP natif
  (if (getf result-clisp :success)
      (format t "  CLISP natif    : ~A (~A)~%" 
              (format-time (getf result-clisp :time))
              (getf result-clisp :result))
      (format t "  CLISP natif    : ERREUR - ~A~%" 
              (or (getf result-clisp :error) "résultat incorrect")))
  
  ;; VM0
  (if (getf result-vm0 :success)
      (format t "  VM0 (interprété): ~A (~A) [~A instr]~%" 
              (format-time (getf result-vm0 :time))
              (getf result-vm0 :result)
              (or (getf result-vm0 :instructions) "?"))
      (format t "  VM0 (interprété): ERREUR - ~A~%" 
              (or (getf result-vm0 :error) "résultat incorrect")))
  
  ;; VM1+VM2
  (if (getf result-vm1 :success)
      (format t "  VM1+VM2 (compilé): ~A (~A)~%" 
              (format-time (getf result-vm1 :time))
              (getf result-vm1 :result))
      (format t "  VM1+VM2 (compilé): ERREUR - ~A~%" 
              (or (getf result-vm1 :error) "résultat incorrect")))
  
  ;; Ratios de performance
  (when (and (getf result-clisp :success) (getf result-vm0 :success))
    (let ((ratio-vm0 (/ (getf result-vm0 :time) (max (getf result-clisp :time) 0.000001))))
      (format t "~%  Ratio VM0/CLISP : ~,1fx plus lent~%" ratio-vm0)))
  
  (when (and (getf result-clisp :success) (getf result-vm1 :success))
    (let ((ratio-vm1 (/ (getf result-vm1 :time) (max (getf result-clisp :time) 0.000001))))
      (format t "  Ratio VM1/CLISP : ~,1fx plus lent~%" ratio-vm1)))
  
  (format t "~%"))

(defun print-summary (results)
  (format t "~%╔════════════════════════════════════════════════════════════════════════════╗~%")
  (format t "║  RÉSUMÉ DES PERFORMANCES                                                   ║~%")
  (format t "╚════════════════════════════════════════════════════════════════════════════╝~%~%")
  
  (let ((total 0)
        (success-clisp 0)
        (success-vm0 0)
        (success-vm1 0)
        (total-time-clisp 0)
        (total-time-vm0 0)
        (total-time-vm1 0))
    
    (dolist (result results)
      (incf total)
      (when (getf (getf result :clisp) :success)
        (incf success-clisp)
        (incf total-time-clisp (getf (getf result :clisp) :time)))
      (when (getf (getf result :vm0) :success)
        (incf success-vm0)
        (incf total-time-vm0 (getf (getf result :vm0) :time)))
      (when (getf (getf result :vm1) :success)
        (incf success-vm1)
        (incf total-time-vm1 (getf (getf result :vm1) :time))))
    
    (format t "Tests réussis:~%")
    (format t "  - CLISP natif    : ~A/~A (~A%)~%" success-clisp total (round (* 100 (/ success-clisp total))))
    (format t "  - VM0 (interprété): ~A/~A (~A%)~%" success-vm0 total (round (* 100 (/ success-vm0 total))))
    (format t "  - VM1+VM2 (compilé): ~A/~A (~A%)~%" success-vm1 total (round (* 100 (/ success-vm1 total))))
    
    (format t "~%Temps total:~%")
    (format t "  - CLISP natif    : ~A~%" (format-time total-time-clisp))
    (format t "  - VM0 (interprété): ~A~%" (format-time total-time-vm0))
    (format t "  - VM1+VM2 (compilé): ~A~%" (format-time total-time-vm1))
    
    (when (> success-clisp 0)
      (format t "~%Performance moyenne:~%")
      (format t "  - VM0 est ~,1fx plus lent que CLISP~%" 
              (/ total-time-vm0 (max total-time-clisp 0.000001)))
      (format t "  - VM1 est ~,1fx plus lent que CLISP~%" 
              (/ total-time-vm1 (max total-time-clisp 0.000001))))
    
    (format t "~%")))

;;; ============================================================================
;;; EXECUTION PRINCIPALE
;;; ============================================================================

(defun run-all-benchmarks (&key (skip-slow nil))
  "Exécute tous les benchmarks et affiche les résultats"
  (print-benchmark-header)
  (setf *benchmark-results* '())
  
  (dolist (benchmark *benchmarks*)
    (let* ((name (getf benchmark :name))
           (description (getf benchmark :description))
           (expr (getf benchmark :lisp-expr))
           (expected (getf benchmark :expected))
           ;; Skip Fibonacci(20) et Ackermann si demandé (trop lents sur VM)
           (should-skip (and skip-slow 
                            (or (search "Fibonacci(20)" name)
                                (search "Ackermann" name)))))
      
      (if should-skip
          (format t "~%SKIP: ~A (trop lent pour VM)~%" name)
          (progn
            (format t "~%Exécution: ~A..." name)
            (force-output)
            
            ;; Exécuter sur CLISP
            (format t " CLISP...")
            (force-output)
            (let ((result-clisp (run-benchmark-clisp expr expected)))
              
              ;; Exécuter sur VM0
              (format t " VM0...")
              (force-output)
              (let ((result-vm0 (run-benchmark-vm0 expr expected)))
                
                ;; Exécuter sur VM1+VM2
                (format t " VM1...")
                (force-output)
                (let ((result-vm1 (run-benchmark-vm1-vm2 expr expected)))
                  
                  (format t " OK~%")
                  
                  ;; Stocker les résultats
                  (push (list :name name
                             :description description
                             :clisp result-clisp
                             :vm0 result-vm0
                             :vm1 result-vm1)
                        *benchmark-results*)
                  
                  ;; Afficher les résultats
                  (print-benchmark-result name description result-clisp result-vm0 result-vm1))))))))
  
  ;; Inverser pour avoir l'ordre original
  (setf *benchmark-results* (reverse *benchmark-results*))
  
  ;; Afficher le résumé
  (print-summary *benchmark-results*))

;;; ============================================================================
;;; LANCEMENT
;;; ============================================================================

(format t "~%Fichier de benchmarks chargé.~%")
(format t "~%Pour lancer les benchmarks:~%")
(format t "  (run-all-benchmarks)           ; Tous les tests~%")
(format t "  (run-all-benchmarks :skip-slow t) ; Skip Fibonacci(20) et Ackermann~%")
(format t "~%")

;; Lancer automatiquement avec skip-slow
(format t "~%Lancement automatique des benchmarks (tests rapides)...~%")
(run-all-benchmarks :skip-slow t)
