;;;; test-compiler.lisp
;;;; Tests du compilateur LISP → MIPS

(load "compiler.lisp")

;;; ============================================================================
;;; TESTS DU COMPILATEUR
;;; ============================================================================

(defun test-compiler-constant ()
  "Test: compilation de constante"
  (format t "~%=== TEST: Compilation de constante (42) ===~%")
  (compile-and-run 42))

(defun test-compiler-addition ()
  "Test: compilation d'addition simple"
  (format t "~%=== TEST: Compilation d'addition (+ 5 3) ===~%")
  (compile-and-run '(+ 5 3)))

(defun test-compiler-complex ()
  "Test: compilation d'expression complexe"
  (format t "~%=== TEST: Compilation de (* (+ 10 5) 2) ===~%")
  (compile-and-run '(* (+ 10 5) 2)))

(defun test-compiler-comparison ()
  "Test: compilation de comparaison"
  (format t "~%=== TEST: Compilation de (< 5 10) ===~%")
  (compile-and-run '(< 5 10)))

(defun test-compiler-if ()
  "Test: compilation de IF/THEN/ELSE"
  (format t "~%=== TEST: Compilation de (if (< 5 10) 100 200) ===~%")
  (compile-and-run '(if (< 5 10) 100 200)))

(defun test-compiler-if-false ()
  "Test: compilation de IF avec condition fausse"
  (format t "~%=== TEST: Compilation de (if (> 5 10) 100 200) ===~%")
  (compile-and-run '(if (> 5 10) 100 200)))

(defun test-compiler-simple-function ()
  "Test: compilation de fonction simple"
  (format t "~%=== TEST: Fonction simple (defun double (x) (* x 2)) appelée avec 21 ===~%")
  ;; On doit compiler la définition puis l'appel
  (let ((vm (make-new-vm :verbose nil))
        (func-def '(defun double (x) (* x 2)))
        (func-call '(double 21)))
    ;; Compiler la fonction
    (let ((func-code (compile-lisp func-def))
          (call-code (compile-lisp func-call)))
      ;; Générer un JMP vers :MAIN pour sauter les définitions de fonctions
      (let ((full-code (append (list (list :JMP ':MAIN))   ; Jump to main code
                              func-code                      ; Function definitions
                              (list (list :LABEL ':MAIN))   ; Main code label
                              call-code                      ; Function calls
                              (list (list :PRINT *reg-v0*)
                                    (list :HALT)))))
        (format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
        (dolist (instr full-code)
          (format t "~A~%" instr))
        (format t "~%=== EXÉCUTION ===~%")
        (load-and-run vm full-code :verbose nil)
        (format t "~%Résultat dans $v0: ~A~%" (get-register vm *reg-v0*))
        vm))))

(defun test-compiler-fibonacci (&optional (n 10))
  "Test: compilation de fibonacci récursif"
  (format t "~%=== TEST: Fibonacci récursif fib(~A) ===~%" n)
  (let ((vm (make-new-vm :verbose nil))
        (fib-def '(defun fib (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1))
                           (fib (- n 2))))))
        (fib-call (list 'fib n)))
    (let ((fib-code (compile-lisp fib-def))
          (call-code (compile-lisp fib-call)))
      ;; Architecture code avec JMP :MAIN
      (let ((full-code (append (list (list :JMP ':MAIN))
                              fib-code
                              (list (list :LABEL ':MAIN))
                              call-code
                              (list (list :PRINT *reg-v0*)
                                    (list :HALT)))))
        (format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ (partiel) ===~%")
        (dolist (instr (subseq full-code 0 (min 20 (length full-code))))
          (format t "~A~%" instr))
        (format t "... (~A instructions au total)~%" (length full-code))
        (format t "~%=== EXÉCUTION ===~%")
        (load-and-run vm full-code :verbose nil)
        (format t "~%Résultat dans $v0: ~A~%" (get-register vm *reg-v0*))
        vm))))

;;; ============================================================================
;;; TESTS DE PERFORMANCE
;;; ============================================================================

;; Variables globales pour stocker les résultats
(defvar *fib-native-time* 0)
(defvar *fib-native-result* 0)
(defvar *fib-vm-time* 0)
(defvar *fib-vm-result* 0)
(defvar *fib-vm-instructions* 0)

(defun fib-native (n)
  "Fibonacci natif CLISP pour comparaison"
  (if (<= n 1)
      n
      (+ (fib-native (- n 1))
         (fib-native (- n 2)))))

(defun test-fibonacci-performance (&optional (n 10))
  "Compare les performances de fibonacci entre VM et CLISP natif"
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "           TEST DE PERFORMANCE: FIBONACCI(~A)~%" n)
  (format t "================================================================================~%")
  
  ;; Test avec CLISP natif
  (format t "~%--- Test avec CLISP natif ---~%")
  (let* ((start-time (get-internal-real-time))
         (result (fib-native n))
         (end-time (get-internal-real-time))
         (elapsed (/ (- end-time start-time) 
                    internal-time-units-per-second)))
    (format t "Résultat: ~A~%" result)
    (format t "Temps: ~,6F secondes~%" elapsed)
    (setf *fib-native-time* elapsed)
    (setf *fib-native-result* result))
  
  ;; Test avec notre VM
  (format t "~%--- Test avec VM MIPS ---~%")
  (let ((vm (make-new-vm :verbose nil))
        (fib-def '(defun fib (n)
                    (if (<= n 1)
                        n
                        (+ (fib (- n 1))
                           (fib (- n 2))))))
        (fib-call (list 'fib n)))
    (let ((fib-code (compile-lisp fib-def))
          (call-code (compile-lisp fib-call)))
      (let* ((full-code (append (list (list :JMP ':MAIN))
                                fib-code
                                (list (list :LABEL ':MAIN))
                                call-code
                                (list (list :HALT))))
             (start-time (get-internal-real-time))
             (dummy (load-and-run vm full-code :verbose nil))
             (end-time (get-internal-real-time))
             (elapsed (/ (- end-time start-time) 
                        internal-time-units-per-second))
             (result (get-register vm *reg-v0*)))
        (format t "Résultat: ~A~%" result)
        (format t "Temps: ~,6F secondes~%" elapsed)
        (format t "Instructions exécutées: ~A~%" (vm-instruction-count vm))
        (setf *fib-vm-time* elapsed)
        (setf *fib-vm-result* result)
        (setf *fib-vm-instructions* (vm-instruction-count vm)))))
  
  ;; Comparaison
  (format t "~%~%")
  (format t "================================================================================~%")
  (format t "                           COMPARAISON~%")
  (format t "================================================================================~%")
  (format t "~%CLISP natif:~%")
  (format t "  Résultat: ~A~%" *fib-native-result*)
  (format t "  Temps: ~,6F secondes~%" *fib-native-time*)
  (format t "~%VM MIPS:~%")
  (format t "  Résultat: ~A~%" *fib-vm-result*)
  (format t "  Temps: ~,6F secondes~%" *fib-vm-time*)
  (format t "  Instructions: ~A~%" *fib-vm-instructions*)
  (when (and (> *fib-native-time* 0) (> *fib-vm-time* 0))
    (format t "~%Ratio (VM / Natif): ~,2Fx plus lent~%" 
            (/ *fib-vm-time* *fib-native-time*)))
  (format t "~%")
  (if (= *fib-native-result* *fib-vm-result*)
      (format t "✓ Résultats identiques!~%")
      (format t "✗ ERREUR: Résultats différents!~%"))
  (format t "================================================================================~%")
  (format t "~%"))

;;; ============================================================================
;;; SUITE DE TESTS COMPLÈTE
;;; ============================================================================

(defun run-all-compiler-tests ()
  "Exécute tous les tests du compilateur"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                     TESTS DU COMPILATEUR LISP → ASM~%")
  (format t "================================================================================~%")
  
  (test-compiler-constant)
  (test-compiler-addition)
  (test-compiler-complex)
  (test-compiler-comparison)
  (test-compiler-if)
  (test-compiler-if-false)
  
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                     TESTS TERMINÉS~%")
  (format t "================================================================================~%"))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(format t "~%Tests du compilateur chargés. Fonctions disponibles:~%")
(format t "  - (test-compiler-constant)~%")
(format t "  - (test-compiler-addition)~%")
(format t "  - (test-compiler-complex)~%")
(format t "  - (test-compiler-comparison)~%")
(format t "  - (test-compiler-if)~%")
(format t "  - (test-compiler-if-false)~%")
(format t "  - (test-compiler-simple-function)~%")
(format t "  - (test-compiler-fibonacci n)~%")
(format t "  - (test-fibonacci-performance n)~%")
(format t "  - (run-all-compiler-tests)~%~%")
