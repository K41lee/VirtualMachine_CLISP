;;;; test-fibo20-with-original.lisp
;;;; Test de fibonacci(20) avec le compilateur original pour comparaison

(format t "~%╔════════════════════════════════════════════════════╗~%")
(format t "║   TEST FIBONACCI(20) - Compilateur original       ║~%")
(format t "╚════════════════════════════════════════════════════╝~%~%")

(format t "Chargement des modules...~%")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(format t "✓ Modules chargés~%~%")

(defvar *fibo-code*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1))
            (fibo (- n 2))))))

(format t "Code Fibonacci:~%~A~%~%" *fibo-code*)

(format t "Compilation avec le compilateur original...~%")
(defvar *compiled* (compile-lisp *fibo-code*))
(format t "✓ Compilé: ~A instructions~%~%" (length *compiled*))

(format t "Création de la VM et chargement...~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *compiled*)
(set-register *vm* :$A0 20)
(format t "✓ Code chargé, $A0 = 20~%~%")

(format t "Exécution de fibo(20)...~%")
(format t "(Patience, cela prend 10-30 secondes)~%~%")

(let ((start (get-internal-real-time)))
  (run-vm *vm*)
  (let* ((end (get-internal-real-time))
         (elapsed (/ (- end start) internal-time-units-per-second))
         (result (get-register *vm* :$V0))
         (inst-count (vm-instruction-count *vm*)))
    
    (format t "~%╔═══════════════════════════════════════════╗~%")
    (format t "║              RÉSULTATS                     ║~%")
    (format t "╚═══════════════════════════════════════════╝~%~%")
    
    (format t "fibo(20) = ~A~%~%" result)
    (format t "Statistiques:~%")
    (format t "  • Temps:        ~,3F secondes~%" elapsed)
    (format t "  • Instructions: ~:D~%" inst-count)
    (format t "  • Inst/sec:     ~:D~%~%" (floor (/ inst-count elapsed)))
    
    (if (= result 6765)
        (progn
          (format t "✅ SUCCÈS ! Résultat correct.~%~%")
          (format t "Le compilateur original fonctionne parfaitement.~%~%"))
        (progn
          (format t "❌ ÉCHEC ! Résultat incorrect (attendu 6765).~%~%")))))
