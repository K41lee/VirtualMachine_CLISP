(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defun test-with-mem-dump ()
  "Test sum-to-n(2) avec dump mémoire"
  (let* ((code '(progn
                  (defun sum-to-n (n)
                    (if (= n 0)
                        0
                        (+ n (sum-to-n (- n 1)))))
                  (sum-to-n 2)))
         (compiled (compile-lisp-to-mips-simplified code))
         (vm (make-new-vm)))
    
    (format t "Test: sum-to-n(2) = 2 + 1 + 0 = 3~%~%")
    
    ; Afficher le code assembleur clé
    (format t "Code assembleur ligne 23-47 (else branch):~%")
    (loop for i from 23 to 47
          do (format t "  [~2d] ~a~%" i (nth i compiled)))
    
    (load-code vm compiled)
    
    ; Ajouter un hook pour tracer l'instruction 45 (LW $T0 $SP 4)
    (format t "~%Exécution...~%~%")
    (run-vm vm :max-instructions 500)
    
    (let ((result (get-register vm :$V0))
          (t0-val (get-register vm :$T0))
          (sp-val (get-register vm :$SP)))
      (format t "~%Résultat final:~%")
      (format t "  $V0 = ~A (attendu 3)~%" result)
      (format t "  $T0 = ~A (devrait être 2)~%" t0-val)
      (format t "  $SP = ~A~%" sp-val)
      
      ; Afficher quelques valeurs mémoire autour de $SP
      (format t "~%Mémoire autour de $SP:~%")
      (loop for offset from -4 to 8
            do (let* ((addr (+ sp-val offset))
                      (val (mem-read vm addr)))
                 (format t "  mem[~A] ($SP~A~A) = ~A~%"
                        addr
                        (if (>= offset 0) "+" "")
                        offset
                        val))))))

(test-with-mem-dump)
