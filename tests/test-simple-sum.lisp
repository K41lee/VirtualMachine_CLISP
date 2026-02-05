(load "main.lisp")

; Test simple sans fonction compile-and-run
(let* ((vm (make-vm))
       (code '(
         (defun sum-to-n (n)
           (if (= n 0)
               0
               (+ n (sum-to-n (- n 1)))))
         (sum-to-n 3)
       ))
       (asm (compile-lisp code)))
  
  ; Afficher certaines lignes clés
  (format t "Lignes clés du code assembleur:~%")
  (format t "Ligne 23-27 (sauvegarde n et ajustements):~%")
  (dotimes (i 5)
    (format t "  [~2d] ~a~%" (+ 23 i) (nth (+ 23 i) asm)))
  (format t "Ligne 44-47 (restauration et addition):~%")
  (dotimes (i 4)
    (format t "  [~2d] ~a~%" (+ 44 i) (nth (+ 44 i) asm)))
  
  ; Charger et exécuter
  (reset-vm vm)
  (load-program vm asm)
  (vm-execute-final vm)
  
  (format t "~%Résultat dans $V0: ~a (attendu 6)~%" (register-get vm *reg-v0*))
  (format t "Valeur de $S0: ~a~%" (register-get vm *reg-s0*))
  (format t "Valeur de $T0: ~a~%" (register-get vm *reg-t0*)))
