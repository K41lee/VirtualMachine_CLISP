(load "src/main.lisp")

(format t "~%Test détaillé de l'instruction LIST~%")
(format t "═══════════════════════════════════════~%~%")

;; Code simple
(defun make-list () (list 1 2 3))

(format t "Compilation...~%")
(let ((code (compile-lisp 'make-list)))
  (format t "→ ~A instructions~%~%" (length code))
  
  ;; Afficher le code
  (format t "Code généré:~%")
  (loop for i from 0 below (length code)
        for instr in code
        do (format t "  [~2D] ~A~%" i instr))
  
  (format t "~%Chargement dans la VM...~%")
  (let ((vm (make-vm)))
    (init-vm-memory vm)
    (load-program vm code)
    
    (format t "~%Code chargé en mémoire:~%")
    (loop for addr from *code-start* below (+ *code-start* (min 25 (length code)))
          for i from 0
          do (let ((instr (mem-read vm addr)))
               (format t "  [@~D] = ~A~%" addr instr)))
    
    (format t "~%Exécution avec trace complète...~%")
    (execute-vm vm :verbose t :max-steps 50)))

(quit)
