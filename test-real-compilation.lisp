;;; Test RÉEL avec le code de src/vm-compilable.lisp
;;; Compile le fichier entier et compte les DEFUN

(load "src/compiler.lisp")

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  COMPILATION RÉELLE : src/vm-compilable.lisp          ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

;; Charger le fichier pour avoir accès aux définitions
(format t "Chargement de src/vm-compilable.lisp...~%")
(handler-case
    (load "src/vm-compilable.lisp")
  (error (e)
    (format t "✗ ERREUR chargement : ~A~%" e)
    (quit)))

(format t "✓ Fichier chargé~%~%")

;; Lister toutes les fonctions définies
(defparameter *vm-functions*
  '(MAKE-NEW-VM INIT-REGISTERS REG-INDEX
    GET-REGISTER SET-REGISTER REGISTER-P
    GET-MEMORY SET-MEMORY
    ALLOC-MEMORY PUSH-STACK POP-STACK PEEK-STACK
    LOAD-PROGRAM EXECUTE-INSTRUCTION
    EXECUTE-LI EXECUTE-MOVE EXECUTE-ADD EXECUTE-ADDI
    EXECUTE-SUB EXECUTE-MUL EXECUTE-DIV EXECUTE-MOD
    EXECUTE-AND EXECUTE-OR EXECUTE-XOR EXECUTE-NOR
    EXECUTE-SLT EXECUTE-BEQ EXECUTE-BNE EXECUTE-J
    EXECUTE-JAL EXECUTE-JR EXECUTE-LW EXECUTE-SW
    EXECUTE-HALT EXECUTE-PRINT
    RUN-VM HALT-VM? LOAD-AND-RUN))

(defparameter *compiled* 0)
(defparameter *failed* 0)
(defparameter *total-instructions* 0)
(defparameter *errors* '())

(format t "Test de compilation des fonctions :~%~%")

(dolist (fn-name *vm-functions*)
  (format t "~A : " fn-name)
  (handler-case
      (let* ((fn-def (symbol-function fn-name))
             (fn-lambda (function-lambda-expression fn-def)))
        (if fn-lambda
            (handler-case
                (let ((compiled-code (compile-lisp fn-lambda)))
                  (incf *compiled*)
                  (incf *total-instructions* (length compiled-code))
                  (format t "✓ ~A instructions~%" (length compiled-code)))
              (error (e)
                (incf *failed*)
                (push (cons fn-name (format nil "~A" e)) *errors*)
                (format t "✗ ERREUR: ~A~%" e)))
            (progn
              (incf *failed*)
              (push (cons fn-name "Pas de source lambda disponible") *errors*)
              (format t "✗ Pas de source~%"))))
    (error (e)
      (incf *failed*)
      (push (cons fn-name (format nil "~A" e)) *errors*)
      (format t "✗ ERREUR: ~A~%" e))))

(let ((total (+ *compiled* *failed*)))
  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║  RÉSULTATS COMPILATION RÉELLE                          ║~%")
  (format t "╚════════════════════════════════════════════════════════╝~%~%")
  
  (format t "Total fonctions      : ~A~%" total)
  (format t "Compilées avec succès: ~A~%" *compiled*)
  (format t "Échecs               : ~A~%" *failed*)
  (format t "Taux de compilation  : ~A%~%~%" 
          (if (> total 0)
              (round (* 100 (/ *compiled* total)))
              0))
  
  (format t "Total instructions MIPS: ~A~%~%" *total-instructions*)
  
  (when (> *failed* 0)
    (format t "~%═══ FONCTIONS ÉCHOUÉES ═══~%~%")
    (dolist (err (reverse *errors*))
      (format t "✗ ~A~%  → ~A~%~%" (car err) (cdr err))))
  
  (format t "~%Session 3 : 62%% (16/26 DEFUN)~%")
  (format t "Session 4 : ~A%% (~A/~A DEFUN)~%~%" 
          (if (> total 0)
              (round (* 100 (/ *compiled* total)))
              0)
          *compiled*
          total)
  
  (let ((pct (if (> total 0)
                 (round (* 100 (/ *compiled* total)))
                 0)))
    (cond
      ((>= pct 77)
       (format t "✅ OBJECTIF 77%% ATTEINT ! (~A%%)~%" pct))
      ((>= pct 70)
       (format t "Proche : ~A%% / 77%% (manque ~A%%)~%" pct (- 77 pct)))
      (t
       (format t "Progression : 62%% → ~A%% (+~A points)~%" pct (- pct 62))))))
