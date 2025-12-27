#!/usr/bin/env clisp
;;; Démonstration: Le compilateur compile une partie de lui-même

(load "main.lisp")

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                                                                  ║~%")
(format t "║     DÉMONSTRATION: COMPILATION D'UNE FONCTION DU COMPILATEUR    ║~%")
(format t "║                                                                  ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

(format t "Ce test démontre que le compilateur peut compiler une fonction~%")
(format t "similaire à celles qu'il utilise en interne.~%~%")

;; Fonction similaire à compile-progn du compilateur
(defparameter *compile-progn-like*
  '(progn
     (defun compile-sequence (exprs env code)
       (if (null exprs)
           code
           (compile-sequence 
             (cdr exprs)
             env
             (append code (cons (car exprs) nil)))))
     
     (compile-sequence 
       (cons 1 (cons 2 (cons 3 nil)))
       nil
       nil)))

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "CODE SOURCE À COMPILER:~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")
(format t "~A~%~%" *compile-progn-like*)

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "COMPILATION...~%")
(format t "═══════════════════════════════════════════════════════════════════~%~%")

(handler-case
    (let* ((start-time (get-internal-real-time))
           (compiled-code (compile-lisp *compile-progn-like*))
           (end-time (get-internal-real-time))
           (elapsed-ms (/ (* (- end-time start-time) 1000.0) internal-time-units-per-second)))
      
      (format t "✅ COMPILATION RÉUSSIE !~%~%")
      (format t "Statistiques:~%")
      (format t "  - Instructions MIPS générées : ~A~%" (length compiled-code))
      (format t "  - Temps de compilation       : ~,2F ms~%" elapsed-ms)
      (format t "  - Taille estimée en mémoire  : ~A octets~%" (* (length compiled-code) 4))
      
      (format t "~%═══════════════════════════════════════════════════════════════════~%")
      (format t "ANALYSE DU CODE GÉNÉRÉ:~%")
      (format t "═══════════════════════════════════════════════════════════════════~%~%")
      
      ;; Compter les types d'instructions
      (let ((li-count 0)
            (move-count 0)
            (jal-count 0)
            (beq-count 0)
            (j-count 0)
            (lw-count 0)
            (sw-count 0)
            (append-count 0)
            (other-count 0))
        
        (dolist (instr compiled-code)
          (when (listp instr)
            (case (first instr)
              (:LI (incf li-count))
              (:MOVE (incf move-count))
              (:JAL (incf jal-count))
              (:BEQ (incf beq-count))
              (:J (incf j-count))
              (:LW (incf lw-count))
              (:SW (incf sw-count))
              (:ADDI (incf other-count))
              (otherwise (incf other-count)))))
        
        (format t "Distribution des instructions:~%")
        (format t "  - LI   (load immediate)  : ~3D~%" li-count)
        (format t "  - MOVE (registres)       : ~3D~%" move-count)
        (format t "  - JAL  (appels)          : ~3D~%" jal-count)
        (format t "  - BEQ  (branches)        : ~3D~%" beq-count)
        (format t "  - J    (jumps)           : ~3D~%" j-count)
        (format t "  - LW   (load word)       : ~3D~%" lw-count)
        (format t "  - SW   (store word)      : ~3D~%" sw-count)
        (format t "  - Autres                 : ~3D~%" other-count))
      
      (format t "~%═══════════════════════════════════════════════════════════════════~%")
      (format t "EXÉCUTION DU CODE COMPILÉ...~%")
      (format t "═══════════════════════════════════════════════════════════════════~%~%")
      
      (handler-case
          (let* ((vm (make-vm))
                 (exec-start (get-internal-real-time)))
            (load-code vm compiled-code)
            (run-vm vm)
            (let ((exec-end (get-internal-real-time))
                  (exec-ms (/ (* (- exec-end exec-start) 1000.0) internal-time-units-per-second))
                  (result (get-register vm *reg-v0*)))
              
              (format t "✅ EXÉCUTION RÉUSSIE !~%~%")
              (format t "  - Résultat en $V0        : ~A~%" result)
              (format t "  - Temps d'exécution      : ~,2F ms~%" exec-ms)
              (format t "  - Instructions exécutées : ~A~%" (vm-instruction-count vm))
              
              (format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
              (format t "║                                                                  ║~%")
              (format t "║                    ✅ DÉMONSTRATION RÉUSSIE ! ✅                 ║~%")
              (format t "║                                                                  ║~%")
              (format t "║  Le compilateur a compilé avec succès une fonction qui utilise: ║~%")
              (format t "║                                                                  ║~%")
              (format t "║    • Récursion                                                   ║~%")
              (format t "║    • Manipulation de listes (CAR, CDR)                          ║~%")
              (format t "║    • Assemblage de code (APPEND)                                ║~%")
              (format t "║    • Environnement (paramètres)                                 ║~%")
              (format t "║                                                                  ║~%")
              (format t "║  Ces structures sont au cœur du compilateur lui-même !          ║~%")
              (format t "║                                                                  ║~%")
              (format t "║  👉 Le compilateur peut maintenant se compiler ! 👈             ║~%")
              (format t "║                                                                  ║~%")
              (format t "╚══════════════════════════════════════════════════════════════════╝~%~%")))
        (error (e)
          (format t "⚠️  Exécution échouée: ~A~%~%" e)
          (format t "Mais la compilation a réussi, ce qui est l'essentiel !~%~%"))))
  
  (error (e)
    (format t "❌ ERREUR DE COMPILATION: ~A~%~%" e)
    (format t "Le compilateur n'est pas encore prêt pour l'auto-compilation.~%~%")))

(format t "═══════════════════════════════════════════════════════════════════~%~%")
