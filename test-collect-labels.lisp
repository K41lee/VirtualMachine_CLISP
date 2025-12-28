;;; Test de compilation de collect-labels
;;; Cette fonction est essentielle pour le loader complet

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║        TEST COMPILATION DE COLLECT-LABELS                        ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%~%")

;;; Version simplifiée de collect-labels pour commencer
;;; Remplace dolist par while, mapcar par des boucles explicites

(defparameter *collect-labels-simplified*
  '(defun collect-labels-simple (asm-code code-start)
     "Version simplifiée: compte juste les labels"
     (let ((labels (vm-make-hash-table :test 'equal))
           (position 0)
           (i 0))
       ;; Parcourir le code avec un compteur
       ;; Note: asm-code devrait être une liste, mais on simule avec length=3
       (while (< i 3)
         ;; Pour le test, on suppose qu'on a 3 instructions
         ;; L'instruction i=0 est un label, les autres non
         (if (= i 0)
             ;; C'est un label, l'enregistrer
             (vm-hash-set labels 100 (+ code-start position))
             ;; Sinon, incrémenter la position
             (setq position (+ position 1)))
         (setq i (+ i 1)))
       ;; Retourner le nombre de labels trouvés
       (vm-hash-table-count labels))))

(format t "═══════════════════════════════════════════════════════════════════~%")
(format t "TEST : Compilation de collect-labels simplifié~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

(format t "  → Compilation...~%")
(handler-case
    (progn
      (defparameter *compiled* (compile-lisp *collect-labels-simplified*))
      (format t "  ✓ Compilé : ~A instructions~%~%" (length *compiled*))
      
      (format t "  → Code généré (premiers 20 lignes):~%")
      (dotimes (i (min 20 (length *compiled*)))
        (format t "    [~2D] ~A~%" i (nth i *compiled*)))
      
      (format t "~%  → Exécution du test...~%")
      (defparameter *vm* (make-new-vm :verbose nil))
      (load-code *vm* *compiled* :verbose nil)
      
      ;; Appeler collect-labels-simple avec code-start = 10000
      (set-register *vm* (get-reg :a0) nil)  ; asm-code (ignoré dans version simple)
      (set-register *vm* (get-reg :a1) 10000) ; code-start
      (set-register *vm* (get-reg :pc) (calculate-code-start *vm*))
      
      (run-vm *vm* :max-instructions 10000)
      
      (defparameter *result* (get-register *vm* :$v0))
      (format t "  ✓ Résultat : ~A labels trouvés~%~%" *result*)
      
      (if (= *result* 1)
          (format t "  ✅ TEST RÉUSSI! (1 label trouvé comme attendu)~%")
          (format t "  ❌ TEST ÉCHOUÉ! (attendu: 1, obtenu: ~A)~%" *result*)))
  (error (e)
    (format t "  ❌ ERREUR lors de la compilation:~%")
    (format t "     ~A~%~%" e)))

(format t "~%Test terminé.~%~%")
