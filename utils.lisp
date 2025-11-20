;;;; utils.lisp
;;;; Outils de visualisation, traçage et débogage

(load "loader.lisp")

;;; ============================================================================
;;; VISUALISATION COMPLÈTE
;;; ============================================================================

(defun dump-vm-state (vm)
  "Affiche l'état complet de la VM"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                           ÉTAT DE LA VM~%")
  (format t "================================================================================~%")
  (format t "État: ~A~%" (vm-state vm))
  (format t "Instructions exécutées: ~A~%" (vm-instruction-count vm))
  (dump-registers vm)
  (dump-stack vm 5)
  (format t "~%=== ZONE TAS ===~%")
  (let ((hp (get-register vm :HP)))
    (format t "Heap pointer: ~A~%" hp)
    (when (> hp +heap-start+)
      (dump-memory vm +heap-start+ (min (1- hp) (+ +heap-start+ 20)))))
  (format t "~%")
  (format t "================================================================================~%"))

;;; ============================================================================
;;; MODE TRACE
;;; ============================================================================

(defun trace-vm (vm asm-code &key (step-by-step nil))
  "Exécute le code en mode trace"
  (let ((original-verbose (vm-verbose vm)))
    (setf (vm-verbose vm) t)
    (load-code vm asm-code :verbose t)
    (format t "~%=== MODE TRACE ACTIVÉ ===~%")
    (dump-registers vm)
    
    (setf (vm-state vm) :running)
    (setf (vm-instruction-count vm) 0)
    
    (handler-case
        (loop while (eq (vm-state vm) :running)
              do (let ((instr (fetch-instruction vm)))
                   (when (zerop instr)
                     (error "Instruction nulle"))
                   (format t "~%--- Instruction ~A ---~%" 
                           (vm-instruction-count vm))
                   (execute-instruction vm instr)
                   (incf (vm-instruction-count vm))
                   
                   (when step-by-step
                     (format t "~%Appuyez sur Entrée pour continuer...")
                     (read-line))))
      (error (e)
        (setf (vm-state vm) :error)
        (format t "~%ERREUR: ~A~%" e)))
    
    (dump-vm-state vm)
    (setf (vm-verbose vm) original-verbose)))

;;; ============================================================================
;;; STATISTIQUES
;;; ============================================================================

(defun get-vm-stats (vm)
  "Retourne des statistiques sur l'état de la VM"
  (let ((sp (get-register vm :SP))
        (fp (get-register vm :FP))
        (hp (get-register vm :HP))
        (stack-max (- *maxmem* *code-size*)))
    (list :instructions (vm-instruction-count vm)
          :state (vm-state vm)
          :heap-used (- hp +heap-start+)
          :heap-available (- (get-register vm :SP) hp)
          :stack-used (- stack-max sp)
          :stack-available sp)))

(defun print-vm-stats (vm)
  "Affiche les statistiques de la VM"
  (let ((stats (get-vm-stats vm)))
    (format t "~%=== STATISTIQUES VM ===~%")
    (format t "Instructions exécutées: ~A~%" (getf stats :instructions))
    (format t "État: ~A~%" (getf stats :state))
    (format t "Tas utilisé: ~A octets~%" (getf stats :heap-used))
    (format t "Tas disponible: ~A octets~%" (getf stats :heap-available))
    (format t "Pile utilisée: ~A octets~%" (getf stats :stack-used))
    (format t "Pile disponible: ~A octets~%" (getf stats :stack-available))))

;;; ============================================================================
;;; TESTS SIMPLES
;;; ============================================================================

(defun test-simple-arithmetic ()
  "Test des opérations arithmétiques simples: 5 + 3"
  (format t "~%=== TEST: Arithmétique simple (5 + 3) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 5 :R0)    ; R0 = 5
                    (:LOADI 3 :R1)    ; R1 = 3
                    (:ADD :R1 :R0)    ; R0 = R0 + R1 = 8
                    (:PRINT :R0)      ; Affiche R0
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 8~%")))

(defun test-multiplication ()
  "Test de multiplication: 7 * 6"
  (format t "~%=== TEST: Multiplication (7 * 6) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 7 :R0)    ; R0 = 7
                    (:LOADI 6 :R1)    ; R1 = 6
                    (:MUL :R1 :R0)    ; R0 = R0 * R1 = 42
                    (:PRINT :R0)      ; Affiche R0
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 42~%")))

(defun test-jump ()
  "Test de saut inconditionnel"
  (format t "~%=== TEST: Saut ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 10 :R0)   ; R0 = 10
                    (:PRINT :R0)      ; Affiche 10
                    (:JMP END)        ; Saute à END
                    (:LOADI 999 :R0)  ; Cette ligne ne s'exécute pas
                    (:PRINT :R0)      
                    (:LABEL END)      
                    (:LOADI 20 :R0)   ; R0 = 20
                    (:PRINT :R0)      ; Affiche 20
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 10, 20 (pas 999)~%")))

(defun run-all-tests ()
  "Exécute tous les tests"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                        EXÉCUTION DES TESTS~%")
  (format t "================================================================================~%")
  (test-simple-arithmetic)
  (test-multiplication)
  (test-jump)
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                        TESTS TERMINÉS~%")
  (format t "================================================================================~%"))

;;; ============================================================================
;;; HELPERS DE DEBUG
;;; ============================================================================

(defun disassemble-code (asm-code)
  "Affiche le code assembleur de manière lisible"
  (format t "~%=== CODE ASSEMBLEUR ===~%")
  (loop for instr in asm-code
        for i from 0
        do (format t "[~3A] ~A~%" i (format-instruction instr))))

(defun compare-execution-time (lisp-fn asm-code iterations)
  "Compare le temps d'exécution entre LISP natif et la VM"
  (format t "~%=== COMPARAISON DE PERFORMANCE (~A itérations) ===~%" iterations)
  
  ;; Test LISP natif
  (format t "~%Exécution LISP native...~%")
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall lisp-fn))
    (let* ((end-time (get-internal-real-time))
           (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
      (format t "Temps LISP: ~,6F secondes~%" elapsed)
      
      ;; Test VM
      (format t "~%Exécution dans la VM...~%")
      (let ((vm-start (get-internal-real-time)))
        (dotimes (i iterations)
          (let ((vm (make-new-vm)))
            (load-and-run vm asm-code :verbose nil)))
        (let* ((vm-end (get-internal-real-time))
               (vm-elapsed (/ (- vm-end vm-start) internal-time-units-per-second)))
          (format t "Temps VM: ~,6F secondes~%" vm-elapsed)
          (format t "Ratio VM/LISP: ~,2Fx plus lent~%" (/ vm-elapsed elapsed)))))))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(dump-vm-state trace-vm get-vm-stats print-vm-stats
          test-simple-arithmetic test-multiplication test-jump
          run-all-tests disassemble-code compare-execution-time))
