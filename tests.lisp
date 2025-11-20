;;;; tests.lisp
;;;; Fichier de tests pour la VM

(load "utils.lisp")

;;; ============================================================================
;;; TESTS DE LA VM
;;; ============================================================================

(defun test-vm-basic ()
  "Tests de base de la VM"
  (format t "~%=== TEST VM: Création et initialisation ===~%")
  (let ((vm (make-new-vm :verbose t)))
    (format t "VM créée avec succès~%")
    (format t "État: ~A~%" (vm-state vm))
    (format t "$sp: ~A~%" (get-register vm :$sp))
    (format t "$fp: ~A~%" (get-register vm :$fp))
    (format t "$gp: ~A~%" (get-register vm :$gp))
    (format t "$pc: ~A~%" (get-register vm :$pc))
    vm))

(defun test-stack-operations ()
  "Test des opérations de pile"
  (format t "~%=== TEST: Opérations de pile ===~%")
  (let ((vm (make-new-vm)))
    (format t "Push 10...~%")
    (push-stack vm 10)
    (format t "Push 20...~%")
    (push-stack vm 20)
    (format t "Push 30...~%")
    (push-stack vm 30)
    (dump-stack vm 5)
    (format t "~%Pop: ~A (attendu: 30)~%" (pop-stack vm))
    (format t "Pop: ~A (attendu: 20)~%" (pop-stack vm))
    (format t "Pop: ~A (attendu: 10)~%" (pop-stack vm))
    (format t "Test réussi!~%")))

(defun test-memory-operations ()
  "Test des opérations mémoire"
  (format t "~%=== TEST: Opérations mémoire ===~%")
  (let ((vm (make-new-vm)))
    (format t "Écriture à l'adresse 100: 42~%")
    (mem-write vm 100 42)
    (format t "Lecture à l'adresse 100: ~A (attendu: 42)~%" 
            (mem-read vm 100))
    (format t "Allocation mémoire: ~A octets~%" 10)
    (let ((addr (alloc-memory vm 10)))
      (format t "Adresse allouée: ~A~%" addr)
      (format t "Nouveau $gp: ~A~%" (get-register vm :$gp)))
    (format t "Test réussi!~%")))

(defun test-labels ()
  "Test de la résolution des labels"
  (format t "~%=== TEST: Résolution des labels ===~%")
  (let ((code '((:LOADI 1 :R0)
                (:JMP SKIP)
                (:LOADI 999 :R0)
                (:LABEL SKIP)
                (:LOADI 2 :R0)
                (:HALT))))
    (disassemble-code code)
    (multiple-value-bind (resolved labels)
        (preprocess-code code)
      (format t "~%Code résolu:~%")
      (disassemble-code resolved)
      (format t "~%Labels:~%")
      (maphash (lambda (name addr)
                 (format t "  ~A -> ~A~%" name addr))
               labels)
      (format t "~%Test réussi!~%"))))

;;; ============================================================================
;;; TESTS D'EXÉCUTION
;;; ============================================================================

(defun test-execution-simple ()
  "Test d'exécution simple: 5 + 3"
  (format t "~%=== TEST: Exécution simple (5 + 3) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 5 :R0)
                    (:LOADI 3 :R1)
                    (:ADD :R1 :R0)
                    (:PRINT :R0)
                    (:HALT))
                  :verbose t)))

(defun test-execution-complex ()
  "Test d'exécution complexe: (10 + 5) * 2"
  (format t "~%=== TEST: Exécution complexe ((10 + 5) * 2) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 10 :R0)   ; R0 = 10
                    (:LOADI 5 :R1)    ; R1 = 5
                    (:ADD :R1 :R0)    ; R0 = R0 + R1 = 15
                    (:LOADI 2 :R1)    ; R1 = 2
                    (:MUL :R1 :R0)    ; R0 = R0 * R1 = 30
                    (:PRINT :R0)      ; Affiche R0
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 30~%")))

(defun test-conditional-jump ()
  "Test de saut conditionnel avec JZ"
  (format t "~%=== TEST: Saut conditionnel ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 5 :R0)    ; R0 = 5
                    (:LOADI 5 :R1)    ; R1 = 5
                    (:CMP :R0 :R1)    ; Compare R0 et R1 (égaux -> EQ=1)
                    (:JZ EQUAL)       ; Saute si EQ=1
                    (:LOADI 100 :R2)  ; Ne s'exécute pas
                    (:PRINT :R2)
                    (:JMP END)
                    (:LABEL EQUAL)
                    (:LOADI 200 :R2)  ; R2 = 200
                    (:PRINT :R2)      ; Affiche 200
                    (:LABEL END)
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 200~%")))

(defun test-comparison ()
  "Test des comparaisons"
  (format t "~%=== TEST: Comparaison ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 10 :R0)
                    (:LOADI 5 :R1)
                    (:CMP :R0 :R1)
                    (:HALT))
                  :verbose t)
    (format t "~%Registres après comparaison:~%")
    (format t "GT (10 > 5): ~A (attendu: 1)~%" (get-register vm :GT))
    (format t "LT (10 < 5): ~A (attendu: 0)~%" (get-register vm :LT))
    (format t "EQ (10 = 5): ~A (attendu: 0)~%" (get-register vm :EQ))))

(defun test-move-and-load ()
  "Test des instructions MOVE et LOAD"
  (format t "~%=== TEST: MOVE et LOAD ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 42 :R0)      ; R0 = 42
                    (:MOVE :R0 :R1)      ; R1 = R0 = 42
                    (:PRINT :R1)         ; Affiche 42
                    (:LOADI 100 :R2)     ; R2 = 100 (adresse)
                    (:STORE :R0 :R2)     ; MEM[100] = R0 = 42
                    (:LOADI 0 :R1)       ; R1 = 0 (réinitialiser)
                    (:LOADI 100 :R2)     ; R2 = 100
                    (:LOAD :R2 :R1)      ; R1 = MEM[100] = 42
                    (:PRINT :R1)         ; Affiche 42
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 42, 42~%")))

(defun test-stack-with-registers ()
  "Test de la pile avec les registres"
  (format t "~%=== TEST: Pile avec registres ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 10 :R0)      ; R0 = 10
                    (:LOADI 20 :R1)      ; R1 = 20
                    (:PUSH :R0)          ; Empile R0
                    (:PUSH :R1)          ; Empile R1
                    (:LOADI 0 :R0)       ; R0 = 0
                    (:LOADI 0 :R1)       ; R1 = 0
                    (:POP :R2)           ; R2 = 20 (dépile)
                    (:POP :R0)           ; R0 = 10 (dépile)
                    (:PRINT :R0)         ; Affiche 10
                    (:PRINT :R2)         ; Affiche 20
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 10, 20~%")))

(defun test-comparison-branches ()
  "Test des branchements basés sur comparaisons"
  (format t "~%=== TEST: Branchements conditionnels ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 10 :R0)      ; R0 = 10
                    (:LOADI 5 :R1)       ; R1 = 5
                    (:CMP :R0 :R1)       ; Compare R0 et R1 (10 > 5)
                    (:JGT GREATER)       ; Saute si GT=1
                    (:LOADI 0 :R2)       ; Ne s'exécute pas
                    (:JMP END)
                    (:LABEL GREATER)
                    (:LOADI 99 :R2)      ; R2 = 99
                    (:PRINT :R2)         ; Affiche 99
                    (:LABEL END)
                    (:HALT))
                  :verbose t)
    (format t "Résultat attendu: 99~%")))

(defun test-jal-jr ()
  "Test de JAL (Jump And Link) et JR (Jump Register)"
  (format t "~%=== TEST: JAL et JR (appel de fonction simple) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '(;; Main
                    (:LI 5 :$a0)           ; Argument 1
                    (:LI 3 :$a1)           ; Argument 2
                    (:JAL ADD_FUNC)        ; Appelle la fonction
                    (:MOVE :$v0 :$t0)      ; Récupère le résultat
                    (:PRINT :$t0)          ; Affiche le résultat
                    (:HALT)
                    
                    ;; Fonction ADD_FUNC
                    (:LABEL ADD_FUNC)
                    (:ADD :$a0 :$a1 :$v0)  ; $v0 = $a0 + $a1
                    (:JR :$ra))            ; Retour
                  :verbose t)
    (format t "Résultat attendu: 8 (5 + 3)~%")))

(defun test-jal-jr-with-stack ()
  "Test de JAL/JR avec sauvegarde de registres sur la pile"
  (format t "~%=== TEST: JAL/JR avec sauvegarde pile ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '(;; Main
                    (:LI 10 :$s0)          ; Variable sauvegardée
                    (:LI 20 :$s1)          ; Variable sauvegardée
                    (:LI 5 :$a0)           ; Argument
                    (:JAL FUNC)            ; Appel fonction
                    (:PRINT :$s0)          ; Vérifie $s0 non modifié (10)
                    (:PRINT :$s1)          ; Vérifie $s1 non modifié (20)
                    (:PRINT :$v0)          ; Résultat fonction
                    (:HALT)
                    
                    ;; Fonction qui sauvegarde $s0
                    (:LABEL FUNC)
                    (:PUSH :$s0)           ; Sauvegarde $s0
                    (:LI 100 :$s0)         ; Utilise $s0
                    (:ADD :$a0 :$s0 :$v0)  ; $v0 = $a0 + $s0 = 5 + 100 = 105
                    (:POP :$s0)            ; Restaure $s0
                    (:JR :$ra))            ; Retour
                  :verbose t)
    (format t "Résultat attendu: 10, 20, 105~%")))

(defun test-nested-calls ()
  "Test d'appels de fonction imbriqués (A appelle B)"
  (format t "~%=== TEST: Appels imbriqués (A appelle B) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '(;; Main
                    (:LI 5 :$a0)           ; Argument
                    (:JAL FUNC_A)          ; Appelle A
                    (:PRINT :$v0)          ; Résultat final
                    (:HALT)
                    
                    ;; Fonction A qui appelle B
                    (:LABEL FUNC_A)
                    (:PUSH :$ra)           ; Sauvegarde $ra (important!)
                    (:ADDI :$a0 10 :$a0)   ; $a0 = $a0 + 10 = 15
                    (:JAL FUNC_B)          ; Appelle B
                    (:ADDI :$v0 5 :$v0)    ; $v0 = $v0 + 5
                    (:POP :$ra)            ; Restaure $ra
                    (:JR :$ra)             ; Retour au main
                    
                    ;; Fonction B
                    (:LABEL FUNC_B)
                    (:ADDI :$a0 2 :$v0)    ; $v0 = $a0 + 2 = 17
                    (:JR :$ra))            ; Retour à A
                  :verbose t)
    (format t "Résultat attendu: 22 (5 + 10 + 2 + 5)~%")))

;;; ============================================================================
;;; SUITE DE TESTS COMPLÈTE
;;; ============================================================================

(defun run-all-vm-tests ()
  "Exécute tous les tests de la VM"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                     SUITE DE TESTS COMPLÈTE DE LA VM~%")
  (format t "================================================================================~%")
  
  (handler-case
      (progn
        (test-vm-basic)
        (test-stack-operations)
        (test-memory-operations)
        (test-labels)
        (test-execution-simple)
        (test-execution-complex)
        (test-conditional-jump)
        (test-comparison)
        (test-move-and-load)
        (test-stack-with-registers)
        (test-comparison-branches)
        (test-jal-jr)
        (test-jal-jr-with-stack)
        (test-nested-calls)
        (run-all-tests)
        
        (format t "~%")
        (format t "================================================================================~%")
        (format t "                     TOUS LES TESTS ONT RÉUSSI! ✓~%")
        (format t "================================================================================~%"))
    (error (e)
      (format t "~%")
      (format t "================================================================================~%")
      (format t "                     ÉCHEC D'UN TEST: ~A~%" e)
      (format t "================================================================================~%"))))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(test-vm-basic test-stack-operations test-memory-operations
          test-labels test-execution-simple test-execution-complex
          test-conditional-jump test-comparison test-jal-jr
          test-jal-jr-with-stack test-nested-calls run-all-vm-tests))
