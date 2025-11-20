;;;; examples-mips.lisp
;;;; Exemples de programmes en style MIPS pour la VM

(load "utils.lisp")

;;; ============================================================================
;;; EXEMPLES STYLE MIPS
;;; ============================================================================

(defun example-mips-arithmetic ()
  "Exemple MIPS: Addition simple"
  (format t "~%=== EXEMPLE MIPS: Addition (5 + 3) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 5 :$t0)          ; $t0 = 5
                    (:LI 3 :$t1)          ; $t1 = 3
                    (:ADD :$t0 :$t1 :$t2) ; $t2 = $t0 + $t1 = 8
                    (:PRINT :$t2)         ; Affiche 8
                    (:HALT))
                  :verbose t)))

(defun example-mips-addi ()
  "Exemple MIPS: Addition immédiate"
  (format t "~%=== EXEMPLE MIPS: ADDI (10 + 42) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 10 :$t0)          ; $t0 = 10
                    (:ADDI :$t0 42 :$t1)   ; $t1 = $t0 + 42 = 52
                    (:PRINT :$t1)          ; Affiche 52
                    (:HALT))
                  :verbose t)))

(defun example-mips-branches ()
  "Exemple MIPS: Branchements conditionnels"
  (format t "~%=== EXEMPLE MIPS: Branchements (BEQ, BNE, BLT, BGT) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 10 :$t0)          ; $t0 = 10
                    (:LI 5 :$t1)           ; $t1 = 5
                    (:BEQ :$t0 :$t1 EQUAL) ; Ne saute pas (10 != 5)
                    (:BNE :$t0 :$t1 NOTEQUAL) ; Saute car 10 != 5
                    (:LI 999 :$t2)         ; Ne s'exécute pas
                    (:J END)
                    (:LABEL EQUAL)
                    (:LI 100 :$t2)         ; Ne s'exécute pas
                    (:J END)
                    (:LABEL NOTEQUAL)
                    (:BGT :$t0 :$t1 GREATER) ; Saute car 10 > 5
                    (:LI 200 :$t2)
                    (:J END)
                    (:LABEL GREATER)
                    (:LI 42 :$t2)          ; $t2 = 42
                    (:LABEL END)
                    (:PRINT :$t2)          ; Affiche 42
                    (:HALT))
                  :verbose nil)))

(defun example-mips-memory ()
  "Exemple MIPS: Accès mémoire (LW/SW)"
  (format t "~%=== EXEMPLE MIPS: Load/Store Word ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 42 :$t0)          ; $t0 = 42
                    (:LI 1000 :$t1)        ; $t1 = 1000 (adresse de base)
                    (:SW :$t0 :$t1 0)      ; MEM[1000+0] = 42
                    (:SW :$t0 :$t1 4)      ; MEM[1000+4] = 42
                    (:LI 100 :$t2)         ; $t2 = 100
                    (:SW :$t2 :$t1 8)      ; MEM[1000+8] = 100
                    (:LI 0 :$t3)           ; $t3 = 0
                    (:LW :$t1 0 :$t3)      ; $t3 = MEM[1000+0] = 42
                    (:PRINT :$t3)          ; Affiche 42
                    (:LW :$t1 8 :$t3)      ; $t3 = MEM[1000+8] = 100
                    (:PRINT :$t3)          ; Affiche 100
                    (:HALT))
                  :verbose nil)))

(defun example-mips-mul-div ()
  "Exemple MIPS: Multiplication et division"
  (format t "~%=== EXEMPLE MIPS: MUL et DIV ($hi/$lo) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 7 :$t0)           ; $t0 = 7
                    (:LI 6 :$t1)           ; $t1 = 6
                    (:MUL :$t0 :$t1)       ; $lo = 7 * 6 = 42
                    (:MFLO :$t2)           ; $t2 = $lo = 42
                    (:PRINT :$t2)          ; Affiche 42
                    (:LI 10 :$t0)          ; $t0 = 10
                    (:LI 3 :$t1)           ; $t1 = 3
                    (:DIV :$t0 :$t1)       ; $lo = 10/3 = 3, $hi = 10%3 = 1
                    (:MFLO :$t2)           ; $t2 = quotient = 3
                    (:MFHI :$t3)           ; $t3 = reste = 1
                    (:PRINT :$t2)          ; Affiche 3
                    (:PRINT :$t3)          ; Affiche 1
                    (:HALT))
                  :verbose nil)))

(defun example-mips-slt ()
  "Exemple MIPS: Set on Less Than"
  (format t "~%=== EXEMPLE MIPS: SLT (Set on Less Than) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 5 :$t0)           ; $t0 = 5
                    (:LI 10 :$t1)          ; $t1 = 10
                    (:SLT :$t0 :$t1 :$t2)  ; $t2 = ($t0 < $t1) = 1
                    (:PRINT :$t2)          ; Affiche 1
                    (:SLT :$t1 :$t0 :$t3)  ; $t3 = ($t1 < $t0) = 0
                    (:PRINT :$t3)          ; Affiche 0
                    (:HALT))
                  :verbose t)))

(defun example-mips-loop ()
  "Exemple MIPS: Boucle comptant de 1 à 5"
  (format t "~%=== EXEMPLE MIPS: Boucle (1 à 5) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 1 :$t0)           ; Compteur = 1
                    (:LI 5 :$t1)           ; Limite = 5
                    (:LABEL LOOP)
                    (:PRINT :$t0)          ; Affiche compteur
                    (:ADDI :$t0 1 :$t0)    ; Compteur++
                    (:BLT :$t0 :$t1 LOOP)  ; Si compteur < 5, continuer
                    (:BEQ :$t0 :$t1 LOOP)  ; Si compteur == 5, une dernière fois
                    (:HALT))
                  :verbose nil)))

(defun example-mips-factorial ()
  "Exemple MIPS: Factorielle itérative (5!)"
  (format t "~%=== EXEMPLE MIPS: Factorielle (5!) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 5 :$a0)           ; n = 5
                    (:LI 1 :$v0)           ; result = 1
                    (:LI 1 :$t0)           ; constante 1
                    (:LABEL LOOP)
                    (:BLT :$a0 :$t0 END)   ; Si n < 1, fin
                    (:MUL :$v0 :$a0)       ; $lo = result * n
                    (:MFLO :$v0)           ; result = $lo
                    (:SUB :$a0 :$t0 :$a0)  ; n = n - 1
                    (:J LOOP)
                    (:LABEL END)
                    (:PRINT :$v0)          ; Affiche 120
                    (:HALT))
                  :verbose nil)))

(defun example-mips-zero-register ()
  "Exemple MIPS: Test du registre $zero"
  (format t "~%=== EXEMPLE MIPS: Registre $zero (toujours 0) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LI 42 :$t0)          ; $t0 = 42
                    (:ADD :$t0 :$zero :$t1) ; $t1 = $t0 + 0 = 42
                    (:PRINT :$t1)          ; Affiche 42
                    (:MOVE :$zero :$t2)    ; $t2 = $zero = 0
                    (:PRINT :$t2)          ; Affiche 0
                    ;; Tenter d'écrire dans $zero (devrait être ignoré)
                    (:LI 999 :$zero)       ; Essai d'écriture (ignoré)
                    (:PRINT :$zero)        ; Affiche 0 (pas 999)
                    (:HALT))
                  :verbose t)))

(defun example-mips-conventions ()
  "Exemple MIPS: Conventions d'appel (simulation)"
  (format t "~%=== EXEMPLE MIPS: Conventions (arguments/retour) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '(;; Simuler un appel avec arguments dans $a0, $a1
                    (:LI 10 :$a0)          ; Premier argument
                    (:LI 20 :$a1)          ; Deuxième argument
                    ;; "Fonction" qui additionne les arguments
                    (:ADD :$a0 :$a1 :$v0)  ; Résultat dans $v0
                    (:PRINT :$v0)          ; Affiche 30
                    ;; Utilisation de registres sauvegardés
                    (:LI 100 :$s0)         ; $s0 = 100 (sauvegardé)
                    (:ADDI :$s0 23 :$s1)   ; $s1 = 123
                    (:PRINT :$s1)          ; Affiche 123
                    (:HALT))
                  :verbose nil)))

;;; ============================================================================
;;; EXÉCUTION DE TOUS LES EXEMPLES MIPS
;;; ============================================================================

(defun run-all-mips-examples ()
  "Exécute tous les exemples MIPS"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                    EXEMPLES DE PROGRAMMES STYLE MIPS~%")
  (format t "================================================================================~%")
  
  (example-mips-arithmetic)
  (example-mips-addi)
  (example-mips-branches)
  (example-mips-memory)
  (example-mips-mul-div)
  (example-mips-slt)
  (example-mips-loop)
  (example-mips-factorial)
  (example-mips-zero-register)
  (example-mips-conventions)
  
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                        FIN DES EXEMPLES MIPS~%")
  (format t "================================================================================~%"))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(example-mips-arithmetic example-mips-addi example-mips-branches
          example-mips-memory example-mips-mul-div example-mips-slt
          example-mips-loop example-mips-factorial example-mips-zero-register
          example-mips-conventions run-all-mips-examples))
