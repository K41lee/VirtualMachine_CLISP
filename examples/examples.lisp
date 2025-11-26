;;;; examples.lisp
;;;; Exemples de programmes en assembleur pour la VM

(load "utils.lisp")

;;; ============================================================================
;;; EXEMPLES SIMPLES
;;; ============================================================================

(defun example-arithmetic ()
  "Exemple: Calcul arithmétique (10 + 5) * 2"
  (format t "~%=== EXEMPLE: Arithmétique (10 + 5) * 2 ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 10 :R0)       ; R0 = 10
                    (:LOADI 5 :R1)        ; R1 = 5
                    (:ADD :R1 :R0)        ; R0 = 10 + 5 = 15
                    (:LOADI 2 :R1)        ; R1 = 2
                    (:MUL :R1 :R0)        ; R0 = 15 * 2 = 30
                    (:PRINT :R0)          ; Affiche 30
                    (:HALT))
                  :verbose t)))

(defun example-conditional ()
  "Exemple: Condition if x > 10 then 100 else 200"
  (format t "~%=== EXEMPLE: Condition (x > 10) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 15 :R0)       ; x = 15
                    (:LOADI 10 :R1)       ; seuil = 10
                    (:CMP :R0 :R1)        ; Compare x et 10
                    (:JGT THEN)           ; Si x > 10, aller à THEN
                    (:LOADI 200 :R2)      ; ELSE: R2 = 200
                    (:JMP END)
                    (:LABEL THEN)
                    (:LOADI 100 :R2)      ; THEN: R2 = 100
                    (:LABEL END)
                    (:PRINT :R2)          ; Affiche 100
                    (:HALT))
                  :verbose t)))

(defun example-memory ()
  "Exemple: Utilisation de la mémoire"
  (format t "~%=== EXEMPLE: Accès mémoire ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 42 :R0)       ; Valeur à stocker
                    (:LOADI 1000 :R1)     ; Adresse
                    (:STORE :R0 :R1)      ; MEM[1000] = 42
                    (:LOADI 0 :R0)        ; Efface R0
                    (:PRINT :R0)          ; Affiche 0
                    (:LOAD :R1 :R0)       ; R0 = MEM[1000]
                    (:PRINT :R0)          ; Affiche 42
                    (:HALT))
                  :verbose t)))

(defun example-loop-simple ()
  "Exemple: Boucle simple (compte de 1 à 5)"
  (format t "~%=== EXEMPLE: Boucle (1 à 5) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 1 :R0)        ; Compteur = 1
                    (:LOADI 5 :R1)        ; Limite = 5
                    (:LABEL LOOP)
                    (:PRINT :R0)          ; Affiche compteur
                    (:LOADI 1 :R2)        ; R2 = 1
                    (:ADD :R2 :R0)        ; R0 = R0 + 1
                    (:CMP :R0 :R1)        ; Compare compteur et limite
                    (:JGT END)            ; Si compteur > 5, fin
                    (:JMP LOOP)           ; Sinon, continuer la boucle
                    (:LABEL END)
                    (:HALT))
                  :verbose nil)))

(defun example-factorial-iteratif ()
  "Exemple: Factorielle itérative (5!)"
  (format t "~%=== EXEMPLE: Factorielle itérative (5!) ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 5 :R0)        ; n = 5
                    (:LOADI 1 :R1)        ; result = 1
                    (:LABEL LOOP)
                    (:LOADI 1 :R2)        ; R2 = 1
                    (:CMP :R0 :R2)        ; Compare n et 1
                    (:JLT END)            ; Si n < 1, fin
                    (:MUL :R0 :R1)        ; result = result * n
                    (:LOADI 1 :R2)        ; R2 = 1
                    (:SUB :R2 :R0)        ; n = n - 1
                    (:JMP LOOP)           ; Continuer
                    (:LABEL END)
                    (:PRINT :R1)          ; Affiche 120
                    (:HALT))
                  :verbose nil)))

(defun example-max ()
  "Exemple: Maximum de deux nombres"
  (format t "~%=== EXEMPLE: Maximum de deux nombres ===~%")
  (let ((vm (make-new-vm)))
    (load-and-run vm
                  '((:LOADI 42 :R0)       ; a = 42
                    (:LOADI 17 :R1)       ; b = 17
                    (:CMP :R0 :R1)        ; Compare a et b
                    (:JGT AMAX)           ; Si a > b, aller à AMAX
                    (:MOVE :R1 :R2)       ; max = b
                    (:JMP END)
                    (:LABEL AMAX)
                    (:MOVE :R0 :R2)       ; max = a
                    (:LABEL END)
                    (:PRINT :R2)          ; Affiche 42
                    (:HALT))
                  :verbose t)))

(defun example-somme-tableau ()
  "Exemple: Somme des éléments d'un tableau en mémoire"
  (format t "~%=== EXEMPLE: Somme d'un tableau ===~%")
  (let ((vm (make-new-vm)))
    ;; D'abord, charger les valeurs en mémoire
    (format t "Initialisation du tableau en mémoire...~%")
    (mem-write vm 1000 10)
    (mem-write vm 1001 20)
    (mem-write vm 1002 30)
    (mem-write vm 1003 40)
    (mem-write vm 1004 50)
    
    (load-and-run vm
                  '((:LOADI 0 :R0)        ; somme = 0
                    (:LOADI 1000 :R1)     ; adresse = 1000 (début)
                    (:LOADI 1005 :R2)     ; fin = 1005 (après dernier)
                    (:LABEL LOOP)
                    (:CMP :R1 :R2)        ; Compare adresse et fin
                    (:JGT END)            ; Si adresse >= fin, fin
                    (:LOAD :R1 :MEM)      ; MEM = valeur à l'adresse
                    (:ADD :MEM :R0)       ; somme = somme + MEM
                    (:LOADI 1 :MEM)       ; MEM = 1
                    (:ADD :MEM :R1)       ; adresse = adresse + 1
                    (:JMP LOOP)
                    (:LABEL END)
                    (:PRINT :R0)          ; Affiche 150
                    (:HALT))
                  :verbose nil)))

;;; ============================================================================
;;; EXÉCUTION DE TOUS LES EXEMPLES
;;; ============================================================================

(defun run-all-examples ()
  "Exécute tous les exemples"
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                        EXEMPLES DE PROGRAMMES ASM~%")
  (format t "================================================================================~%")
  
  (example-arithmetic)
  (example-conditional)
  (example-memory)
  (example-loop-simple)
  (example-factorial-iteratif)
  (example-max)
  (example-somme-tableau)
  
  (format t "~%")
  (format t "================================================================================~%")
  (format t "                        FIN DES EXEMPLES~%")
  (format t "================================================================================~%"))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(example-arithmetic example-conditional example-memory
          example-loop-simple example-factorial-iteratif example-max
          example-somme-tableau run-all-examples))
