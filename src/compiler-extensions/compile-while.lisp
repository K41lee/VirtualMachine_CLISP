;;;; compile-while.lisp
;;;; Extension du compilateur pour supporter WHILE loops
;;;; Phase 11 - Compilation de la VM

;;; ============================================================================
;;; WHILE LOOP
;;; ============================================================================

(defun compile-while (condition body env)
  "Compile une boucle WHILE
   
   Syntaxe: (while condition body)
   
   Génère du code équivalent à:
   LOOP_START:
     évaluer condition
     si condition = 0, sauter vers LOOP_END
     exécuter body
     sauter vers LOOP_START
   LOOP_END:
   
   Exemple:
   (while (< x 10)
     (progn
       (setq x (+ x 1))
       (print x)))
   
   Arguments:
   - condition: expression booléenne
   - body: expression à exécuter dans la boucle
   - env: environnement de compilation
   
   Retour:
   - Liste d'instructions MIPS"
  
  (let ((label-start (gen-label env "WHILE_START"))
        (label-end (gen-label env "WHILE_END"))
        (code '()))
    
    ;; LOOP_START:
    (setf code (append code (list (list :LABEL label-start))))
    
    ;; Compiler la condition
    (setf code (append code (compile-expr condition env)))
    
    ;; Si condition = 0 (faux), sauter vers LOOP_END
    ;; Résultat de la condition est dans *reg-v0*
    (setf code (append code
                      (list (list :BEQ *reg-v0* *reg-zero* label-end))))
    
    ;; Compiler le body
    (setf code (append code (compile-expr body env)))
    
    ;; Sauter vers LOOP_START (recommencer la boucle)
    (setf code (append code
                      (list (list :J label-start))))
    
    ;; LOOP_END:
    (setf code (append code (list (list :LABEL label-end))))
    
    ;; Le résultat d'un WHILE est nil (0)
    (setf code (append code
                      (list (list :MOVE *reg-zero* *reg-v0*))))
    
    code))

;;; ============================================================================
;;; INTÉGRATION DANS COMPILE-EXPR
;;; ============================================================================

;; INSTRUCTIONS POUR INTÉGRER WHILE DANS src/compiler.lisp:
;;
;; 1. Dans la fonction compile-expr, ajouter un cas pour WHILE:
;;
;;    ((eq head 'while)
;;     (compile-while (second parsed) (third parsed) env))
;;
;; 2. Exemple d'utilisation:
;;
;;    (compile-expr '(while (< x 10)
;;                     (progn
;;                       (setq x (+ x 1))
;;                       (print x)))
;;                  env)

;;; ============================================================================
;;; EXEMPLES DE CODE GÉNÉRÉ
;;; ============================================================================

;; Exemple 1: Boucle simple
;; Source: (while (< x 10) (setq x (+ x 1)))
;; 
;; Code MIPS généré:
;; WHILE_START_0:
;;   ;; Évaluer (< x 10)
;;   LW $t0 $fp -4          ; Charger x
;;   LI 10 $t1              ; Charger 10
;;   SLT $t0 $t1 $v0        ; $v0 = (x < 10)
;;   BEQ $v0 $zero WHILE_END_0  ; Si faux, sortir
;;   
;;   ;; Évaluer (setq x (+ x 1))
;;   LW $t0 $fp -4          ; Charger x
;;   ADDI $t0 1 $t0         ; x + 1
;;   SW $t0 $fp -4          ; Sauvegarder x
;;   
;;   J WHILE_START_0        ; Recommencer
;; WHILE_END_0:
;;   MOVE $zero $v0         ; Résultat = nil

;; Exemple 2: Boucle avec body complexe
;; Source: (while condition (progn expr1 expr2 expr3))
;;
;; Le PROGN permet d'exécuter plusieurs expressions dans le body

;;; ============================================================================
;;; OPTIMISATIONS POSSIBLES
;;; ============================================================================

;; 1. WHILE avec condition constante true
;;    (while t body) -> boucle infinie
;;    Peut détecter et éviter d'évaluer la condition à chaque itération
;;
;; 2. WHILE avec condition simple
;;    (while var body) -> si var est une variable simple
;;    Peut optimiser l'accès à la variable
;;
;; 3. BREAK/CONTINUE (extension future)
;;    Ajouter support pour sortir prématurément de la boucle

;;; ============================================================================
;;; TESTS
;;; ============================================================================

;; Test 1: Boucle simple compteur
;; (let ((x 0))
;;   (while (< x 5)
;;     (setq x (+ x 1)))
;;   x)
;; Résultat attendu: 5

;; Test 2: Boucle avec accumulation
;; (let ((sum 0)
;;       (i 1))
;;   (while (<= i 10)
;;     (progn
;;       (setq sum (+ sum i))
;;       (setq i (+ i 1))))
;;   sum)
;; Résultat attendu: 55

;; Test 3: Boucle infinie avec condition false initiale
;; (while nil
;;   (print "never"))
;; Résultat attendu: nil (ne devrait jamais imprimer)

;; Test 4: Boucle imbriquée
;; (let ((i 0)
;;       (j 0)
;;       (sum 0))
;;   (while (< i 3)
;;     (progn
;;       (setq j 0)
;;       (while (< j 3)
;;         (progn
;;           (setq sum (+ sum 1))
;;           (setq j (+ j 1))))
;;       (setq i (+ i 1))))
;;   sum)
;; Résultat attendu: 9

;;; ============================================================================
;;; NOTES D'IMPLÉMENTATION
;;; ============================================================================

;; 1. WHILE vs LOOP
;;    - WHILE est plus simple que LOOP (pas de FOR, pas de COLLECT, etc.)
;;    - Suffisant pour les besoins de vm.lisp
;;    - LOOP peut être désucré en WHILE + LET

;; 2. Désucrage de LOOP en WHILE
;;    
;;    ;; LOOP simple:
;;    (loop while condition do body)
;;    ;; Devient:
;;    (while condition body)
;;    
;;    ;; LOOP avec FOR:
;;    (loop for i from 0 to 10 do body)
;;    ;; Devient:
;;    (let ((i 0))
;;      (while (<= i 10)
;;        (progn
;;          body
;;          (setq i (+ i 1)))))

;; 3. Condition d'arrêt
;;    - La condition doit retourner 0 (faux) ou non-zéro (vrai)
;;    - Les opérateurs de comparaison (<, >, =, etc.) retournent 0 ou 1
;;    - nil est traité comme 0, tout le reste comme vrai

;; 4. Variables globales utilisées
;;    - *reg-v0*: Registre pour le résultat de la condition
;;    - *reg-zero*: Registre toujours à 0
;;    - gen-label: Fonction pour générer des labels uniques
;;    - compile-expr: Fonction récursive de compilation

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export 'compile-while)
