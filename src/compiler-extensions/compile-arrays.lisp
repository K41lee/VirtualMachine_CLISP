;;;; compile-arrays.lisp
;;;; Extension du compilateur pour supporter ARRAYS
;;;; Phase 11 - Compilation de la VM

;;; ============================================================================
;;; STRATÉGIE D'IMPLÉMENTATION DES ARRAYS
;;; ============================================================================

;; Les arrays en LISP sont des structures complexes avec:
;; - Taille dynamique
;; - Type des éléments
;; - Dimensions (1D, 2D, etc.)
;; - Métadonnées (fill-pointer, adjustable, etc.)
;;
;; Pour la VM, on simplifie:
;; - Arrays 1D uniquement
;; - Taille fixe (pas de redimensionnement)
;; - Éléments de type entier uniquement
;; - Stockage sur le tas avec métadonnées

;;; ============================================================================
;;; REPRÉSENTATION EN MÉMOIRE
;;; ============================================================================

;; Un array est stocké sur le tas avec cette structure:
;;
;; Adresse    Contenu
;; --------   ---------
;; base+0     MAGIC (0xA88A pour identifier un array)
;; base+1     SIZE (nombre d'éléments)
;; base+2     element[0]
;; base+3     element[1]
;; ...
;; base+n+1   element[n-1]
;;
;; Total: 2 + SIZE mots mémoire

(defconstant +array-magic+ #xA88A
  "Valeur magique pour identifier un array en mémoire")

;;; ============================================================================
;;; MAKE-ARRAY
;;; ============================================================================

;; Syntaxe: (make-array size [:initial-element value])
;;
;; Exemple:
;;   (make-array 10)                        ; array de 10 éléments à 0
;;   (make-array 5 :initial-element 42)    ; array de 5 éléments à 42
;;
;; Compilation:
;;   1. Évaluer size
;;   2. Allouer (2 + size) mots sur le tas
;;   3. Écrire MAGIC à base+0
;;   4. Écrire SIZE à base+1
;;   5. Initialiser les éléments (si :initial-element fourni)
;;   6. Retourner l'adresse base dans $v0

(defun compile-make-array (args env)
  "Compile (make-array size [:initial-element value])
   
   Arguments:
   - args: liste (size [:initial-element value])
   - env: environnement de compilation
   
   Retour:
   - Code MIPS généré
   - Résultat dans $v0: adresse du array sur le tas"
  
  (let ((size-expr (first args))
        (init-value (if (and (>= (length args) 3)
                            (eq (second args) :initial-element))
                        (third args)
                        0))  ; Par défaut, initialiser à 0
        (code '()))
    
    ;; 1. Compiler et évaluer size
    (setf code (append code (compile-expr size-expr env)))
    ;; size est maintenant dans $v0
    
    ;; 2. Sauvegarder size dans $t0 pour calculs
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t0*))))
    
    ;; 3. Calculer taille totale: 2 + size (header + éléments)
    (setf code (append code (list (list :ADDI *reg-t0* 2 *reg-t1*))))
    ;; $t1 contient maintenant la taille totale à allouer
    
    ;; 4. Allouer sur le tas avec MALLOC
    ;; MALLOC attend size dans premier argument, retourne adresse dans $v0
    (setf code (append code (list (list :MOVE *reg-t1* *reg-a0*))))
    (setf code (append code (list (list :MALLOC *reg-a0* *reg-v0*))))
    ;; $v0 contient maintenant l'adresse de base du array
    
    ;; 5. Sauvegarder l'adresse de base dans $t2
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t2*))))
    
    ;; 6. Écrire MAGIC à base+0
    (setf code (append code (list (list :LI +array-magic+ *reg-t3*))))
    (setf code (append code (list (list :SW *reg-t3* *reg-t2* 0))))
    
    ;; 7. Écrire SIZE à base+1
    (setf code (append code (list (list :SW *reg-t0* *reg-t2* 1))))
    
    ;; 8. Initialiser les éléments si init-value fourni
    (unless (and (numberp init-value) (zerop init-value))
      ;; Si init-value n'est pas 0, on doit initialiser
      
      ;; Compiler init-value
      (let ((init-code (if (numberp init-value)
                           (list (list :LI init-value *reg-t3*))
                           (compile-expr init-value env))))
        (setf code (append code init-code)))
      ;; Valeur d'initialisation dans $t3 (ou $v0 si expression)
      (unless (numberp init-value)
        (setf code (append code (list (list :MOVE *reg-v0* *reg-t3*)))))
      
      ;; Boucle d'initialisation
      ;; for (i = 0; i < size; i++) array[i] = init-value
      (let ((label-loop-start (gen-label env "ARRAY_INIT_LOOP"))
            (label-loop-end (gen-label env "ARRAY_INIT_END")))
        
        ;; i = 0 (compteur dans $t4)
        (setf code (append code (list (list :MOVE *reg-zero* *reg-t4*))))
        
        ;; LOOP_START:
        (setf code (append code (list (list :LABEL label-loop-start))))
        
        ;; if (i >= size) goto LOOP_END
        (setf code (append code (list (list :SLT *reg-t4* *reg-t0* *reg-t5*))))
        ;; $t5 = 1 si i < size, 0 sinon
        (setf code (append code (list (list :BEQ *reg-t5* *reg-zero* label-loop-end))))
        
        ;; array[i+2] = init-value (offset +2 pour header)
        (setf code (append code (list (list :ADDI *reg-t4* 2 *reg-t6*))))
        (setf code (append code (list (list :ADD *reg-t2* *reg-t6* *reg-t6*))))
        (setf code (append code (list (list :SW *reg-t3* *reg-t6* 0))))
        
        ;; i++
        (setf code (append code (list (list :ADDI *reg-t4* 1 *reg-t4*))))
        
        ;; goto LOOP_START
        (setf code (append code (list (list :J label-loop-start))))
        
        ;; LOOP_END:
        (setf code (append code (list (list :LABEL label-loop-end))))))
    
    ;; 9. Retourner l'adresse du array dans $v0
    (setf code (append code (list (list :MOVE *reg-t2* *reg-v0*))))
    
    code))

;;; ============================================================================
;;; AREF
;;; ============================================================================

;; Syntaxe: (aref array index)
;;
;; Exemple:
;;   (aref my-array 0)    ; lire élément 0
;;   (aref my-array i)    ; lire élément i
;;
;; Compilation:
;;   1. Évaluer array (obtenir adresse)
;;   2. Évaluer index
;;   3. Vérifier bounds (optionnel)
;;   4. Calculer adresse effective: base + 2 + index
;;   5. Lire la valeur à cette adresse
;;   6. Retourner dans $v0

(defun compile-aref (array-expr index-expr env)
  "Compile (aref array index)
   
   Arguments:
   - array-expr: expression qui retourne l'adresse du array
   - index-expr: expression qui retourne l'index
   - env: environnement de compilation
   
   Retour:
   - Code MIPS généré
   - Résultat dans $v0: valeur de array[index]"
  
  (let ((code '()))
    
    ;; 1. Compiler et évaluer array (adresse dans $v0)
    (setf code (append code (compile-expr array-expr env)))
    
    ;; 2. Sauvegarder adresse array dans $t0
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t0*))))
    
    ;; 3. Compiler et évaluer index
    (setf code (append code (compile-expr index-expr env)))
    ;; index dans $v0
    
    ;; 4. Sauvegarder index dans $t1
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t1*))))
    
    ;; 5. Vérifier bounds (optionnel - pour debug)
    ;; TODO: Ajouter vérification si nécessaire
    
    ;; 6. Calculer offset réel: index + 2 (pour header)
    (setf code (append code (list (list :ADDI *reg-t1* 2 *reg-t2*))))
    
    ;; 7. Calculer adresse effective: base + offset
    (setf code (append code (list (list :ADD *reg-t0* *reg-t2* *reg-t3*))))
    
    ;; 8. Lire la valeur (LW dest base offset)
    (setf code (append code (list (list :LW *reg-v0* *reg-t3* 0))))
    
    code))

;;; ============================================================================
;;; SETF AREF
;;; ============================================================================

;; Syntaxe: (setf (aref array index) value)
;;
;; Exemple:
;;   (setf (aref my-array 0) 42)    ; écrire 42 dans élément 0
;;   (setf (aref my-array i) x)     ; écrire x dans élément i
;;
;; Compilation:
;;   1. Évaluer value
;;   2. Évaluer array (obtenir adresse)
;;   3. Évaluer index
;;   4. Calculer adresse effective: base + 2 + index
;;   5. Écrire value à cette adresse
;;   6. Retourner value dans $v0

(defun compile-setf-aref (array-expr index-expr value-expr env)
  "Compile (setf (aref array index) value)
   
   Arguments:
   - array-expr: expression qui retourne l'adresse du array
   - index-expr: expression qui retourne l'index
   - value-expr: expression qui retourne la valeur à écrire
   - env: environnement de compilation
   
   Retour:
   - Code MIPS généré
   - Résultat dans $v0: value (comme SETF)"
  
  (let ((code '()))
    
    ;; 1. Compiler et évaluer value (pour éviter écrasement registres)
    (setf code (append code (compile-expr value-expr env)))
    
    ;; 2. Sauvegarder value dans $t0
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t0*))))
    
    ;; 3. Compiler et évaluer array
    (setf code (append code (compile-expr array-expr env)))
    
    ;; 4. Sauvegarder adresse array dans $t1
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t1*))))
    
    ;; 5. Compiler et évaluer index
    (setf code (append code (compile-expr index-expr env)))
    
    ;; 6. Sauvegarder index dans $t2
    (setf code (append code (list (list :MOVE *reg-v0* *reg-t2*))))
    
    ;; 7. Calculer offset réel: index + 2 (pour header)
    (setf code (append code (list (list :ADDI *reg-t2* 2 *reg-t3*))))
    
    ;; 8. Calculer adresse effective: base + offset
    (setf code (append code (list (list :ADD *reg-t1* *reg-t3* *reg-t3*))))
    
    ;; 9. Écrire value à cette adresse (SW src base offset)
    (setf code (append code (list (list :SW *reg-t0* *reg-t3* 0))))
    
    ;; 10. Retourner value dans $v0 (comportement SETF)
    (setf code (append code (list (list :MOVE *reg-t0* *reg-v0*))))
    
    code))

;;; ============================================================================
;;; INTÉGRATION DANS COMPILE-EXPR
;;; ============================================================================

;; INSTRUCTIONS POUR INTÉGRER ARRAYS DANS src/compiler.lisp:
;;
;; 1. Dans parse-lisp-expr, ajouter:
;;
;;    (make-array
;;     (list :make-array args))
;;
;; 2. Pour AREF, deux cas:
;;    - Lecture: (aref array index) -> :aref
;;    - Écriture via SETF: géré dans compile-setf
;;
;;    (aref
;;     (list :aref (first args) (second args)))
;;
;; 3. Dans compile-expr, ajouter:
;;
;;    (:make-array
;;     (compile-make-array (second parsed) env))
;;
;;    (:aref
;;     (compile-aref (second parsed) (third parsed) env))
;;
;; 4. Dans compile-setf, détecter cas (aref ...):
;;
;;    (if (and (listp place) (eq (first place) 'aref))
;;        (compile-setf-aref (second place) (third place) value env)
;;        ...)

;;; ============================================================================
;;; EXEMPLES DE CODE GÉNÉRÉ
;;; ============================================================================

;; Exemple 1: Créer array simple
;; Source: (make-array 10)
;;
;; Code MIPS:
;;   LI 10 $v0              ; size = 10
;;   MOVE $v0 $t0           ; sauvegarder size
;;   ADDI $t0 2 $t1         ; total = 10 + 2 = 12
;;   MOVE $t1 $a0           ; préparer argument MALLOC
;;   MALLOC $a0 $v0         ; allouer, résultat dans $v0
;;   MOVE $v0 $t2           ; sauvegarder base
;;   LI 43146 $t3           ; MAGIC = 0xA88A
;;   SW $t3 $t2 0           ; array[0] = MAGIC
;;   SW $t0 $t2 1           ; array[1] = SIZE
;;   MOVE $t2 $v0           ; retourner adresse

;; Exemple 2: Lire élément
;; Source: (aref my-array 5)
;;
;; Code MIPS:
;;   ... (code pour my-array -> $v0)
;;   MOVE $v0 $t0           ; adresse array
;;   LI 5 $v0               ; index = 5
;;   MOVE $v0 $t1           ; sauvegarder index
;;   ADDI $t1 2 $t2         ; offset = 5 + 2 = 7
;;   ADD $t0 $t2 $t3        ; adresse = base + 7
;;   LW $v0 $t3 0           ; lire valeur

;; Exemple 3: Écrire élément
;; Source: (setf (aref my-array 3) 42)
;;
;; Code MIPS:
;;   LI 42 $v0              ; value = 42
;;   MOVE $v0 $t0           ; sauvegarder value
;;   ... (code pour my-array -> $v0)
;;   MOVE $v0 $t1           ; adresse array
;;   LI 3 $v0               ; index = 3
;;   MOVE $v0 $t2           ; sauvegarder index
;;   ADDI $t2 2 $t3         ; offset = 3 + 2 = 5
;;   ADD $t1 $t3 $t3        ; adresse = base + 5
;;   SW $t0 $t3 0           ; écrire value
;;   MOVE $t0 $v0           ; retourner value

;;; ============================================================================
;;; TESTS
;;; ============================================================================

;; Test 1: Créer et lire
;; (let ((arr (make-array 5)))
;;   (aref arr 0))
;; Résultat attendu: 0 (initialisation par défaut)

;; Test 2: Créer avec valeur initiale
;; (let ((arr (make-array 3 :initial-element 42)))
;;   (aref arr 0))
;; Résultat attendu: 42

;; Test 3: Écrire et lire
;; (let ((arr (make-array 5)))
;;   (setf (aref arr 2) 99)
;;   (aref arr 2))
;; Résultat attendu: 99

;; Test 4: Boucle avec array
;; (let ((arr (make-array 5)))
;;   (let ((i 0))
;;     (while (< i 5)
;;       (progn
;;         (setf (aref arr i) (* i 10))
;;         (setq i (+ i 1)))))
;;   (aref arr 3))
;; Résultat attendu: 30

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(compile-make-array compile-aref compile-setf-aref))
