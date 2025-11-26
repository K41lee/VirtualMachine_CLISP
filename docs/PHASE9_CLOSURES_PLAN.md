# Phase 9: Implémentation des CLOSURES

**Date début:** 26 novembre 2025  
**Statut:** 📋 PLANIFICATION  
**Durée estimée:** 20-30 heures  
**Complexité:** ★★★★★ (Très élevée)  
**Priorité:** HAUTE (spécification obligatoire)

---

## 🎯 Objectif Global

Implémenter le support complet des **fermetures (closures)** en LISP, permettant:
- Capture de variables libres
- Fonctions retournant des fonctions
- Closures imbriquées
- État capturé modifiable

**Exemple cible:**
```lisp
;; Closure basique - Fonction retournant fonction
(setq add5 ((lambda (x) (lambda (y) (+ x y))) 5))
(funcall add5 3)  → 8

;; Closure avec état
(setq counter (let ((count 0))
                (lambda () (setq count (+ count 1)))))
(funcall counter)  → 1
(funcall counter)  → 2
(funcall counter)  → 3
```

---

## 📚 Contexte et Prérequis

### État Actuel (Phase 8 Complétée)

✅ **Déjà implémenté:**
- LABELS avec static links corrects (70/70 tests)
- Fonctions locales avec accès variables englobantes
- Frame layout avec static link (FP+8)
- Appels siblings vs enfants correctement gérés

⚠️ **Limitations actuelles:**
- Pas de support LAMBDA
- Pas de capture de variables (seulement accès via static link)
- Pas de tas dynamique (allocation sur pile uniquement)
- Fonctions ne peuvent pas être retournées comme valeurs

### Concepts Théoriques

**Closure = Code + Environnement**
- **Code:** Instructions de la fonction
- **Environnement:** Valeurs des variables capturées

**Variable Libre:**
Une variable utilisée dans une fonction mais non définie localement.

```lisp
(lambda (y) (+ x y))
;; x est libre (non dans les paramètres)
;; y est liée (paramètre)
```

**Stratégies d'Implémentation:**

1. **Copie par valeur** (choisi pour simplicité):
   - Copier valeurs variables libres dans la closure
   - Simple à implémenter
   - Pas de partage d'état entre closures
   
2. **Référence indirecte** (avancé):
   - Stocker pointeurs vers variables
   - Partage d'état possible
   - Plus complexe (nécessite garbage collection)

---

## 🗺️ Plan d'Action Détaillé

### ÉTAPE 1: Conception Théorique (3-4 heures)

#### 1.1 Définir Structure Closure (1h)

**Représentation mémoire:**
```
Adresse    Contenu               Description
────────────────────────────────────────────────
addr+0     CODE_LABEL           Label de la fonction lambda
addr+4     ENV_SIZE             Nombre de variables capturées
addr+8     VAR_1_VALUE          Valeur variable capturée 1
addr+12    VAR_2_VALUE          Valeur variable capturée 2
...
addr+4n    VAR_N_VALUE          Valeur variable capturée N
```

**Type Closure:**
```lisp
(defstruct closure
  "Structure représentant une closure"
  (code-label nil)     ; Label ASM de la fonction
  (env-size 0)         ; Nombre variables capturées
  (heap-addr 0))       ; Adresse sur le tas
```

#### 1.2 Concevoir Gestion du Tas (1h)

**Zone mémoire:**
- Début: 1000
- Fin: 2999
- Taille: 2000 mots (8 KB)

**Allocateur simple:**
```lisp
(defvar *heap-start* 1000)
(defvar *heap-pointer* 1000)  ; Pointeur courant (bump allocator)
(defvar *heap-limit* 3000)

(defun heap-alloc (size)
  "Alloue size mots, retourne adresse"
  (when (>= (+ *heap-pointer* size) *heap-limit*)
    (error "Heap overflow"))
  (let ((addr *heap-pointer*))
    (incf *heap-pointer* size)
    addr))
```

**Pas de garbage collection** (phase 9):
- Allocateur "bump" simple
- Libération manuelle si nécessaire
- GC optionnel pour phase avancée

#### 1.3 Définir Compilation LAMBDA (1h)

**Pipeline de compilation:**
```
(lambda (params) body)
    ↓
1. Analyser body → trouver variables libres
    ↓
2. Allouer closure sur tas (taille = 2 + nb_vars_libres)
    ↓
3. Stocker code_label dans closure[0]
    ↓
4. Stocker env_size dans closure[1]
    ↓
5. Pour chaque variable libre:
   - Charger sa valeur depuis environnement
   - Stocker dans closure[2+i]
    ↓
6. Générer code de la fonction:
   - Prologue: récupérer environnement depuis closure
   - Corps: utiliser env + params
   - Épilogue: retourner résultat
    ↓
7. Retourner adresse closure dans $V0
```

**Appel de closure (FUNCALL):**
```
(funcall closure-value arg1 arg2 ...)
    ↓
1. Charger closure[0] → code_label
2. Charger closure[1] → env_size
3. Préparer arguments dans $A0-$A3
4. Passer adresse closure dans registre spécial ($S1)
5. JAL code_label
```

#### 1.4 Créer Exemples et Diagrammes (30min)

**Exemple complet:**
```lisp
;; Code source
(defun make-adder (x)
  (lambda (y) (+ x y)))

(setq add5 (make-adder 5))
(funcall add5 3)  ; → 8

;; Mémoire après (make-adder 5)
Tas:
1000: 6000        ; code_label = LAMBDA_0
1004: 1           ; env_size = 1
1008: 5           ; x = 5

Code:
6000: LAMBDA_0:
      ; Récupérer x depuis closure
      LW $S1 8 $T0      ; $T0 = closure[2] = x = 5
      ; Calculer x + y
      MOVE $A0 $T1      ; $T1 = y (paramètre)
      ADD $T0 $T1 $V0   ; $V0 = x + y
      JR $RA
```

**Diagramme:**
```
┌─────────────────────────────────────────┐
│ Environnement Global                    │
│ add5 → [heap:1000]                      │
└─────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────┐
│ Tas (1000-2999)                         │
│                                         │
│ 1000: [LAMBDA_0] ← Code                 │
│ 1004: [1]        ← Env size             │
│ 1008: [5]        ← Captured: x=5        │
│ 1012: [libre]                           │
│ ...                                     │
└─────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────┐
│ Code Segment                            │
│                                         │
│ LAMBDA_0:                               │
│   LW $S1 8 $T0    ; Charger x           │
│   MOVE $A0 $T1    ; Charger y           │
│   ADD $T0 $T1 $V0 ; x + y               │
│   JR $RA                                │
└─────────────────────────────────────────┘
```

**Livrable Étape 1:**
- [ ] Document `docs/CLOSURES_DESIGN.md` (10-15 pages)
- [ ] Diagrammes structure mémoire
- [ ] Exemples détaillés avec traces
- [ ] Décisions techniques documentées

---

### ÉTAPE 2: Extension VM - Tas Dynamique (5-6 heures)

#### 2.1 Ajouter Instructions Tas (2h)

**Fichier:** `src/asm-ops.lisp`

**Nouvelles instructions:**
```lisp
;; Allocation
(:MALLOC size-reg result-reg)
  ; Alloue [size-reg] mots sur le tas
  ; Adresse retournée dans [result-reg]

;; Lecture
(:LOAD-HEAP addr-reg offset result-reg)
  ; Charge mémoire[[addr-reg] + offset] → [result-reg]

;; Écriture
(:STORE-HEAP value-reg addr-reg offset)
  ; Sauvegarde [value-reg] → mémoire[[addr-reg] + offset]
```

**Modifications:**
```lisp
;; Dans *asm-ops*
(defparameter *asm-ops* 
  '(
    ;; ... instructions existantes ...
    
    ;; Nouvelles instructions tas
    (:MALLOC 2)      ; MALLOC size result
    (:LOAD-HEAP 3)   ; LOAD-HEAP addr offset result
    (:STORE-HEAP 3)  ; STORE-HEAP value addr offset
  ))
```

#### 2.2 Implémenter Exécution Instructions (2h)

**Fichier:** `src/vm.lisp`

**Ajouter gestionnaire tas:**
```lisp
(defun init-heap (vm)
  "Initialise le tas de la VM"
  (setf (vm-heap-start vm) 1000)
  (setf (vm-heap-pointer vm) 1000)
  (setf (vm-heap-limit vm) 3000))

(defun vm-malloc (vm size)
  "Alloue size mots sur le tas, retourne adresse"
  (let ((addr (vm-heap-pointer vm)))
    (when (>= (+ addr size) (vm-heap-limit vm))
      (error "HEAP OVERFLOW: Impossible d'allouer ~A mots" size))
    (setf (vm-heap-pointer vm) (+ addr size))
    addr))
```

**Étendre execute-instruction:**
```lisp
(defun execute-instruction (vm instr)
  (case (first instr)
    ;; ... instructions existantes ...
    
    (:MALLOC
     (let* ((size-reg (second instr))
            (result-reg (third instr))
            (size (get-register vm size-reg))
            (addr (vm-malloc vm size)))
       (set-register vm result-reg addr)))
    
    (:LOAD-HEAP
     (let* ((addr-reg (second instr))
            (offset (third instr))
            (result-reg (fourth instr))
            (addr (+ (get-register vm addr-reg) offset))
            (value (aref (vm-memory vm) addr)))
       (set-register vm result-reg value)))
    
    (:STORE-HEAP
     (let* ((value-reg (second instr))
            (addr-reg (third instr))
            (offset (fourth instr))
            (value (get-register vm value-reg))
            (addr (+ (get-register vm addr-reg) offset)))
       (setf (aref (vm-memory vm) addr) value)))
    
    ;; ... rest ...
    ))
```

#### 2.3 Tester Instructions Tas (1-2h)

**Fichier:** `tests/unit/test-heap.lisp`

**Tests à créer:**
```lisp
(defun test-malloc-simple ()
  "Test allocation simple"
  (let ((vm (make-new-vm)))
    (init-heap vm)
    (vm-malloc vm 10)  ; Alloue 10 mots
    (assert (= (vm-heap-pointer vm) 1010))
    (format t "✓ test-malloc-simple~%")))

(defun test-heap-read-write ()
  "Test lecture/écriture tas"
  (compile-and-run '(progn
    ;; Allouer 3 mots
    (let ((addr (malloc 3)))
      ;; Écrire valeurs
      (store-heap 42 addr 0)
      (store-heap 99 addr 1)
      (store-heap 17 addr 2)
      ;; Lire et vérifier
      (+ (load-heap addr 0)
         (load-heap addr 1)
         (load-heap addr 2)))))
  ;; Résultat: 42 + 99 + 17 = 158
  )

(defun test-heap-overflow ()
  "Test dépassement tas"
  (let ((vm (make-new-vm)))
    (init-heap vm)
    (handler-case
        (vm-malloc vm 2001)  ; Trop grand
      (error (e) 
        (format t "✓ test-heap-overflow: ~A~%" e)))))
```

**Validation:**
- [ ] test-malloc-simple passe
- [ ] test-heap-read-write passe
- [ ] test-heap-overflow détecte erreur
- [ ] Aucune régression (70/70 tests existants)

**Livrable Étape 2:**
- [ ] Instructions MALLOC, LOAD-HEAP, STORE-HEAP opérationnelles
- [ ] Tests tas (5+ tests)
- [ ] Documentation instructions dans code
- [ ] Aucune régression

---

### ÉTAPE 3: Analyse Variables Libres (4-5 heures)

#### 3.1 Implémenter free-variables (2h)

**Fichier:** `src/compiler.lisp`

**Fonction principale:**
```lisp
(defun free-variables (expr &optional (bound-vars '()))
  "Retourne la liste des variables libres dans expr.
   bound-vars = variables liées dans le scope actuel"
  (cond
    ;; Variable simple
    ((symbolp expr)
     (if (member expr bound-vars) '() (list expr)))
    
    ;; Constante (nombre, nil, t, etc.)
    ((atom expr) '())
    
    ;; Lambda
    ((eq (first expr) 'lambda)
     (let ((params (second expr))
           (body (cddr expr)))
       (remove-duplicates
         (apply #'append
                (mapcar (lambda (e) 
                          (free-variables e (append params bound-vars)))
                        body)))))
    
    ;; Let
    ((eq (first expr) 'let)
     (let* ((bindings (second expr))
            (body (cddr expr))
            (vars (mapcar #'first bindings))
            (vals (mapcar #'second bindings)))
       ;; Variables libres dans les valeurs + dans le corps
       (append
         (apply #'append (mapcar (lambda (v) (free-variables v bound-vars)) vals))
         (apply #'append (mapcar (lambda (e) 
                                   (free-variables e (append vars bound-vars)))
                                 body)))))
    
    ;; Labels
    ((eq (first expr) 'labels)
     (let* ((definitions (second expr))
            (body (cddr expr))
            (fn-names (mapcar #'first definitions)))
       (append
         ;; Variables libres dans les définitions
         (apply #'append
                (mapcar (lambda (def)
                          (let ((params (second def))
                                (fn-body (cddr def)))
                            (apply #'append
                                   (mapcar (lambda (e)
                                             (free-variables e 
                                               (append params fn-names bound-vars)))
                                           fn-body))))
                        definitions))
         ;; Variables libres dans le corps
         (apply #'append
                (mapcar (lambda (e)
                          (free-variables e (append fn-names bound-vars)))
                        body)))))
    
    ;; Application (appel de fonction)
    (t
     (remove-duplicates
       (apply #'append
              (mapcar (lambda (sub-expr) (free-variables sub-expr bound-vars))
                      expr))))))
```

#### 3.2 Tester free-variables (1h)

**Tests:**
```lisp
(defun test-free-vars ()
  ;; Variable simple libre
  (assert (equal (free-variables 'x) '(x)))
  
  ;; Variable liée
  (assert (equal (free-variables '(lambda (x) x)) '()))
  
  ;; Variable libre dans lambda
  (assert (equal (free-variables '(lambda (x) y)) '(y)))
  
  ;; Multiples variables
  (assert (equal (sort (free-variables '(lambda (x) (+ x y z))) #'string<)
                 '(y z)))
  
  ;; Lambda imbriquée
  (assert (equal (free-variables '(lambda (x) (lambda (y) (+ x y)))) '()))
  
  ;; Let
  (assert (equal (free-variables '(let ((x 1)) (+ x y))) '(y)))
  
  ;; Labels
  (assert (equal (free-variables '(labels ((f (x) (+ x y))) (f 5))) '(y)))
  
  (format t "✓ Tous les tests free-variables passent~%"))
```

#### 3.3 Intégrer à l'Environnement (1-2h)

**Ajouter à compiler-env:**
```lisp
(defstruct compiler-env
  ;; ... champs existants ...
  (captured-vars '())  ; Variables capturées dans la closure actuelle
  (closure-depth 0))   ; Profondeur closures imbriquées
```

**Helper functions:**
```lisp
(defun add-captured-var (env var value-code)
  "Ajoute une variable capturée à l'environnement"
  (push (cons var value-code) (compiler-env-captured-vars env)))

(defun lookup-captured-var (env var)
  "Recherche une variable capturée, retourne code pour y accéder"
  (cdr (assoc var (compiler-env-captured-vars env))))
```

**Livrable Étape 3:**
- [ ] Fonction free-variables complète
- [ ] Tests free-variables (10+ tests)
- [ ] Intégration environnement compilateur
- [ ] Documentation algorithme

---

### ÉTAPE 4: Compilation LAMBDA (6-8 heures)

#### 4.1 Compiler Lambda Simple (2h)

**Fichier:** `src/compiler.lisp`

**Ajouter à compile-expr:**
```lisp
(defun compile-expr (expr env)
  (let ((parsed (parse-lisp-expr expr)))
    (case (first parsed)
      ;; ... cas existants ...
      
      (:lambda
       (compile-lambda (second parsed) (third parsed) env))
      
      ;; ... rest ...
      )))
```

**Fonction compile-lambda:**
```lisp
(defun compile-lambda (params body env)
  "Compile (lambda params body) en closure
   Retourne code qui crée closure sur tas et retourne son adresse dans $V0"
  (let* ((free-vars (remove-duplicates 
                      (apply #'append 
                             (mapcar (lambda (e) (free-variables e params)) 
                                     body))))
         (closure-size (+ 2 (length free-vars)))  ; 2 = label + size
         (lambda-label (gen-label env "LAMBDA"))
         (skip-label (gen-label env "SKIP_LAMBDA"))
         (code '()))
    
    ;; 1. Allouer closure sur tas
    (setf code (append code
                      (list (list :LI closure-size *reg-t0*)
                            (list :MALLOC *reg-t0* *reg-v0*))))
    
    ;; 2. Stocker label de la fonction dans closure[0]
    (setf code (append code
                      (list (list :LA lambda-label *reg-t1*)  ; Load Address
                            (list :STORE-HEAP *reg-t1* *reg-v0* 0))))
    
    ;; 3. Stocker taille environnement dans closure[1]
    (setf code (append code
                      (list (list :LI (length free-vars) *reg-t1*)
                            (list :STORE-HEAP *reg-t1* *reg-v0* 1))))
    
    ;; 4. Capturer chaque variable libre
    (loop for var in free-vars
          for i from 2
          do (let ((var-code (compile-variable var env)))
               (setf code (append code
                                 var-code  ; Charger valeur dans $V0 temporairement
                                 (list (list :MOVE *reg-v0* *reg-t2*)
                                       (list :STORE-HEAP *reg-t2* *reg-v0* i))))))
    
    ;; 5. Sauter par-dessus le code de la fonction
    (setf code (append code (list (list :J skip-label))))
    
    ;; 6. Générer code de la fonction lambda
    (setf code (append code (compile-lambda-body lambda-label params body free-vars env)))
    
    ;; 7. Label de saut
    (setf code (append code (list (list :LABEL skip-label))))
    
    ;; Résultat: adresse closure dans $V0
    code))
```

#### 4.2 Compiler Corps Lambda (2h)

**Fonction compile-lambda-body:**
```lisp
(defun compile-lambda-body (label params body free-vars parent-env)
  "Génère le code de la fonction lambda
   Entrée: $A0-$A3 = paramètres
           $S1 = adresse closure (environnement capturé)
   Sortie: $V0 = résultat"
  (let ((code '())
        (lambda-env (copy-env parent-env)))
    
    ;; Label de début
    (setf code (append code (list (list :LABEL label))))
    
    ;; Prologue: sauvegarder $RA et créer frame
    (setf code (append code
                      (list (list :ADDI *reg-sp* -12 *reg-sp*)
                            (list :SW (get-reg :fp) *reg-sp* 0)
                            (list :SW *reg-ra* *reg-sp* 4)
                            (list :SW *reg-s1* *reg-sp* 8)  ; Sauver closure addr
                            (list :MOVE *reg-sp* (get-reg :fp))))))
    
    ;; Charger variables capturées depuis closure dans environnement
    (loop for var in free-vars
          for i from 2
          do (progn
               ;; Allouer emplacement pile pour la variable
               (setf code (append code
                                 (list (list :ADDI *reg-sp* -4 *reg-sp*)
                                       (list :LOAD-HEAP *reg-s1* i *reg-t0*)
                                       (list :SW *reg-t0* *reg-sp* 0))))
               ;; Ajouter à l'environnement
               (add-variable lambda-env var (cons :fp (- 12 (* 4 (1+ i)))))))
    
    ;; Charger paramètres dans environnement
    (loop for param in params
          for i from 0
          for arg-reg in (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*)
          do (progn
               (setf code (append code
                                 (list (list :ADDI *reg-sp* -4 *reg-sp*)
                                       (list :SW arg-reg *reg-sp* 0))))
               (add-variable lambda-env param (cons :fp (- 12 (* 4 (+ i (length free-vars) 1)))))))
    
    ;; Compiler le corps
    (dolist (expr body)
      (setf code (append code (compile-expr expr lambda-env))))
    
    ;; Épilogue: restaurer et retourner
    (setf code (append code
                      (list (list :MOVE (get-reg :fp) *reg-sp*)
                            (list :LW *reg-sp* 4 *reg-ra*)
                            (list :LW *reg-sp* 0 (get-reg :fp))
                            (list :ADDI *reg-sp* 12 *reg-sp*)
                            (list :JR *reg-ra*))))
    
    code))
```

#### 4.3 Compiler FUNCALL (2h)

**Ajouter à compile-expr:**
```lisp
(defun compile-expr (expr env)
  (case (first parsed)
    ;; ... cas existants ...
    
    (:funcall
     (compile-funcall (second parsed) (cddr parsed) env))
    
    ;; ... rest ...
    ))
```

**Fonction compile-funcall:**
```lisp
(defun compile-funcall (closure-expr args env)
  "Compile (funcall closure-expr arg1 arg2 ...)
   closure-expr évalue à une adresse de closure sur le tas"
  (let ((code '())
        (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*)))
    
    ;; 1. Évaluer l'expression closure → adresse dans $V0
    (setf code (append code (compile-expr closure-expr env)))
    (setf code (append code (list (list :MOVE *reg-v0* *reg-s1*))))  ; Sauver addr closure
    
    ;; 2. Compiler les arguments et les placer dans $A0-$A3
    (loop for arg in args
          for reg in arg-regs
          do (let ((arg-code (compile-expr arg env)))
               (setf code (append code
                                 arg-code
                                 (list (list :MOVE *reg-v0* reg))))))
    
    ;; 3. Charger le label de la fonction depuis closure[0]
    (setf code (append code
                      (list (list :LOAD-HEAP *reg-s1* 0 *reg-t0*))))  ; $T0 = code_label
    
    ;; 4. Appeler la fonction (adresse dans $T0, closure dans $S1)
    (setf code (append code
                      (list (list :JALR *reg-t0* *reg-ra*))))  ; Jump And Link Register
    
    code))
```

#### 4.4 Ajouter Instruction JALR (1h)

**Fichier:** `src/asm-ops.lisp`

```lisp
;; Ajouter instruction JALR (Jump And Link Register)
(:JALR 2)  ; JALR target-reg link-reg
```

**Fichier:** `src/vm.lisp`

```lisp
(defun execute-instruction (vm instr)
  (case (first instr)
    ;; ... instructions existantes ...
    
    (:JALR
     (let* ((target-reg (second instr))
            (link-reg (third instr))
            (target-addr (get-register vm target-reg))
            (return-addr (+ (get-register vm :$pc) 1)))
       (set-register vm link-reg return-addr)
       (set-register vm :$pc (1- target-addr))))  ; -1 car PC sera incrémenté
    
    ;; ... rest ...
    ))
```

#### 4.5 Tester Closures Basiques (1-2h)

**Tests:**
```lisp
(defun test-closure-simple ()
  "Test closure basique sans capture"
  (compile-and-run '(funcall (lambda (x) (* x 2)) 5))
  ;; Résultat: 10
  )

(defun test-closure-capture-une-var ()
  "Test capture d'une variable"
  (compile-and-run '(let ((y 3))
                      (funcall (lambda (x) (+ x y)) 5)))
  ;; Résultat: 8
  )

(defun test-closure-retour-fonction ()
  "Test fonction retournant fonction"
  (compile-and-run '(let ((adder (lambda (x) (lambda (y) (+ x y)))))
                      (funcall (funcall adder 5) 3)))
  ;; Résultat: 8
  )
```

**Livrable Étape 4:**
- [ ] compile-lambda opérationnel
- [ ] compile-funcall opérationnel
- [ ] Instruction JALR implémentée
- [ ] Tests closures basiques (5+ tests)

---

### ÉTAPE 5: Tests et Validation (2-3 heures)

#### 5.1 Tests Unitaires Complets (1h)

**Fichier:** `tests/unit/test-closures.lisp`

**10+ tests à créer:**
```lisp
1. (test-closure-no-capture)      ; Lambda sans capture
2. (test-closure-one-var)         ; Capture 1 variable
3. (test-closure-multiple-vars)   ; Capture plusieurs variables
4. (test-closure-nested)          ; Closures imbriquées
5. (test-closure-higher-order)    ; Fonction → fonction
6. (test-closure-currying)        ; Currying (partial application)
7. (test-closure-with-let)        ; Closure dans LET
8. (test-closure-with-labels)     ; Closure dans LABELS
9. (test-closure-recursive)       ; Closure récursive
10. (test-closure-state)          ; Closure avec état (compteur)
```

**Exemples détaillés:**
```lisp
(defun test-closure-currying ()
  "Test currying - Application partielle"
  (format t "=== Test Currying ===~%")
  (let ((result (compile-and-run 
                  '(let ((add (lambda (x) (lambda (y) (lambda (z) (+ x y z))))))
                     (funcall (funcall (funcall add 1) 2) 3)))))
    (assert (= (get-register result *reg-v0*) 6))
    (format t "✓ test-closure-currying: 6~%")))

(defun test-closure-state ()
  "Test closure avec état modifiable (compteur)"
  (format t "=== Test Closure État ===~%")
  ;; Note: Nécessite SETQ sur variables capturées (avancé)
  (compile-and-run 
    '(let ((count 0))
       (let ((counter (lambda () (setq count (+ count 1)))))
         (+ (funcall counter)
            (funcall counter)
            (funcall counter)))))
  ;; Résultat: 1 + 2 + 3 = 6
  )
```

#### 5.2 Tests de Non-Régression (30min)

**Vérifier:**
```bash
./run-unit-tests.sh

# Devrait afficher:
# Tests totaux     : 80+
# Tests réussis    : 80+ ✓
# Tests échoués    : 0 ✗
# Taux de réussite : 100%
```

#### 5.3 Tests d'Intégration (1h)

**Exemples complexes:**
```lisp
;; Exemple 1: Make-counter
(defun example-make-counter ()
  (compile-and-run 
    '(labels ((make-counter (start)
               (let ((count start))
                 (lambda () 
                   (setq count (+ count 1))
                   count))))
       (let ((c1 (make-counter 0))
             (c2 (make-counter 10)))
         (+ (funcall c1)    ; 1
            (funcall c1)    ; 2
            (funcall c2)    ; 11
            (funcall c1))))))  ; 3
  ;; Résultat: 1 + 2 + 11 + 3 = 17

;; Exemple 2: Fonction compose
(defun example-compose ()
  (compile-and-run
    '(labels ((compose (f g)
               (lambda (x) (funcall f (funcall g x)))))
       (let ((add1 (lambda (x) (+ x 1)))
             (mul2 (lambda (x) (* x 2))))
         (funcall (compose add1 mul2) 5)))))
  ;; Résultat: add1(mul2(5)) = add1(10) = 11
```

**Livrable Étape 5:**
- [ ] 10+ tests closures
- [ ] Tous tests passent (80+/80+)
- [ ] Tests intégration complexes
- [ ] Documentation exemples

---

## 📋 Checklist Globale Phase 9

### Préparation
- [ ] Lire spécifications closures dans SpecificationProjet.txt
- [ ] Comprendre concepts (variable libre, capture, environnement)
- [ ] Étudier exemples de référence

### Étape 1: Conception (3-4h)
- [ ] Structure closure définie
- [ ] Gestion tas conçue
- [ ] Pipeline compilation LAMBDA documenté
- [ ] Exemples et diagrammes créés
- [ ] Document CLOSURES_DESIGN.md complet

### Étape 2: Extension VM (5-6h)
- [ ] Instructions MALLOC, LOAD-HEAP, STORE-HEAP ajoutées
- [ ] Gestionnaire tas implémenté
- [ ] Instruction JALR ajoutée
- [ ] Tests tas (5+) passent
- [ ] Aucune régression (70/70)

### Étape 3: Analyse Variables (4-5h)
- [ ] Fonction free-variables implémentée
- [ ] Tests free-variables (10+) passent
- [ ] Intégration environnement compilateur
- [ ] Documentation algorithme

### Étape 4: Compilation LAMBDA (6-8h)
- [ ] compile-lambda implémenté
- [ ] compile-lambda-body implémenté
- [ ] compile-funcall implémenté
- [ ] Tests closures basiques (5+) passent

### Étape 5: Tests Validation (2-3h)
- [ ] Tests unitaires closures (10+) créés
- [ ] Tous tests passent (80+/80+)
- [ ] Tests intégration complexes passent
- [ ] Documentation mise à jour

### Finalisation
- [ ] Git commit avec message détaillé
- [ ] Documentation PHASE9_CLOSURES_COMPLETE.md
- [ ] Mise à jour ETAT_PROJET.md
- [ ] Prêt pour Phase 10 (optionnel)

---

## 🚨 Pièges à Éviter

### 1. Gestion Mémoire Tas
- ⚠️ **Piège:** Oublier de vérifier overflow
- ✅ **Solution:** Toujours vérifier `heap_pointer + size < heap_limit`

### 2. Variables Libres
- ⚠️ **Piège:** Confondre variables libres et variables capturées
- ✅ **Solution:** Variable libre = utilisée mais non définie localement

### 3. Ordre d'Évaluation
- ⚠️ **Piège:** Capturer mauvaises valeurs si ordre incorrect
- ✅ **Solution:** Capturer variables AVANT d'évaluer corps lambda

### 4. Registres
- ⚠️ **Piège:** Écraser registres temporaires
- ✅ **Solution:** Sauvegarder/restaurer $T0-$T9 si nécessaire

### 5. Static Link vs Closure
- ⚠️ **Piège:** Confondre static link (LABELS) et environnement closure
- ✅ **Solution:** Static link = frame parent, Closure = valeurs capturées sur tas

---

## 📊 Métriques de Succès

### Critères Obligatoires
- ✅ Lambda sans capture: `(lambda (x) x)`
- ✅ Lambda avec capture: `(lambda (x) (lambda (y) (+ x y)))`
- ✅ Funcall basique: `(funcall (lambda (x) (* x 2)) 5)`
- ✅ Fonction → fonction: `((lambda (x) (lambda (y) (+ x y))) 5)`

### Critères Avancés
- ⭐ Closures imbriquées 3+ niveaux
- ⭐ Currying complet
- ⭐ Closures récursives
- ⭐ État capturé modifiable (SETQ)

### Tests
- **Minimum:** 80+ tests passent (70 existants + 10 closures)
- **Optimal:** 90+ tests passent
- **Aucune régression**

---

## 🔧 Outils et Commandes

### Tests Rapides
```bash
# Test closure simple
clisp -q -x "(load \"main.lisp\") \
  (compile-and-run '(funcall (lambda (x) (* x 2)) 5))"

# Tous tests
./run-unit-tests.sh

# Tests closures seulement
clisp -q -x "(load \"main.lisp\") (load \"tests/unit/test-closures.lisp\")"
```

### Debug
```bash
# Voir code assembleur généré
clisp -q -x "(load \"main.lisp\") \
  (pprint (compile-lisp '(lambda (x) (* x 2))))"

# Examiner tas après exécution
clisp -q -x "(load \"main.lisp\") \
  (let ((vm (compile-and-run '(lambda (x) x)))) \
    (format t \"Heap pointer: ~A~%\" (vm-heap-pointer vm)))"
```

### Git
```bash
# Commit incrémental après chaque étape
git add -A
git commit -m "Phase 9 Étape X: ..."

# Branche dédiée (recommandé)
git checkout -b phase9-closures
```

---

## 📚 Références

### Documentation Interne
- `docs/PHASE8_LABELS_FIX.md`: Static links (prérequis)
- `docs/ETAT_PROJET_PHASE8.md`: État avant Phase 9
- `PLAN_ACTION_COMPLET.md`: Plan global 600+ lignes

### Concepts
- **Variable libre:** Utilisée mais non définie localement
- **Capture:** Copier valeur variable libre dans closure
- **Environnement:** Ensemble valeurs capturées
- **Tas (Heap):** Zone mémoire allocation dynamique

### Ressources Externes
- SICP Chapter 3.2: Environment Model
- "Compiling with Closures" - Andrew Appel

---

## 🎯 Estimation Temps Réaliste

| Étape | Temps Min | Temps Max | Médiane |
|-------|-----------|-----------|---------|
| 1. Conception | 3h | 4h | 3.5h |
| 2. Extension VM | 5h | 6h | 5.5h |
| 3. Variables Libres | 4h | 5h | 4.5h |
| 4. Compile LAMBDA | 6h | 8h | 7h |
| 5. Tests | 2h | 3h | 2.5h |
| **TOTAL** | **20h** | **26h** | **23h** |

**Ajout buffer 20%:** 23h × 1.2 = **27.6h ≈ 28h**

**Rythmes possibles:**
- **Intensif:** 7h/jour → 4 jours
- **Normal:** 4h/jour → 7 jours (1 semaine)
- **Relax:** 2h/jour → 14 jours (2 semaines)
- **Lent:** 1h/jour → 28 jours (4 semaines)

---

## ✅ Validation Finale Phase 9

### Avant de Passer à Phase 10

**Checklist complète:**
- [ ] 80+ tests passent (100%)
- [ ] Aucune régression
- [ ] Documentation complète
- [ ] Exemples fonctionnent
- [ ] Code propre et commenté
- [ ] Git commit avec tag `phase9-complete`

**Tests de validation:**
```lisp
;; Ces 5 exemples DOIVENT tous fonctionner
(compile-and-run '(funcall (lambda (x) x) 42))  → 42
(compile-and-run '(funcall (lambda (x) (* x 2)) 5))  → 10
(compile-and-run '(let ((y 3)) (funcall (lambda (x) (+ x y)) 5)))  → 8
(compile-and-run '(funcall ((lambda (x) (lambda (y) (+ x y))) 5) 3))  → 8
(compile-and-run '(funcall (funcall (funcall 
  (lambda (x) (lambda (y) (lambda (z) (+ x y z)))) 1) 2) 3))  → 6
```

---

**Prêt à démarrer la Phase 9 !** 🚀

Cette phase est la plus complexe mais aussi la plus gratifiante. Les closures sont au cœur des langages fonctionnels modernes. Bonne chance ! 💪
