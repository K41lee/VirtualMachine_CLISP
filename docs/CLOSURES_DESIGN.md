# Phase 9: Conception CLOSURES

**Date:** 26 novembre 2025  
**Statut:** 📐 CONCEPTION

---

## 🎯 Objectif

Implémenter les **fermetures (closures)** en LISP, permettant aux fonctions LAMBDA de capturer et utiliser des variables de leur environnement lexical.

---

## 🧠 Concepts Fondamentaux

### Qu'est-ce qu'une Closure?

Une **closure** est une fonction qui "capture" les variables de son environnement lexical au moment de sa création.

**Exemple:**
```lisp
(let ((x 5))
  (lambda (y) (+ x y)))
```

La fonction lambda capture `x` et peut l'utiliser même après la fin du `let`.

### Variables Libres vs Liées

- **Variable liée:** Définie dans la fonction (paramètre ou let local)
- **Variable libre:** Définie à l'extérieur, doit être capturée

**Exemple:**
```lisp
(lambda (x)           ; x est liée
  (lambda (y)         ; y est liée
    (+ x y z)))       ; z est libre (doit être capturée)
```

---

## 🏗️ Architecture Mémoire

### Zone Tas (Heap)

**Adresse:** 1000-2999 (2000 mots)  
**Usage:** Allocation dynamique des closures

```
Mémoire:
[0    - 999 ] : Données statiques
[1000 - 2999] : TAS (closures)
[3000 - 4899] : Disponible
[4900 - 4999] : PILE (stack)
[5000+      ] : CODE (instructions)
```

### Structure d'une Closure

```
+------------------+  ← Adresse closure (ex: 1000)
| Code Label       |  [0] Adresse du code de la fonction
+------------------+
| Env Size         |  [1] Nombre de variables capturées
+------------------+
| Var 0            |  [2] Valeur de la 1ère variable
+------------------+
| Var 1            |  [3] Valeur de la 2ème variable
+------------------+
| ...              |
+------------------+
```

**Taille:** 2 + nombre_variables_capturées

**Exemple concret:**
```lisp
(let ((x 5) (z 10))
  (lambda (y) (+ x y z)))
```

Closure en mémoire:
```
[1000] = LAMBDA_LABEL_1   ; Adresse du code
[1001] = 2                ; 2 variables capturées (x, z)
[1002] = 5                ; Valeur de x
[1003] = 10               ; Valeur de z
```

---

## 🔧 Instructions VM

### Nouvelles Instructions Tas

```lisp
;; Allocation
(:MALLOC size result-reg)
;; Alloue 'size' mots sur le tas
;; Retourne l'adresse dans result-reg
;; Exemple: (MALLOC 4 $V0) → $V0 = 1000

;; Lecture
(:LOAD-HEAP addr-reg offset result-reg)
;; Charge mémoire[addr-reg + offset] → result-reg
;; Exemple: (LOAD-HEAP $V0 2 $T0) → $T0 = mémoire[1002]

;; Écriture
(:STORE-HEAP value-reg addr-reg offset)
;; Sauvegarde value-reg → mémoire[addr-reg + offset]
;; Exemple: (STORE-HEAP $T0 $V0 2) → mémoire[1002] = $T0
```

### Gestionnaire Allocation

```lisp
*heap-pointer* : 1000  ; Pointeur courant
*heap-limit*   : 3000  ; Limite

Allocation:
  Si heap-pointer + size <= heap-limit:
    adresse = heap-pointer
    heap-pointer += size
    retourner adresse
  Sinon:
    ERREUR: Tas plein
```

---

## 📝 Compilation LAMBDA

### Étapes de Compilation

**Code source:**
```lisp
(lambda (y) (+ x y))  ; x est libre
```

**Étapes:**

1. **Analyser variables libres**
   - Paramètres: {y}
   - Variables utilisées: {x, y}
   - Variables libres: {x} (utilisée mais pas paramètre)

2. **Allouer closure sur tas**
   - Taille: 2 + 1 = 3 mots
   - MALLOC 3 → adresse dans $V0

3. **Stocker code label**
   - STORE-HEAP LAMBDA_1 $V0 0

4. **Stocker taille environnement**
   - STORE-HEAP 1 $V0 1

5. **Capturer variables libres**
   - Charger valeur de x
   - STORE-HEAP x_value $V0 2

6. **Générer code fonction**
   - Label LAMBDA_1
   - Prologue (frame)
   - Restaurer environnement capturé
   - Compiler corps
   - Épilogue

---

## 🔍 Analyse Variables Libres

### Algorithme

```lisp
(defun free-variables (expr bound-vars)
  "Retourne liste variables libres dans expr"
  (cond
    ;; Variable
    ((symbolp expr)
     (if (member expr bound-vars) '() (list expr)))
    
    ;; Constante
    ((numberp expr) '())
    
    ;; Lambda
    ((and (listp expr) (eq (car expr) 'lambda))
     (let ((params (second expr))
           (body (cddr expr)))
       (free-variables-list body (append params bound-vars))))
    
    ;; Let
    ((and (listp expr) (eq (car expr) 'let))
     (let ((bindings (second expr))
           (body (cddr expr)))
       (append
         ;; Variables libres dans les valeurs
         (free-variables-list (mapcar #'second bindings) bound-vars)
         ;; Variables libres dans le corps
         (free-variables-list body 
           (append (mapcar #'first bindings) bound-vars)))))
    
    ;; Application
    ((listp expr)
     (free-variables-list expr bound-vars))))

(defun free-variables-list (exprs bound-vars)
  (remove-duplicates
    (apply #'append (mapcar (lambda (e) 
                              (free-variables e bound-vars)) 
                            exprs))))
```

### Exemples

```lisp
(free-variables 'x '())           → (x)
(free-variables 'x '(x))          → ()
(free-variables '(+ x y) '(x))    → (y)
(free-variables '(lambda (x) x) '()) → ()
(free-variables '(lambda (x) y) '()) → (y)
(free-variables '(lambda (x) (+ x y)) '()) → (y)
```

---

## 🎨 Application de Closure

### Appel de Fonction Closure

**Code source:**
```lisp
(f 3)  ; où f est une closure
```

**Étapes:**

1. **Charger closure**
   - f est dans environnement → charger adresse

2. **Extraire code label**
   - LOAD-HEAP closure_addr 0 → code_label

3. **Extraire environnement**
   - LOAD-HEAP closure_addr 1 → env_size
   - Pour i de 0 à env_size-1:
     - LOAD-HEAP closure_addr (2+i) → restaurer variable

4. **Passer paramètres**
   - Comme appel normal ($A0, $A1, ...)

5. **Appeler**
   - JAL code_label

---

## 🧪 Cas de Test

### Test 1: Closure Basique
```lisp
(let ((x 5))
  ((lambda (y) (+ x y)) 3))
→ 8
```

### Test 2: Closure Retournée
```lisp
(let ((x 5))
  (let ((f (lambda (y) (+ x y))))
    (f 3)))
→ 8
```

### Test 3: Closure Imbriquée
```lisp
((lambda (x) 
   (lambda (y) 
     (+ x y))) 
 5)
→ <closure>

;; Application
(((lambda (x) (lambda (y) (+ x y))) 5) 3)
→ 8
```

### Test 4: Closures Multiples
```lisp
(let ((x 5))
  (let ((f (lambda (y) (+ x y)))
        (g (lambda (y) (* x y))))
    (+ (f 3) (g 2))))
→ 18  ; (5+3) + (5*2) = 8 + 10
```

### Test 5: Closure avec État Modifiable
```lisp
(let ((counter 0))
  (let ((inc (lambda () (setq counter (+ counter 1)))))
    (inc)
    (inc)
    counter))
→ 2
```

### Test 6: Higher-Order Functions
```lisp
(let ((make-adder (lambda (x) 
                    (lambda (y) (+ x y)))))
  (let ((add5 (make-adder 5)))
    (add5 3)))
→ 8
```

---

## 🔨 Plan d'Implémentation

### Partie 1: VM (5-6h)

**Fichiers:**
- `src/asm-ops.lisp`: Définir MALLOC, LOAD-HEAP, STORE-HEAP
- `src/vm.lisp`: Implémenter exécution

**Tests:**
- `tests/unit/test-heap.lisp`: Tests VM tas

### Partie 2: Analyse (4-5h)

**Fichiers:**
- `src/compiler.lisp`: free-variables, free-variables-list

**Tests:**
- Tests unitaires free-variables

### Partie 3: Compilation (6-8h)

**Fichiers:**
- `src/compiler.lisp`: compile-lambda, compile-application-closure

**Ajouts:**
- Environnement: Tracker closures
- Compilation: Générer code capture/restauration

### Partie 4: Tests (2-3h)

**Fichiers:**
- `tests/unit/test-closures.lisp`: 10+ tests

**Validation:**
- Tests basiques passent
- Tests avancés passent
- Aucune régression (70 tests existants)

---

## 📊 Métriques Succès

- [ ] Instructions tas fonctionnent
- [ ] free-variables correct
- [ ] Closures basiques (Test 1-2) passent
- [ ] Closures imbriquées (Test 3) passent
- [ ] Higher-order (Test 6) passe
- [ ] 10+ nouveaux tests passent
- [ ] 70 tests existants passent toujours
- [ ] Total: 80+ tests (100%)

---

## 🚧 Difficultés Anticipées

1. **Gestion mémoire:** Pas de garbage collection
2. **Lifetime closures:** Référence après libération (accepté)
3. **Modification variables:** SETQ dans closure (complexe)
4. **Performance:** Allocation dynamique lente
5. **Débogage:** Difficile de tracer tas

---

## 🎯 Objectif Minimum

**MVP (Minimum Viable Product):**
- Closures basiques fonctionnent
- Capture 1-2 variables
- Pas de SETQ dans closure
- Pas de garbage collection

**Si temps permet:**
- Closures imbriquées multiples
- SETQ dans closure (références)
- Optimisations

---

**Conception terminée!** Passons à l'implémentation. 🚀
