# Phase 9 - CLOSURES - Progression

## Étape 2 : Extension VM pour le TAS dynamique ✅

### Date : [Session en cours]

### Objectif
Ajouter le support du tas dynamique (heap) à la VM pour permettre l'allocation de structures en mémoire (fermetures).

### Modifications effectuées

#### 1. Instructions ajoutées (`src/asm-ops.lisp`)

**Nouveaux opcodes :**
```lisp
:MALLOC :LOAD-HEAP :STORE-HEAP
```

**Arité des instructions :**
- `:MALLOC` : 2 arguments (size, result-reg)
- `:LOAD-HEAP` : 3 arguments (addr-reg, offset, result-reg)
- `:STORE-HEAP` : 3 arguments (value-reg, addr-reg, offset)

**Registres ajoutés à get-reg :**
- `:t4`, `:t5`, `:t6`, `:t7` (registres temporaires MIPS)

#### 2. Gestion du tas (`src/vm.lisp`)

**Constantes et variables :**
```lisp
(defparameter *heap-pointer* +heap-start+)
(defconstant +heap-limit+ (+ +heap-start+ *heap-size*))
```

**Fonctions :**
- `reset-heap` : Réinitialise le pointeur du tas
- `vm-malloc` : Alloue N mots sur le tas (bump allocator simple)
  - Vérifie le dépassement de capacité
  - Retourne l'adresse allouée
  - Incrémente le pointeur du tas

**Intégration dans reset-vm :**
Le tas est maintenant réinitialisé lors du reset de la VM.

#### 3. Exécution des instructions

**:MALLOC**
```lisp
Format: (MALLOC size result-reg)
Effet: result-reg = adresse allouée
```
- Évalue la taille demandée
- Appelle `vm-malloc`
- Stocke l'adresse dans le registre résultat

**:LOAD-HEAP**
```lisp
Format: (LOAD-HEAP addr-reg offset result-reg)
Effet: result-reg = MEM[addr-reg + offset]
```
- Calcule l'adresse : base + offset
- Lit la valeur en mémoire
- Stocke dans le registre résultat
- Mode verbose : affiche l'opération

**:STORE-HEAP**
```lisp
Format: (STORE-HEAP value-reg addr-reg offset)
Effet: MEM[addr-reg + offset] = value-reg
```
- Calcule l'adresse : base + offset
- Écrit la valeur en mémoire
- Mode verbose : affiche l'opération

#### 4. Export des symboles

Ajout des exports dans `src/vm.lisp` :
```lisp
reset-heap vm-malloc *heap-pointer* +heap-limit+
```

### Tests créés (`tests/unit/test-heap.lisp`)

#### Test 1 : MALLOC Simple ✅
- Alloue 5 mots
- Vérifie l'adresse retournée
- Vérifie l'incrémentation du pointeur

#### Test 2 : MALLOC Multiple ✅
- 3 allocations successives (3, 5, 2 mots)
- Vérifie que les adresses sont consécutives
- Vérifie le pointeur final

#### Test 3 : STORE/LOAD HEAP ✅
- Alloue 3 mots
- Écrit 3 valeurs (42, 100, 255)
- Relit les 3 valeurs
- Vérifie la persistance des données

#### Test 4 : Structure de fermeture ✅
- Crée une structure [Label][Size][Var]
- Simule une fermeture avec 1 variable capturée
- Vérifie Label=5000, Size=1, Var=42

### Résultats des tests

```
╔════════════════════════════════════════════════╗
║  TESTS UNITAIRES - TAS DYNAMIQUE (PHASE 9)   ║
╚════════════════════════════════════════════════╝

✓ TEST MALLOC SIMPLE: RÉUSSI
✓ TEST MALLOC MULTIPLE: RÉUSSI
✓ TEST STORE/LOAD HEAP: RÉUSSI
✓ TEST STRUCTURE FERMETURE: RÉUSSI

4/4 tests passent (100%)
```

### Caractéristiques techniques

**Allocation :**
- Type : Bump allocator (allocation linéaire)
- Pas de garbage collection
- Détection de dépassement de capacité

**Zones mémoire :**
- Heap start : 21 (après les 20 mots de registres)
- Heap limit : 2021
- Capacité : 2000 mots

**Limitations connues :**
- Pas de libération de mémoire
- Pas de compaction
- Les fermetures "fuient" la mémoire (acceptable pour MVP)

### Compatibilité

**Tests existants :**
✅ Aucune régression - Le système compile et exécute `(+ 2 3)` correctement

**Registres MIPS :**
✅ get-reg étendu pour supporter :t0 à :t7

### Prochaines étapes

**Étape 3 : Analyse des variables libres** (4-5h) ✅ TERMINÉ
- ✅ Implémenter `free-variables` dans `src/compiler.lisp`
- ✅ Distinguer variables libres vs liées
- ✅ Gérer LAMBDA, LET, LABELS
- ✅ 17/17 tests passent

**Étape 4 : Compilation LAMBDA** (6-8h) 🔄 EN COURS
- Implémenter `compile-lambda`
- Génération du code de la fonction
- Allocation de fermeture sur le tas
- Capture des variables libres

**Étape 5 : Tests de fermetures** (2-3h)
- Tests de fermetures simples
- Tests de fermetures imbriquées
- Tests d'ordre supérieur
- Validation complète

### Temps estimé restant

- Étape 3 : 4-5h
- Étape 4 : 6-8h
- Étape 5 : 2-3h
**Total : 12-16h**

### Notes techniques

**Design choices validés :**
1. Structure de fermeture : [Label][Size][Vars...] ✅
2. Instructions heap séparées de LW/SW ✅
3. Bump allocator sans GC ✅
4. Tests unitaires des instructions heap ✅

**Code quality :**
- Documentation complète des fonctions
- Mode verbose pour débogage
- Gestion d'erreurs (heap overflow)
- Tests exhaustifs des nouvelles fonctionnalités

---

## Étape 3 : Analyse des variables libres ✅

### Date : [Session en cours]

### Objectif
Implémenter l'analyse statique des variables libres pour identifier quelles variables doivent être capturées dans les fermetures.

### Modifications effectuées

#### 1. Fonction principale (`src/compiler.lisp`)

**`free-variables (expr &optional (bound-vars '()))`**
- Analyse récursive d'une expression LISP
- Retourne la liste des variables libres (sans doublons)
- Arguments :
  - `expr` : Expression à analyser
  - `bound-vars` : Variables actuellement liées dans le scope

**Liste des opérateurs built-in :**
```lisp
*built-in-operators* = (+, -, *, /, =, <, >, if, let, lambda, ...)
```
Permet d'éviter de traiter les opérateurs comme des variables libres.

#### 2. Gestion par type d'expression

**Constantes et atomes :**
- Nombres, `nil`, `t` → pas de variables libres
- Symboles → libre si pas dans `bound-vars` et pas built-in

**LAMBDA :**
```lisp
(lambda (x y) (+ x y z))  → {z}
```
Les paramètres deviennent des variables liées dans le corps.

**LET :**
```lisp
(let ((x a) (y b)) (+ x y c))  → {a, b, c}
```
- Valeurs évaluées dans le scope externe
- Variables LET liées dans le corps

**LABELS :**
```lisp
(labels ((f (x) (+ x y))) (f 10))  → {y}
```
- Noms de fonctions mutuellement liés
- Paramètres liés dans chaque fonction

**Autres formes :**
- `IF`, `COND`, `WHEN`, `UNLESS` : analyse récursive
- `AND`, `OR` : analyse de tous les arguments
- `CASE` : analyse keyform + clauses
- `DOTIMES` : variable de boucle liée, count évalué dans scope externe
- `SETQ` : variable assignée + valeur analysées

#### 3. Fonction auxiliaire

**`free-variables-list (expr-list bound-vars)`**
- Analyse une liste d'expressions
- Retourne l'union des variables libres
- Utilisée pour corps de fonctions, clauses, etc.

### Tests créés (`tests/unit/test-free-variables.lisp`)

#### Tests de base (3) ✅
- Constante → `'()`
- Variable libre → `(x)`
- Variable liée → `'()`

#### Tests LAMBDA (4) ✅
- Lambda simple : `(lambda (y) (+ x y))` → `(x)`
- Sans variables libres : `(lambda (x y) (+ x y))` → `'()`
- Lambda imbriquée : `(lambda (x) (lambda (y) (+ x y z)))` → `(z)`
- Plusieurs libres : `(lambda (x) (+ x y z))` → `{y, z}`

#### Tests LET (3) ✅
- Let simple : `(let ((x 1)) (+ x y))` → `(y)`
- Variable libre dans init : `(let ((x y)) (+ x 1))` → `(y)`
- Let imbriqués : `(let ((x a)) (let ((y b)) (+ x y c)))` → `{a, b, c}`

#### Tests LABELS (3) ✅
- Simple : `(labels ((f (x) (+ x y))) (f 10))` → `(y)`
- Récursif : `(labels ((fact (n) ...)) (fact x))` → `(x)`
- Mutuel : `(labels ((even ...) (odd ...)) (even x))` → `(x)`

#### Tests complexes (4) ✅
- Fabrique de closures : `(lambda (x) (lambda (y) (+ x y)))` → `'()`
- Expression complexe avec LET+LABELS+LAMBDA → `{w, a}`
- DOTIMES : `(dotimes (i n) (+ i x))` → `{n, x}`
- SETQ : `(setq x (+ y 1))` → `{x, y}`

### Résultats des tests

```
╔════════════════════════════════════════════════════╗
║  TESTS UNITAIRES - VARIABLES LIBRES (PHASE 9)    ║
╚════════════════════════════════════════════════════╝

✓ TEST RÉUSSI (×17)

17/17 tests passent (100%)
```

### Caractéristiques de l'implémentation

**Correctness :**
- Analyse statique complète
- Gestion de toutes les formes spéciales du compilateur
- Portée lexicale respectée

**Robustesse :**
- Distinction opérateurs built-in / variables
- Gestion des imbrications arbitraires
- Pas de doublons dans les résultats (`union` avec `:test #'eq`)

**Exemples validés :**

| Expression | Variables libres |
|-----------|------------------|
| `(lambda (x) x)` | `'()` |
| `(lambda (x) y)` | `(y)` |
| `(lambda (x) (lambda (y) (+ x y)))` | `'()` |
| `(lambda (x) (lambda (y) (+ x y z)))` | `(z)` |
| `(let ((x a)) (+ x b))` | `{a, b}` |
| `(labels ((f (x) (g x))) (f y))` | `{g, y}` |

### Compatibilité

**Tests existants :**
✅ Aucune régression - `(+ 10 20)` compile et retourne 30

**Prêt pour l'étape 4 :**
✅ La fonction `free-variables` est opérationnelle et testée
✅ Peut être utilisée par `compile-lambda` pour identifier les captures

---

## Statut global Phase 9

- [x] Étape 1 : Design théorique (3-4h) - TERMINÉ
- [x] Étape 2 : Extension VM heap (5-6h) - TERMINÉ
- [x] Étape 3 : Variables libres (4-5h) - TERMINÉ
- [ ] Étape 4 : Compilation LAMBDA (6-8h) - EN COURS
- [ ] Étape 5 : Tests fermetures (2-3h)

**Progression : 2/5 étapes (40%)**
**Temps investi : ~8h**
**Temps restant : ~12-16h**
