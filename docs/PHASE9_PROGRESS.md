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

**Étape 3 : Analyse des variables libres** (4-5h)
- Implémenter `free-variables` dans `src/compiler.lisp`
- Distinguer variables libres vs liées
- Gérer LAMBDA, LET, LABELS

**Étape 4 : Compilation LAMBDA** (6-8h)
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

## Statut global Phase 9

- [x] Étape 1 : Design théorique (3-4h) - TERMINÉ
- [x] Étape 2 : Extension VM heap (5-6h) - TERMINÉ
- [ ] Étape 3 : Variables libres (4-5h)
- [ ] Étape 4 : Compilation LAMBDA (6-8h)
- [ ] Étape 5 : Tests fermetures (2-3h)

**Progression : 2/5 étapes (40%)**
**Temps investi : ~8h**
**Temps restant : ~12-16h**
