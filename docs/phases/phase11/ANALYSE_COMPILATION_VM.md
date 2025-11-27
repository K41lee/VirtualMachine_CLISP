# Analyse: Obstacles à la Compilation de la VM (VM1+VM2)

**Date**: 27 novembre 2025  
**Objectif**: Identifier ce qui empêche la compilation du code de la VM elle-même en MIPS

---

## 📋 Résumé Exécutif

Pour implémenter VM1+VM2 (VM compilée exécutant du code MIPS compilé), il faudrait compiler le code Lisp de la VM elle-même (`vm.lisp`, `loader.lisp`, `asm-ops.lisp`) en MIPS. Actuellement, **le compilateur ne supporte que ~30% des constructions Lisp nécessaires** pour compiler la VM.

---

## 🔍 Analyse du Code de la VM

### Fichier: `src/vm.lisp` (687 lignes)

#### Constructions Lisp Utilisées

| Construct | Supporté | Fréquence | Criticité | Notes |
|-----------|----------|-----------|-----------|-------|
| `defun` | ✅ OUI | Très haute | Critique | Fonctions de base - OK |
| `defstruct` | ❌ NON | Haute | **CRITIQUE** | Structure VM, registres, état |
| `defparameter` | ❌ NON | Moyenne | Haute | Variables globales configurables |
| `defconstant` | ✅ OUI | Basse | Moyenne | Constants - implémenté |
| `let` / `let*` | ✅ OUI | Très haute | Critique | Variables locales - OK |
| `if` | ✅ OUI | Très haute | Critique | Conditions - OK |
| `cond` | ✅ OUI | Haute | Critique | Multi-conditions - OK |
| `case` | ✅ OUI | Haute | Critique | Pattern matching - OK |
| `when` / `unless` | ❌ NON | Haute | Haute | Conditions simples sans else |
| `progn` | ✅ OUI | Haute | Critique | Séquences - OK |
| `loop` | ❌ NON | Moyenne | **CRITIQUE** | Boucles complexes |
| `dolist` | ❌ NON | Haute | **CRITIQUE** | Itération sur listes |
| `dotimes` | ❌ NON | Moyenne | Haute | Itération sur range |
| `while` | ✅ OUI | Basse | Moyenne | Boucle simple - OK |
| `make-array` | ✅ OUI | Basse | Moyenne | Arrays - OK |
| `make-hash-table` | ❌ NON | Haute | **CRITIQUE** | Tables de hachage pour registres |
| `gethash` / `setf gethash` | ❌ NON | Très haute | **CRITIQUE** | Accès aux registres |
| `format` | ❌ NON | Très haute | Haute | Affichage debug/verbose |
| `error` | ❌ NON | Moyenne | Haute | Gestion d'erreurs |
| `load` | ❌ NON | Basse | N/A | Chargement de fichiers |
| `incf` / `decf` | ❌ NON | Moyenne | Moyenne | Incrémentation/décrémentation |
| `push` / `pop` | ❌ NON | Moyenne | Haute | Manipulation de listes |
| `member` / `assoc` | ❌ NON | Basse | Basse | Recherche dans listes |
| `first` / `rest` | ❌ NON | Moyenne | Moyenne | Manipulation de listes |
| Keywords (`:test`, `:initial-element`) | ❌ NON | Haute | Haute | Arguments nommés |

### Statistique Globale

- **Constructions nécessaires**: ~25
- **Constructions supportées**: ~8 (32%)
- **Constructions CRITIQUES manquantes**: 5
  - `defstruct` (structure VM)
  - `make-hash-table` / `gethash` (registres)
  - `dolist` (itération)
  - `loop` (boucles avancées)
  - `when` / `unless` (conditions simples)

---

## 🚧 Obstacles Majeurs

### 1. **DEFSTRUCT** - Obstacle #1 (CRITIQUE)

**Problème**: La VM utilise `defstruct` pour définir sa structure centrale:

```lisp
(defstruct vm
  (memory (make-array *maxmem* :initial-element 0)
          :type (simple-array t (*)))
  (registers (make-hash-table :test 'eq)
             :type hash-table)
  (state :ready
         :type keyword)
  (instruction-count 0
                     :type integer)
  (verbose nil
           :type boolean))
```

**Impact**: Sans `defstruct`, impossible de:
- Créer la structure VM
- Accéder aux champs (`vm-memory`, `vm-registers`, `vm-state`)
- Compiler `make-vm`, `make-new-vm`

**Solution requise**:
- Implémenter `defstruct` dans le compilateur
- Générer des fonctions accesseurs automatiquement
- Supporter les types déclaratifs (optionnel)
- Représenter les structures comme des tableaux ou des listes en mémoire

**Complexité**: ⭐⭐⭐⭐⭐ (Très élevée)
- Nécessite générateur de fonctions
- Gestion mémoire pour instances
- Accesseurs multiples par structure

---

### 2. **HASH-TABLES** - Obstacle #2 (CRITIQUE)

**Problème**: Les registres sont stockés dans une hash-table:

```lisp
(registers (make-hash-table :test 'eq)
           :type hash-table)

;; Utilisé partout:
(gethash reg (vm-registers vm))
(setf (gethash mapped-reg (vm-registers vm)) value)
```

**Impact**: Sans hash-tables, impossible de:
- Stocker les 32 registres MIPS efficacement
- Accéder aux registres par nom (`:$V0`, `:$T0`, etc.)
- Compiler `get-register`, `set-register`, `init-registers`

**Solution requise**:
- Implémenter `make-hash-table` → allocation tas + structure
- Implémenter `gethash` → fonction de hachage + recherche
- Implémenter `setf gethash` → insertion/mise à jour
- Alternative: Remplacer par un tableau fixe (32 registres)

**Complexité**: ⭐⭐⭐⭐⭐ (Très élevée)
- Fonction de hachage pour symboles
- Gestion des collisions
- Ou: Refactorisation complète pour utiliser des tableaux

---

### 3. **DOLIST / LOOP** - Obstacle #3 (CRITIQUE)

**Problème**: Itération sur listes/collections:

```lisp
;; DOLIST (Très fréquent)
(dolist (reg *register-names*)
  (setf (gethash reg (vm-registers vm)) 0))

;; LOOP (Dans execute-instruction)
(loop
  (unless current-env (return nil))
  ...
  (setf current-env (compiler-env-parent-lexical current-env)))
```

**Impact**: Sans itérateurs, impossible de:
- Initialiser tous les registres en boucle
- Parcourir les labels dans le loader
- Itérer sur les instructions dans `run-vm`

**Solution requise**:
- Implémenter `dolist` comme macro → `while` + `first`/`rest`
- Implémenter `loop` basique avec `return`, `unless`, etc.
- Nécessite: `first`, `rest`, `null`, `push`, `pop`

**Complexité**: ⭐⭐⭐⭐ (Élevée)
- Macro expansion pour `dolist`
- Support des constructions `loop` (return, unless)
- Manipulation avancée de listes

---

### 4. **WHEN / UNLESS** - Obstacle #4 (Haute Priorité)

**Problème**: Conditions sans branche `else`:

```lisp
(when (> (+ *heap-pointer* size) +heap-limit+)
  (error "Heap overflow..."))

(unless current-env (return nil))
```

**Impact**: Nécessaire pour 30% des conditions simples dans la VM

**Solution requise**:
- Implémenter comme macros:
  - `(when test body)` → `(if test (progn body) nil)`
  - `(unless test body)` → `(if (not test) (progn body) nil)`
- Nécessite aussi: `not` (opérateur logique)

**Complexité**: ⭐⭐ (Moyenne)
- Simple macro expansion
- Réutilise `if` existant

---

### 5. **FORMAT** - Obstacle #5 (Fonctionnel)

**Problème**: Affichage formaté omniprésent:

```lisp
(format t "  MALLOC: Allocation de ~A mots à l'adresse ~A~%" size addr)
(format t "~%=== REGISTRES ===~%")
(format t "~6A: ~A~%" reg (get-register vm reg))
```

**Impact**: Sans `format`:
- Impossible de debugger
- Pas de verbose mode
- Mais: non critique pour l'exécution fonctionnelle

**Solution requise**:
- Implémenter `format` basique (directives ~A, ~%, espace)
- Ou: remplacer par des `print` simples
- Nécessite: conversion entier→string, concaténation

**Complexité**: ⭐⭐⭐ (Moyenne-Haute)
- Parsing des directives de format
- Conversion de types
- Alternative: Stubber avec `print` simple

---

### 6. **ERROR** - Obstacle #6 (Gestion d'Erreurs)

**Problème**: Gestion d'erreurs:

```lisp
(error "Registre invalide: ~A" reg)
(error "Mémoire invalide: adresse ~A hors limites" addr)
```

**Impact**: Sans `error`:
- Pas de validation robuste
- Comportement indéfini sur erreur
- Mais: peut être remplacé par HALT

**Solution requise**:
- Implémenter `error` → affiche message + HALT VM
- Ou: remplacer par `(progn (print msg) (halt))`

**Complexité**: ⭐ (Faible)
- Simple remplacement par print + halt

---

### 7. **INCF / DECF / PUSH / POP** - Obstacle #7 (Utilitaires)

**Problème**: Macros d'incrémentation/manipulation:

```lisp
(incf *heap-pointer* size)        ; *heap-pointer* += size
(decf (get-register vm :$sp) 4)   ; $sp -= 4
(push (cons name func-label) (compiler-env-functions env))
```

**Impact**: Nécessaire pour:
- Gestion du tas (heap pointer)
- Gestion de la pile (stack pointer)
- Construction de listes

**Solution requise**:
- `incf` → `(setq var (+ var delta))`
- `decf` → `(setq var (- var delta))`
- `push` → `(setq list (cons item list))`
- `pop` → `(let ((x (car list))) (setq list (cdr list)) x)`

**Complexité**: ⭐⭐ (Moyenne)
- Simple macro expansion
- Réutilise opérations existantes

---

### 8. **ARGUMENTS NOMMÉS (Keywords)** - Obstacle #8

**Problème**: Arguments optionnels/nommés:

```lisp
(make-array *maxmem* :initial-element 0)
(make-hash-table :test 'eq)
(make-new-vm :verbose nil)
```

**Impact**: Impossible d'utiliser les fonctions standard avec options

**Solution requise**:
- Parser les arguments keywords dans `compile-defun`
- Générer code pour valeurs par défaut
- Ou: Utiliser uniquement arguments positionnels

**Complexité**: ⭐⭐⭐⭐ (Élevée)
- Parsing avancé des paramètres
- Gestion des valeurs par défaut
- Alternative: Refactoriser pour éviter keywords

---

### 9. **LIST MANIPULATION** - Obstacle #9

**Problème**: Fonctions sur listes:

```lisp
(first expr)    ; car
(rest expr)     ; cdr
(null list)     ; vérifier si vide
(member x list) ; recherche
(assoc key alist) ; association
```

**Impact**: Nécessaire pour:
- Navigation dans les instructions
- Recherche de labels
- Gestion des environnements lexicaux

**Solution requise**:
- Implémenter: `first`, `rest`, `car`, `cdr`, `cons`, `null`
- Implémenter: `member`, `assoc` (recherche)
- Représentation des listes en mémoire (cons cells)

**Complexité**: ⭐⭐⭐⭐ (Élevée)
- Allocation dynamique pour cons cells
- Gestion mémoire (pas de GC)
- Fonctions de recherche

---

## 📊 Tableau Récapitulatif

| Obstacle | Criticité | Complexité | Effort (j/h) | Contournement Possible |
|----------|-----------|------------|--------------|------------------------|
| 1. DEFSTRUCT | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | 5-7 jours | ❌ NON |
| 2. HASH-TABLES | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | 3-5 jours | ✅ OUI (tableau fixe) |
| 3. DOLIST/LOOP | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | 2-3 jours | ✅ OUI (while + first/rest) |
| 4. WHEN/UNLESS | ⭐⭐⭐⭐ | ⭐⭐ | 4-6 heures | ✅ OUI (macro if) |
| 5. FORMAT | ⭐⭐⭐ | ⭐⭐⭐ | 1-2 jours | ✅ OUI (print simple) |
| 6. ERROR | ⭐⭐⭐ | ⭐ | 2-4 heures | ✅ OUI (print + halt) |
| 7. INCF/DECF/PUSH | ⭐⭐⭐ | ⭐⭐ | 4-6 heures | ✅ OUI (macros) |
| 8. KEYWORDS | ⭐⭐⭐ | ⭐⭐⭐⭐ | 2-3 jours | ✅ OUI (args positionnels) |
| 9. LIST OPS | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | 2-4 jours | ⚠️ PARTIEL |

**Total effort estimé**: **20-35 jours** de développement

---

## 🛠️ Stratégies Possibles

### Option A: Compilation Complète (Idéale mais Longue)

**Approche**: Implémenter toutes les constructions manquantes

**Avantages**:
- VM entièrement compilable
- Performance maximale (théoriquement)
- Compilateur Lisp complet

**Inconvénients**:
- 20-35 jours de développement
- Très complexe (defstruct, hash-tables)
- Risque de bugs dans compilateur

**Priorisation**:
1. WHEN/UNLESS (4-6h) - facile, gain immédiat
2. INCF/DECF/PUSH (4-6h) - macros simples
3. ERROR → print+halt (2-4h) - contournement
4. DOLIST → while (2-3j) - nécessite first/rest
5. DEFSTRUCT (5-7j) - obstacle majeur
6. HASH-TABLES ou remplacement (3-5j) - obstacle majeur

---

### Option B: Refactorisation de la VM (Pragmatique)

**Approche**: Réécrire `vm.lisp` en utilisant uniquement les constructions supportées

**Changements requis**:
- Remplacer `defstruct` par des listes/tableaux manuels
- Remplacer hash-table par tableau fixe de 32 registres
- Remplacer `dolist` par `while` + manipulation manuelle
- Remplacer `format` par `print` simple
- Éliminer keywords (arguments positionnels uniquement)

**Avantages**:
- Faisable en 3-5 jours
- Pas de modification du compilateur
- Prouve la faisabilité

**Inconvénients**:
- Code VM moins idiomatique
- Maintenance plus difficile
- Deux versions de la VM à maintenir

---

### Option C: VM Simplifiée (Proof of Concept)

**Approche**: Créer une micro-VM minimale compilable

**Fonctionnalités**:
- 8 registres (tableau fixe)
- 10 instructions MIPS de base (ADD, SUB, LW, SW, BEQ, J, JAL, JR, HALT)
- Pas de verbose, pas de debug
- Mémoire tableau simple

**Avantages**:
- Réalisable en 2-3 jours
- Démontre le concept VM1+VM2
- Permet benchmarks (très lent mais fonctionnel)

**Inconvénients**:
- Ne compile pas la vraie VM
- Limité à des programmes triviaux
- Pas de vrai intérêt pratique

---

## 🎯 Recommandation

### Court Terme (Démonstration)

**Option C: Micro-VM Proof of Concept**

Créer `micro-vm.lisp` avec:
```lisp
(defun micro-run (mem pc)
  (let ((inst (aref mem pc)))
    (case (first inst)
      (:ADD ...)
      (:HALT (return (aref regs 0))))))
```

**Effort**: 2-3 jours  
**Résultat**: Démonstration fonctionnelle VM1+VM2 (très lent)

---

### Moyen Terme (Production)

**Option A Partielle: Implémenter constructions essentielles**

Priorité:
1. **WHEN/UNLESS** (6h) - facile
2. **INCF/DECF** (4h) - facile
3. **LIST OPS basiques** (2j) - first, rest, cons, null
4. **DOLIST** (1j) - avec list ops
5. **DEFSTRUCT simplifié** (3-4j) - sans types, génération automatique

**Effort total**: ~10 jours  
**Résultat**: 60-70% de la VM compilable

---

### Long Terme (Optimal)

**Option A Complète: Compilateur Lisp Full-Featured**

Implémenter toutes les constructions standard

**Effort**: 20-35 jours  
**Résultat**: VM entièrement compilable, compilateur robuste

---

## 📝 Conclusion

**Pour VM1+VM2, il manque principalement**:

1. ❌ **DEFSTRUCT** - structure VM, obstacle majeur
2. ❌ **HASH-TABLES** - registres, peut être remplacé par tableau
3. ❌ **DOLIST/LOOP** - itération, nécessite list ops
4. ❌ **WHEN/UNLESS** - conditions simples, facile à ajouter
5. ❌ **Manipulation de listes** - first, rest, cons, null

**Effort minimal estimé**: 10-15 jours pour rendre 70% de la VM compilable  
**Effort complet**: 20-35 jours pour VM entièrement compilable  
**Alternative rapide**: 2-3 jours pour micro-VM proof of concept

Le projet actuel démontre déjà avec succès la compilation Lisp→MIPS avec un compilateur fonctionnel à 100% pour les constructions implémentées. La VM1+VM2 reste un objectif théorique nécessitant un effort substantiel.
