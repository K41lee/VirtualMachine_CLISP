# 🚀 PHASE 10 : BOOTSTRAP - PLAN D'ACTION DÉTAILLÉ

**Date de début :** 27 novembre 2025  
**Objectif :** Auto-compilation - La VM peut compiler et exécuter elle-même  
**Durée estimée :** 15-20 heures  
**Complexité :** ★★★★★

---

## 📋 CONCEPT DU BOOTSTRAP

### Qu'est-ce que le bootstrap ?

Le **bootstrap** (amorçage) consiste à faire compiler le compilateur par lui-même. Dans notre projet :

```
VM₀ (LISP natif - hôte)
  ├─ Compile: compiler.lisp → ASM
  ├─ Charge: VM₁ (VM compilée en MIPS)
  └─ Exécute dans VM₁:
       ├─ Code utilisateur (fibonacci, etc.)
       └─ Potentiellement: compiler₁ (compilateur dans VM)
```

### Hiérarchie des niveaux

1. **VM₀** : Machine virtuelle native (Common Lisp)
2. **VM₁** : Machine virtuelle compilée (MIPS dans VM₀)
3. **Code** : Programme utilisateur exécuté dans VM₁

### Objectifs de la Phase 10

✅ **Objectif 1 :** Compiler le loader en ASM  
✅ **Objectif 2 :** Compiler la VM en ASM  
✅ **Objectif 3 :** Charger VM₁ dans VM₀  
✅ **Objectif 4 :** Exécuter fibonacci(10) dans VM₁  
✅ **Objectif 5 :** Compiler le compilateur (auto-compilation complète)  
✅ **Objectif 6 :** Mesurer et comparer les performances  

---

## 🎯 PRÉREQUIS ET ÉTAT ACTUEL

### ✅ Acquis (Phase 9)

- ✅ Compilateur LISP→MIPS fonctionnel
- ✅ VM MIPS complète (35+ instructions)
- ✅ Loader assembleur opérationnel
- ✅ Closures et captures de variables
- ✅ Tas dynamique (heap)
- ✅ 84/84 tests passants (100%)

### ⚠️ Défis identifiés

1. **Dépendances externes** : Le compilateur utilise des fonctions LISP natives
   - `format`, `apply`, `funcall`, `mapcar`, etc.
   - Solution : Réécrire en LISP pur ou compiler les primitives

2. **Taille du code** : Le compilateur fait ~1900 lignes
   - Code MIPS généré sera très volumineux (5000-10000 instructions)
   - Risque de dépassement mémoire VM

3. **Structures de données** : Hash-tables, listes complexes
   - Besoin de représentation MIPS efficace
   - Gestion mémoire cruciale

4. **Métacircularité** : Le compilateur doit se compiler lui-même
   - Nécessite un point fixe (compiler₀ = compiler₁)
   - Vérification de cohérence essentielle

---

## 📊 STRATÉGIE DE DÉVELOPPEMENT

### Approche progressive (Bottom-Up)

```
Étape 1: Loader simple
   ↓
Étape 2: VM basique
   ↓
Étape 3: VM complète
   ↓
Étape 4: Test fibonacci dans VM₁
   ↓
Étape 5: Compiler (subset)
   ↓
Étape 6: Auto-compilation
```

### Ordre d'implémentation

1. **Loader** (le plus simple, ~175 lignes)
   - Peu de dépendances
   - Fonctions pures majoritairement
   - Test facile : charger du code ASM

2. **VM** (moyennement complexe, ~700 lignes)
   - Boucle d'exécution
   - Dispatch instructions
   - Gestion registres/mémoire

3. **Compilateur** (le plus complexe, ~1900 lignes)
   - Nombreuses dépendances
   - Structures de données complexes
   - Métacircularité

---

## 🗓️ PLAN DÉTAILLÉ PAR ÉTAPES

---

## ÉTAPE 1 : PRÉPARATION DU COMPILATEUR (4-5h)

### Objectif
Rendre le compilateur "self-contained" (auto-suffisant)

### 1.1 Audit des dépendances (1h)

**Actions :**
- [ ] Lister toutes les fonctions LISP natives utilisées
- [ ] Identifier lesquelles sont critiques
- [ ] Catégoriser : primitives / utilitaires / remplaçables

**Commande :**
```bash
cd "/home/etudiant/Bureau/CLisp/TD LISP-20251009/VirtualMachine_CLISP"
grep -E "(format|apply|funcall|mapcar|remove-if|find|assoc)" src/compiler.lisp | wc -l
```

**Livrable :** `docs/AUDIT_DEPENDANCES.md`

### 1.2 Implémentation des primitives (2h)

**Actions :**
- [ ] Créer `src/primitives.lisp`
- [ ] Implémenter versions simplifiées :
  - `my-mapcar` (itération sur listes)
  - `my-remove-if` (filtrage)
  - `my-find` (recherche)
  - `my-assoc` (association lists)

**Exemple d'implémentation :**
```lisp
;;;; primitives.lisp
;;;; Primitives LISP réécrites pour auto-compilation

(defun my-mapcar (fn lst)
  "Version simplifiée de mapcar pour une seule liste"
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (my-mapcar fn (cdr lst)))))

(defun my-append (lst1 lst2)
  "Concaténation de deux listes"
  (if (null lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))

(defun my-reverse (lst)
  "Inversion de liste (tail-recursive)"
  (labels ((rev-aux (lst acc)
             (if (null lst)
                 acc
                 (rev-aux (cdr lst) (cons (car lst) acc)))))
    (rev-aux lst nil)))

(defun my-length (lst)
  "Longueur d'une liste"
  (if (null lst)
      0
      (+ 1 (my-length (cdr lst)))))

(defun my-nth (n lst)
  "N-ième élément d'une liste (0-indexé)"
  (if (= n 0)
      (car lst)
      (my-nth (- n 1) (cdr lst))))
```

**Livrable :** `src/primitives.lisp`

### 1.3 Adaptation du compilateur (1-2h)

**Actions :**
- [ ] Remplacer les appels natifs par nos primitives
- [ ] Créer `src/compiler-bootstrap.lisp` (version adaptée)
- [ ] Tester que la version adaptée fonctionne toujours

**Modifications à faire :**
```lisp
;; AVANT:
(mapcar #'compile-expr args env)

;; APRÈS:
(my-mapcar (lambda (arg) (compile-expr arg env)) args)
```

**Test de validation :**
```bash
clisp -q -x "(load \"main.lisp\") \
  (load \"src/compiler-bootstrap.lisp\") \
  (compile-and-run '(+ 2 3))"
# Attendu: 5
```

**Livrable :** `src/compiler-bootstrap.lisp`

---

## ÉTAPE 2 : COMPILATION DU LOADER (3-4h)

### Objectif
Compiler `loader.lisp` en code MIPS exécutable

### 2.1 Analyse du loader (30min)

**Actions :**
- [ ] Identifier les fonctions principales :
  - `collect-labels` (collecte labels)
  - `resolve-labels` (résolution addresses)
  - `load-asm` (chargement code)
- [ ] Repérer les dépendances :
  - Hash-tables (pour labels)
  - Mapcar, dolist

**Livrable :** Schéma des fonctions du loader

### 2.2 Compilation fonction par fonction (2h)

**Actions :**
- [ ] Compiler `collect-labels` → ASM
- [ ] Compiler `resolve-labels` → ASM
- [ ] Compiler `load-asm` → ASM
- [ ] Assembler le tout

**Commande :**
```lisp
;; Dans CLISP:
(load "main.lisp")
(load "src/compiler-bootstrap.lisp")

;; Compiler collect-labels
(defparameter *asm-collect-labels*
  (compile-to-asm '(defun collect-labels (asm-code code-start)
                     ;; ... code ...
                     )))

;; Sauvegarder
(with-open-file (out "output/loader-compiled.asm" 
                     :direction :output 
                     :if-exists :supersede)
  (dolist (instr *asm-collect-labels*)
    (format out "~A~%" instr)))
```

**Livrable :** `output/loader-compiled.asm`

### 2.3 Tests du loader compilé (1h)

**Actions :**
- [ ] Charger le loader compilé dans VM₀
- [ ] Tester avec un code ASM simple
- [ ] Vérifier que les labels sont bien résolus

**Test :**
```lisp
;; Charger loader compilé
(load-asm-file "output/loader-compiled.asm" *vm*)

;; Code ASM à charger avec le loader compilé
(defparameter *test-code*
  '((:LABEL START)
    (:ADDI $t0 $zero 42)
    (:SYSCALL)))

;; Appeler le loader compilé (dans VM)
;; Le loader doit résoudre START et charger le code
```

**Critère de succès :**
✅ Loader compilé charge correctement du code ASM  
✅ Labels résolus aux bonnes adresses  
✅ Pas de crash ou erreur  

**Livrable :** Tests dans `tests/integration/test-loader-compiled.lisp`

---

## ÉTAPE 3 : COMPILATION DE LA VM (4-5h)

### Objectif
Compiler `vm.lisp` en code MIPS pour créer VM₁

### 3.1 Analyse de la VM (1h)

**Actions :**
- [ ] Identifier la boucle principale d'exécution
- [ ] Lister toutes les instructions (35+)
- [ ] Repérer les structures critiques :
  - Registres (hash-table)
  - Mémoire (array)
  - État VM

**Questions à résoudre :**
- Comment représenter `vm-struct` en MIPS ?
- Comment gérer le dispatch d'instructions ?
- Quelle taille mémoire allouer pour VM₁ ?

**Livrable :** `docs/ARCHITECTURE_VM1.md`

### 3.2 Représentation de la VM en mémoire (1h)

**Décisions architecturales :**

```
Structure VM₁ en mémoire (dans VM₀):
┌─────────────────────────────────┐
│ Registres VM₁ (38 mots)         │  Offset 0-37
├─────────────────────────────────┤
│ Mémoire VM₁ (5000 mots)         │  Offset 38-5037
├─────────────────────────────────┤
│ État VM₁ (PC, état, compteurs)  │  Offset 5038-5050
├─────────────────────────────────┤
│ Code VM₁ (boucle exec)          │  Offset 5051+
└─────────────────────────────────┘
```

**Actions :**
- [ ] Définir les offsets des structures
- [ ] Créer des macros d'accès
- [ ] Documenter le layout mémoire

**Livrable :** Constantes et macros dans `src/vm-bootstrap.lisp`

### 3.3 Compilation de la boucle d'exécution (2h)

**Actions :**
- [ ] Compiler `vm-run` (boucle principale)
- [ ] Compiler `vm-fetch` (lecture instruction)
- [ ] Compiler `vm-decode` (décodage opcode)
- [ ] Compiler `vm-execute` (dispatch)

**Stratégie de dispatch :**
```lisp
;; Pseudo-code du dispatch
(defun vm-execute (vm instruction)
  (case (first instruction)
    (:ADD (vm-exec-add vm instruction))
    (:ADDI (vm-exec-addi vm instruction))
    (:LW (vm-exec-lw vm instruction))
    ;; ... 35+ instructions
    ))
```

**Optimisation :** Utiliser un jump-table pour le dispatch

**Livrable :** `output/vm-compiled.asm`

### 3.4 Tests de VM₁ (1h)

**Actions :**
- [ ] Charger VM₁ dans VM₀
- [ ] Exécuter une instruction simple (ADDI)
- [ ] Vérifier registres et mémoire de VM₁

**Test progressif :**
```lisp
;; Test 1: Une seule instruction
(vm1-execute '(:ADDI $t0 $zero 42))
(vm1-get-register '$t0) ; → 42

;; Test 2: Séquence d'instructions
(vm1-execute '((:ADDI $t0 $zero 10)
               (:ADDI $t1 $zero 20)
               (:ADD $v0 $t0 $t1)))
(vm1-get-register '$v0) ; → 30

;; Test 3: Boucle simple
(vm1-execute '((:ADDI $t0 $zero 0)
               (:LABEL LOOP)
               (:ADDI $t0 $t0 1)
               (:SLTI $t1 $t0 10)
               (:BNE $t1 $zero LOOP)))
(vm1-get-register '$t0) ; → 10
```

**Critère de succès :**
✅ VM₁ exécute correctement instructions basiques  
✅ Registres mis à jour correctement  
✅ Branches et labels fonctionnent  

**Livrable :** `tests/integration/test-vm1-basic.lisp`

---

## ÉTAPE 4 : TEST FIBONACCI DANS VM₁ (2-3h)

### Objectif
Exécuter fibonacci(10) dans VM₁ et comparer avec VM₀

### 4.1 Compilation de fibonacci (30min)

**Actions :**
- [ ] Compiler fibonacci en MIPS (déjà fait en Phase 1-7)
- [ ] Préparer le code pour chargement dans VM₁

**Code fibonacci :**
```lisp
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

**Livrable :** `tests/integration/fib10-for-vm1.asm`

### 4.2 Chargement dans VM₁ (1h)

**Actions :**
- [ ] Utiliser le loader compilé (de l'étape 2)
- [ ] Charger fibonacci dans la mémoire de VM₁
- [ ] Initialiser PC, registres

**Commande :**
```lisp
;; Dans VM₀, charger VM₁
(load-vm1 *vm0*)

;; Dans VM₁, charger fibonacci
(vm1-load-program "tests/integration/fib10-for-vm1.asm")

;; Configurer l'appel: fib(10)
(vm1-set-register '$a0 10)
(vm1-set-pc 'FIB)  ; Label de la fonction
```

**Livrable :** Script de chargement

### 4.3 Exécution et mesure (1h)

**Actions :**
- [ ] Exécuter fibonacci(10) dans VM₁
- [ ] Récupérer le résultat (doit être 55)
- [ ] Mesurer le temps d'exécution
- [ ] Compter les instructions exécutées

**Mesures attendues :**
```
fibonacci(10) = 55

VM₀ (LISP natif):
  - Temps: ~0.001s
  - Instructions: ~177 (MIPS)

VM₁ (compilée dans VM₀):
  - Temps: ~0.5-1.0s
  - Instructions: ~177 (même code MIPS)
  - Instructions VM₀ pour simuler VM₁: ~50,000-100,000
  - Ratio: 500-1000x plus lent (normal)
```

**Analyse :**
- Chaque instruction MIPS dans VM₁ nécessite ~500 instructions LISP dans VM₀
- C'est l'overhead de la simulation

**Critère de succès :**
✅ fibonacci(10) retourne 55  
✅ Aucune erreur d'exécution  
✅ Mesures de performance documentées  

**Livrable :** `docs/BENCHMARK_VM1.md`

### 4.4 Tests supplémentaires (30min)

**Actions :**
- [ ] Tester fibonacci(5), fibonacci(15)
- [ ] Tester d'autres programmes : factoriel, somme
- [ ] Vérifier cohérence des résultats

**Livrable :** Suite de tests `tests/integration/test-vm1-programs.lisp`

---

## ÉTAPE 5 : AUTO-COMPILATION DU COMPILATEUR (4-5h)

### Objectif
Le compilateur se compile lui-même (métacircularité)

### 5.1 Préparation du compilateur (1h)

**Actions :**
- [ ] Simplifier le compilateur si nécessaire
- [ ] S'assurer que toutes les primitives sont implémentées
- [ ] Créer une version "minimal" si le complet est trop gros

**Stratégie de simplification :**
```lisp
;; Compiler seulement les fonctions essentielles:
;; - compile-expr (cœur)
;; - compile-defun (définitions)
;; - compile-if (conditionnels)
;; - compile-call (appels)
;; Sans: CASE, COND, DOTIMES (peuvent être réécris)
```

**Livrable :** `src/compiler-minimal.lisp`

### 5.2 Compilation du compilateur (2h)

**Actions :**
- [ ] Compiler `compiler-minimal.lisp` → ASM
- [ ] Sauvegarder le résultat
- [ ] Analyser la taille du code généré

**Commande :**
```lisp
;; Compiler le compilateur
(load "main.lisp")
(load "src/compiler-bootstrap.lisp")

(defparameter *compiler-compiled*
  (compile-file-to-asm "src/compiler-minimal.lisp"))

;; Sauvegarder
(save-asm *compiler-compiled* "output/compiler-compiled.asm")

;; Statistiques
(format t "Taille: ~A instructions~%" (length *compiler-compiled*))
```

**Estimation :** 5000-10000 instructions MIPS

**Livrable :** `output/compiler-compiled.asm`

### 5.3 Chargement du compilateur compilé (1h)

**Actions :**
- [ ] Charger compiler₁ dans VM₀
- [ ] Tester une compilation simple
- [ ] Compiler `(+ 2 3)` avec compiler₁

**Test :**
```lisp
;; Charger compiler₁
(load-asm-file "output/compiler-compiled.asm" *vm*)

;; Appeler compiler₁ pour compiler (+ 2 3)
(defparameter *result*
  (vm-call-function *vm* 'compile-expr '(+ 2 3) *empty-env*))

;; *result* doit contenir le code MIPS pour (+ 2 3)
;; Quelque chose comme:
;;   (:ADDI $t0 $zero 2)
;;   (:ADDI $t1 $zero 3)
;;   (:ADD $v0 $t0 $t1)
```

**Critère de succès :**
✅ compiler₁ génère du code MIPS valide  
✅ Le code généré est identique à compiler₀  

**Livrable :** Tests de comparaison

### 5.4 Vérification du point fixe (1h)

**Concept :** Un compilateur atteint le point fixe quand :
```
compiler₀(source) = compiler₁(source) = compiler₂(source)
```

**Actions :**
- [ ] Compiler le même programme avec compiler₀ et compiler₁
- [ ] Comparer les résultats instruction par instruction
- [ ] Documenter les différences (s'il y en a)

**Test du point fixe :**
```lisp
;; Code test
(defparameter *test-program*
  '(defun square (x) (* x x)))

;; Compilation avec compiler₀ (natif)
(defparameter *asm0*
  (compile-to-asm *test-program*))

;; Compilation avec compiler₁ (compilé)
(defparameter *asm1*
  (vm-compile *vm* *test-program*))

;; Comparaison
(equal *asm0* *asm1*) ; → T (succès!)
```

**Critère de succès :**
✅ compiler₀ et compiler₁ génèrent le même code  
✅ Point fixe atteint dès la première itération  

**Livrable :** `docs/POINT_FIXE.md`

---

## ÉTAPE 6 : MESURES ET OPTIMISATIONS (2-3h)

### Objectif
Benchmarker et documenter les performances

### 6.1 Benchmark complet (1h)

**Actions :**
- [ ] Créer une suite de programmes tests
- [ ] Mesurer temps d'exécution VM₀ vs VM₁
- [ ] Mesurer nombre d'instructions
- [ ] Mesurer utilisation mémoire

**Programmes de test :**
```lisp
;; Suite de benchmarks
(defparameter *benchmarks*
  '((fibonacci 10)
    (factorial 10)
    (sum-list (1 2 3 4 5))
    (quick-sort (5 2 8 1 9))
    (map-square (1 2 3 4 5))))
```

**Métriques à mesurer :**
- Temps CPU (secondes)
- Instructions exécutées
- Mémoire utilisée (heap + stack)
- Ratio VM₁/VM₀

**Livrable :** `docs/BENCHMARK_COMPLET.md`

### 6.2 Analyse des goulots d'étranglement (1h)

**Actions :**
- [ ] Identifier les parties les plus lentes
- [ ] Profiler le code généré
- [ ] Trouver les opportunités d'optimisation

**Outils :**
```lisp
;; Profiler basique
(defun profile-vm (vm program)
  "Compte les instructions par type"
  (let ((counts (make-hash-table)))
    (dolist (instr program)
      (incf (gethash (first instr) counts 0)))
    counts))
```

**Livrable :** Rapport d'analyse

### 6.3 Optimisations ciblées (1h optionnel)

**Actions possibles :**
- [ ] Optimiser le dispatch d'instructions (jump table)
- [ ] Réduire les accès mémoire redondants
- [ ] Inline les petites fonctions
- [ ] Utiliser plus de registres

**Note :** Optionnel, seulement si temps disponible

**Livrable :** Liste d'optimisations appliquées

---

## 📊 VALIDATION FINALE

### Tests d'intégration

- [ ] **Test 1 :** Loader compilé charge du code ✅
- [ ] **Test 2 :** VM₁ exécute fibonacci(10) = 55 ✅
- [ ] **Test 3 :** compiler₁ compile (+ 2 3) correctement ✅
- [ ] **Test 4 :** Point fixe atteint (compiler₀ = compiler₁) ✅
- [ ] **Test 5 :** Tous les tests unitaires passent (84/84) ✅

### Démonstration complète

**Scénario final :**
```lisp
;; 1. Démarrer VM₀
(defparameter *vm0* (make-vm :verbose nil))

;; 2. Compiler et charger VM₁
(load-vm1 *vm0*)

;; 3. Dans VM₁, charger le loader compilé
(vm1-load-loader)

;; 4. Dans VM₁, utiliser le loader pour charger fibonacci
(vm1-loader-load "fib10.asm")

;; 5. Exécuter fibonacci(10) dans VM₁
(vm1-run)

;; 6. Récupérer le résultat
(vm1-get-result) ; → 55

;; 7. Afficher les statistiques
(vm1-stats)
;; Instructions VM₁: 177
;; Instructions VM₀: 88,500
;; Ratio: 500x
;; Temps: 0.8s
```

**Critère de succès final :**
✅ Démonstration complète fonctionne de bout en bout  
✅ Documentation exhaustive  
✅ Code propre et commenté  

---

## 📝 DOCUMENTATION À CRÉER

### Documents techniques

1. **AUDIT_DEPENDANCES.md** - Liste des dépendances externes
2. **ARCHITECTURE_VM1.md** - Structure mémoire de VM₁
3. **BENCHMARK_VM1.md** - Performances fibonacci
4. **POINT_FIXE.md** - Vérification métacircularité
5. **BENCHMARK_COMPLET.md** - Suite complète de benchmarks
6. **PHASE10_CONCLUSION.md** - Bilan et leçons apprises

### Code source

1. **src/primitives.lisp** - Primitives réécrites
2. **src/compiler-bootstrap.lisp** - Compilateur adapté
3. **src/compiler-minimal.lisp** - Version simplifiée
4. **src/vm-bootstrap.lisp** - Structures pour VM₁

### Tests

1. **tests/integration/test-loader-compiled.lisp**
2. **tests/integration/test-vm1-basic.lisp**
3. **tests/integration/test-vm1-programs.lisp**
4. **tests/integration/test-point-fixe.lisp**

### Résultats

1. **output/loader-compiled.asm**
2. **output/vm-compiled.asm**
3. **output/compiler-compiled.asm**

---

## ⏱️ PLANNING DÉTAILLÉ

| Étape | Tâche | Durée | Jour | Statut |
|-------|-------|-------|------|--------|
| **1** | **Préparation compilateur** | **4-5h** | **J1** | ⏸️ |
| 1.1 | Audit dépendances | 1h | J1 | ⏸️ |
| 1.2 | Implémentation primitives | 2h | J1 | ⏸️ |
| 1.3 | Adaptation compilateur | 1-2h | J1 | ⏸️ |
| **2** | **Compilation loader** | **3-4h** | **J2** | ⏸️ |
| 2.1 | Analyse loader | 30min | J2 | ⏸️ |
| 2.2 | Compilation fonctions | 2h | J2 | ⏸️ |
| 2.3 | Tests loader compilé | 1h | J2 | ⏸️ |
| **3** | **Compilation VM** | **4-5h** | **J3** | ⏸️ |
| 3.1 | Analyse VM | 1h | J3 | ⏸️ |
| 3.2 | Représentation mémoire | 1h | J3 | ⏸️ |
| 3.3 | Compilation boucle exec | 2h | J3 | ⏸️ |
| 3.4 | Tests VM₁ basiques | 1h | J3 | ⏸️ |
| **4** | **Test fibonacci VM₁** | **2-3h** | **J4** | ⏸️ |
| 4.1 | Compilation fibonacci | 30min | J4 | ⏸️ |
| 4.2 | Chargement dans VM₁ | 1h | J4 | ⏸️ |
| 4.3 | Exécution et mesure | 1h | J4 | ⏸️ |
| 4.4 | Tests supplémentaires | 30min | J4 | ⏸️ |
| **5** | **Auto-compilation** | **4-5h** | **J5** | ⏸️ |
| 5.1 | Préparation compilateur | 1h | J5 | ⏸️ |
| 5.2 | Compilation compilateur | 2h | J5 | ⏸️ |
| 5.3 | Chargement compiler₁ | 1h | J5 | ⏸️ |
| 5.4 | Vérification point fixe | 1h | J5 | ⏸️ |
| **6** | **Mesures finales** | **2-3h** | **J6** | ⏸️ |
| 6.1 | Benchmark complet | 1h | J6 | ⏸️ |
| 6.2 | Analyse goulots | 1h | J6 | ⏸️ |
| 6.3 | Optimisations | 1h | J6 | ⏸️ |

**Total estimé :** 19-25 heures  
**Durée calendaire :** 6 jours de travail (3-4h/jour)

---

## 🎯 CRITÈRES DE SUCCÈS PHASE 10

### Objectifs minimaux (MVP)

✅ **O1 :** Loader compilé fonctionne  
✅ **O2 :** VM₁ exécute fibonacci(10) = 55  
✅ **O3 :** Mesures de performance documentées  

### Objectifs avancés

✅ **O4 :** Compilateur s'auto-compile (point fixe)  
✅ **O5 :** Suite complète de benchmarks  
✅ **O6 :** Documentation exhaustive  

### Objectifs stretch (bonus)

✅ **O7 :** Optimisations de performance  
✅ **O8 :** VM₂ dans VM₁ (triple niveau)  
✅ **O9 :** Interface de démonstration interactive  

---

## ⚠️ RISQUES ET MITIGATION

### Risque 1 : Taille du code généré trop importante

**Probabilité :** Haute  
**Impact :** Élevé  

**Mitigation :**
- Créer version "minimal" du compilateur
- Augmenter taille mémoire VM si nécessaire
- Compiler seulement les fonctions essentielles

### Risque 2 : Performances trop faibles

**Probabilité :** Moyenne  
**Impact :** Moyen  

**Mitigation :**
- Accepter le ratio 500-1000x (normal pour simulation)
- Tester avec fibonacci(5) si (10) est trop lent
- Optimiser les parties critiques seulement

### Risque 3 : Bugs dans la métacircularité

**Probabilité :** Moyenne  
**Impact :** Élevé  

**Mitigation :**
- Tests unitaires exhaustifs à chaque étape
- Valider le point fixe avec plusieurs programmes
- Comparer octet par octet si nécessaire

### Risque 4 : Dépendances non résolvables

**Probabilité :** Faible  
**Impact :** Élevé  

**Mitigation :**
- Audit complet dès l'étape 1
- Réécrire en LISP pur toutes les dépendances
- Simplifier le compilateur si nécessaire

---

## 🚀 COMMANDE DE DÉMARRAGE

Pour commencer la Phase 10 immédiatement :

```bash
# 1. Créer branche Git
cd "/home/etudiant/Bureau/CLisp/TD LISP-20251009/VirtualMachine_CLISP"
git checkout -b phase10-bootstrap

# 2. Créer structure de répertoires
mkdir -p output tests/integration

# 3. Baseline des tests actuels
./run-unit-tests.sh | tee baseline-phase10.log

# 4. Démarrer Étape 1.1 : Audit des dépendances
grep -rn "format\|apply\|funcall\|mapcar\|remove-if" src/compiler.lisp > docs/audit-temp.txt

echo "✅ Phase 10 démarrée! Prochaine action: Étape 1.1 - Audit des dépendances"
```

---

## 📚 RESSOURCES ET RÉFÉRENCES

### Théorie du bootstrap

- [Reflections on Trusting Trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf) - Ken Thompson (1984)
- [Bootstrapping a self-compiling compiler](https://www.cs.virginia.edu/~evans/cs655/readings/trusting.html)

### Compilateurs métacirculaires

- SICP Chapter 4 - Metacircular Evaluator
- Lisp in Small Pieces - Christian Queinnec

### Exemples de VM dans VM

- JVM running on JVM (Java)
- PyPy (Python interpreter written in Python)
- Squeak (Smalltalk VM in Smalltalk)

---

## ✅ CHECKLIST DE PRÉPARATION

### Avant de commencer

- [x] Phase 9 complétée (84/84 tests passants)
- [x] Documentation Phase 9 à jour
- [x] Git propre (pas de modifications non committées)
- [ ] Branche `phase10-bootstrap` créée
- [ ] Répertoires `output/` et `tests/integration/` créés
- [ ] Temps disponible: 15-20h sur 1-2 semaines
- [ ] Plan d'action lu et compris
- [ ] Papier/whiteboard pour schémas architecturaux

### Outils nécessaires

- [x] Common Lisp (CLISP) installé
- [x] Éditeur de code configuré
- [x] Tests unitaires fonctionnels
- [ ] Outil de profiling (optionnel)
- [ ] Chronomètre/timer pour benchmarks

---

**FIN DU PLAN D'ACTION PHASE 10**

**Date de création :** 27 novembre 2025  
**Version :** 1.0  
**Auteur :** Architecture basée sur Phase 9 complétée  
**Status :** ⏸️ PRÊT À DÉMARRER

**Prochaine action immédiate :** Étape 1.1 - Audit des dépendances (1h)
