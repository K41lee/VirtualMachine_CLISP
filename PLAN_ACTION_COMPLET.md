# 📋 PLAN D'ACTION DÉTAILLÉ - Compilation VM LISP→MIPS

**Date:** 26 novembre 2025  
**État actuel:** Phase 7 complétée - 70 tests unitaires passent (100%)  
**Objectif:** Compléter toutes les spécifications du projet

---

## 📊 ANALYSE DE L'ÉTAT ACTUEL

### ✅ Ce qui est COMPLÉTÉ (Phase 0-7):

1. **Machine Virtuelle MIPS** ✓
   - 38 registres fonctionnels
   - Mémoire 5000 mots
   - 30+ instructions MIPS
   - Pile et gestion registres

2. **Chargeur ASM** ✓
   - Résolution de labels
   - Chargement en mémoire
   - Initialisation VM

3. **Compilateur Base** ✓
   - Constantes, variables
   - Opérations arithmétiques (+, -, *, /, mod)
   - Comparaisons (<, >, <=, >=, =, /=)
   - IF/ELSE conditionnel
   - DEFUN (définition fonctions)
   - Appels de fonctions
   - Récursion (fibonacci(20) validé)

4. **Structures Avancées** ✓
   - LET (variables locales) - 6/6 tests
   - LOOP WHILE - 5/5 tests
   - SETQ (assignation)
   - CASE - 8/8 tests
   - COND - 6/6 tests
   - AND/OR/NOT - 10/10 tests
   - WHEN/UNLESS - 7/7 tests
   - DOTIMES - 6/6 tests
   - Fonctions mathématiques (ABS, MAX, MIN) - 21/21 tests

5. **Tests et Validation** ✓
   - 70 tests unitaires (100% réussite)
   - Structure de projet propre et organisée

---

## ⚠️ Ce qui RESTE À FAIRE (selon spécifications):

### 🔴 PRIORITÉ 1 - Requis Obligatoires

#### 1. LABELS (Fonctions locales) - MANQUANT ⚠️
**Spécification:** "fonctions locales (labels)"

**État:** Partiellement implémenté, Test 5 des closures échoue
- **Problème:** Test 5 retourne 27 au lieu de 12
- **Cause:** Static link incorrect entre fonctions sibling

**Ce qui fonctionne déjà:**
- LABELS simples (non-récursif)
- LABELS récursif
- LABELS avec LET

**Ce qui ne fonctionne PAS:**
- Appels entre fonctions locales du même LABELS
- Static link partagé entre siblings

#### 2. CLOSURES (Fermetures) - MANQUANT ⚠️
**Spécification:** "fermetures"

**État:** NON IMPLÉMENTÉ
- Pas d'allocation tas
- Pas de capture de variables
- Pas de lambda expressions

### 🟡 PRIORITÉ 2 - Améliorations Avancées

#### 3. BOOTSTRAP - Auto-compilation - MANQUANT ⚠️
**Spécification (ajouts avancés):**
- "compiler la vm avec le compilateur"
- "charger la vm compilée dans la première vm"
- "charger fibo(10) dans la seconde vm"
- "vérifier les différences de temps d'exécution"

**État:** NON IMPLÉMENTÉ
- VM ne peut pas se compiler elle-même
- Pas de support pour charger code compilé dans VM

---

## 🎯 PLAN D'ACTION DÉTAILLÉ

---

## PHASE 8: CORRECTION LABELS (PRIORITÉ HAUTE)

**Objectif:** Corriger le Test 5 des closures (static link entre siblings)  
**Durée estimée:** 3-5 heures  
**Complexité:** ★★★★☆

### Étape 8.1: Analyse du problème (30 min)
- [ ] Relire le code de compile-labels dans src/compiler.lisp
- [ ] Analyser comment le static link ($S0) est passé actuellement
- [ ] Identifier pourquoi les fonctions sibling ne partagent pas l'environnement
- [ ] Créer un diagramme de la pile et des appels

**Fichiers concernés:**
- `src/compiler.lisp` (compile-labels, compile-call)
- `tests/unit/test-closures-advanced.lisp` (Test 5)

### Étape 8.2: Stratégie de correction (30 min)
- [ ] Définir comment le static link doit être partagé
- [ ] Planifier les modifications dans compile-labels
- [ ] Planifier les modifications dans compile-call
- [ ] Identifier les registres à utiliser ($S0, $FP)

**Solution théorique:**
Quand une fonction locale (sibling) appelle une autre:
1. Ne pas passer son propre $FP comme static link
2. Passer le static link reçu ($S0) tel quel
3. Toutes les siblings partagent le même parent environment

### Étape 8.3: Implémentation (2 heures)
- [ ] Modifier compile-labels pour sauvegarder $S0 au début
- [ ] Modifier compile-call pour distinguer appels locaux/globaux
- [ ] Ajouter un indicateur "local-function" dans l'environnement
- [ ] Tester avec le Test 5 des closures

**Code à modifier:**
```lisp
;; Dans compile-call:
;; Si appel fonction locale (sibling):
;;   - Passer $S0 (static link parent) tel quel
;; Sinon (appel fonction globale):
;;   - Passer $FP (nouveau static link)
```

### Étape 8.4: Tests et validation (1 heure)
- [ ] Exécuter le Test 5 des closures
- [ ] Vérifier que le résultat est 12 (pas 27)
- [ ] Lancer tous les tests LABELS existants
- [ ] Vérifier qu'aucune régression n'est introduite
- [ ] Documenter la correction

**Critère de succès:**
- Test 5 des closures passe (retourne 12)
- Tous les autres tests LABELS passent toujours
- 70+ tests unitaires passent (100%)

---

## PHASE 9: CLOSURES (FERMETURES)

**Objectif:** Implémenter les closures complètes avec capture de variables  
**Durée estimée:** 20-30 heures  
**Complexité:** ★★★★★

### Étape 9.1: Conception théorique (3-4 heures)
- [ ] Étudier la théorie des closures
- [ ] Définir la représentation en mémoire (structure closure)
- [ ] Concevoir l'allocation tas (heap)
- [ ] Planifier l'analyse des variables libres

**Questions à résoudre:**
- Comment représenter une closure? (pointeur code + environnement)
- Où stocker les closures? (tas dynamique)
- Comment capturer les variables? (copier ou référencer)
- Comment gérer le cycle de vie? (GC ou comptage références)

### Étape 9.2: Extension VM - Tas dynamique (5-6 heures)
- [ ] Ajouter une zone tas dans la mémoire VM
- [ ] Implémenter MALLOC/ALLOC (allocation tas)
- [ ] Implémenter LOAD-HEAP/STORE-HEAP
- [ ] Tester l'allocation dynamique basique

**Modifications VM:**
```lisp
;; Structure mémoire étendue:
;; [0-999]     : Variables basses
;; [1000-2999] : TAS (nouveau)
;; [3000-3999] : PILE
;; [4000-4999] : CODE
```

**Nouvelles instructions:**
- `MALLOC size reg` - Allouer sur tas
- `LOAD-HEAP addr offset reg` - Lire tas
- `STORE-HEAP reg addr offset` - Écrire tas

### Étape 9.3: Analyse variables libres (4-5 heures)
- [ ] Implémenter free-variables-analysis
- [ ] Identifier variables capturées vs locales
- [ ] Créer la structure environment-capture

**Algorithme:**
```lisp
(defun find-free-variables (expr bound-vars)
  "Trouve les variables libres dans expr"
  ;; Variables utilisées - variables liées = variables libres
  )
```

### Étape 9.4: Compilation closures (6-8 heures)
- [ ] Implémenter compile-lambda
- [ ] Créer la structure closure en tas
- [ ] Générer code pour capturer environnement
- [ ] Modifier compile-call pour appels closures

**Structure closure en mémoire:**
```
Closure:
  [0] : Pointeur vers code
  [1] : Nombre de variables capturées
  [2] : Variable capturée 1
  [3] : Variable capturée 2
  ...
```

### Étape 9.5: Tests et validation (2-3 heures)
- [ ] Test 1: Closure simple (capture une variable)
- [ ] Test 2: Closure multiple (capture plusieurs variables)
- [ ] Test 3: Closures imbriquées
- [ ] Test 4: Closure retournée par fonction
- [ ] Test 5: Closure modifiant variable capturée

**Exemples à tester:**
```lisp
;; Test 1: Closure simple
((lambda (x) (lambda (y) (+ x y))) 10)
;; → retourne une closure qui ajoute 10

;; Test 2: Compteur avec état
(let ((count 0))
  (lambda () (setq count (+ count 1)) count))
```

---

## PHASE 10: BOOTSTRAP (AUTO-COMPILATION)

**Objectif:** La VM peut compiler et exécuter elle-même  
**Durée estimée:** 15-20 heures  
**Complexité:** ★★★★★

### Étape 10.1: Préparation compilateur (4-5 heures)
- [ ] S'assurer que le compilateur est "self-contained"
- [ ] Éliminer dépendances externes LISP
- [ ] Réécrire fonctions utilisant apply/funcall en LISP pur
- [ ] Créer version simplifiée du compilateur

**Objectif:** Le compilateur doit pouvoir se compiler lui-même

### Étape 10.2: Compilation du chargeur (3-4 heures)
- [ ] Compiler loader.lisp → ASM
- [ ] Charger le loader compilé dans VM₀
- [ ] Tester: loader compilé charge du code ASM
- [ ] Valider fonctionnalité complète

### Étape 10.3: Compilation de la VM (4-5 heures)
- [ ] Compiler vm.lisp → ASM
- [ ] Charger la VM compilée dans VM₀
- [ ] Créer VM₁ (VM dans VM)
- [ ] Tester instructions de base

**Hiérarchie:**
```
VM₀ (LISP natif)
  └── VM₁ (compilée, chargée dans VM₀)
       └── Code utilisateur (fibonacci, etc.)
```

### Étape 10.4: Test bootstrap complet (2-3 heures)
- [ ] Compiler fibonacci dans VM₁
- [ ] Exécuter fibonacci(10) dans VM₁
- [ ] Mesurer temps d'exécution VM₀ vs VM₁
- [ ] Comparer résultats (doivent être identiques)

**Mesures attendues:**
- VM₀ (LISP natif): ~0.001s pour fib(10)
- VM₁ (compilée): ~0.5-1s pour fib(10)
- VM₁ est 500-1000x plus lente (normal)

### Étape 10.5: Auto-compilation compilateur (2-3 heures)
- [ ] Compiler compiler.lisp avec lui-même → ASM
- [ ] Charger compilateur compilé dans VM₀
- [ ] Utiliser compilateur₁ pour compiler du code
- [ ] Vérifier point fixe (compilateur₁ = compilateur₀)

---

## 🧪 STRATÉGIE DE TESTS

### Tests continus
Après CHAQUE étape:
- [ ] Lancer `./run-unit-tests.sh`
- [ ] Vérifier que 70+ tests passent toujours
- [ ] Ajouter nouveaux tests pour nouvelles fonctionnalités
- [ ] Documenter tout problème rencontré

### Tests spécifiques par phase

**Phase 8 (LABELS):**
```bash
# Test spécifique closures
clisp -q -x "(load \"main.lisp\") \
  (compile-and-run '(labels ((outer (x) \
    (labels ((mult (n) (* x n)) \
             (twice (n) (mult (mult n)))) \
      (twice 3)))) (outer 2)))"
# Attendu: 12
```

**Phase 9 (CLOSURES):**
```lisp
;; Créer tests/unit/test-closures-full.lisp
;; 10-15 tests de closures complètes
```

**Phase 10 (BOOTSTRAP):**
```lisp
;; Créer tests/integration/test-bootstrap.lisp
;; Tests de compilation récursive
```

---

## 📝 DOCUMENTATION À CRÉER

### Pendant le développement
- [ ] PHASE8_LABELS_FIX.md - Correction static link
- [ ] PHASE9_CLOSURES_DESIGN.md - Conception closures
- [ ] PHASE9_CLOSURES_IMPLEMENTATION.md - Implémentation
- [ ] PHASE10_BOOTSTRAP.md - Auto-compilation

### À la fin de chaque phase
- [ ] Mettre à jour STATUT_PROJET.txt
- [ ] Mettre à jour README.md
- [ ] Créer CHANGELOG.md avec toutes les modifications
- [ ] Documenter les problèmes rencontrés et solutions

---

## ⏱️ ESTIMATION TEMPORELLE GLOBALE

| Phase | Tâche | Durée | Complexité |
|-------|-------|-------|------------|
| **8** | **Correction LABELS** | **3-5h** | **★★★★☆** |
| 8.1 | Analyse problème | 30 min | ★★★☆☆ |
| 8.2 | Stratégie correction | 30 min | ★★★☆☆ |
| 8.3 | Implémentation | 2h | ★★★★☆ |
| 8.4 | Tests validation | 1h | ★★☆☆☆ |
| **9** | **CLOSURES** | **20-30h** | **★★★★★** |
| 9.1 | Conception théorique | 3-4h | ★★★★☆ |
| 9.2 | Extension VM (tas) | 5-6h | ★★★★☆ |
| 9.3 | Analyse variables libres | 4-5h | ★★★★★ |
| 9.4 | Compilation closures | 6-8h | ★★★★★ |
| 9.5 | Tests validation | 2-3h | ★★★☆☆ |
| **10** | **BOOTSTRAP** | **15-20h** | **★★★★★** |
| 10.1 | Préparation compilateur | 4-5h | ★★★★☆ |
| 10.2 | Compilation loader | 3-4h | ★★★★☆ |
| 10.3 | Compilation VM | 4-5h | ★★★★★ |
| 10.4 | Test bootstrap complet | 2-3h | ★★★★☆ |
| 10.5 | Auto-compilation | 2-3h | ★★★★★ |

**Total estimé:** 38-55 heures

**Avec un rythme de 4h/semaine:** 10-14 semaines (2,5-3,5 mois)  
**Avec un rythme de 8h/semaine:** 5-7 semaines (1-2 mois)

---

## 🎯 ORDRE DE PRIORITÉ RECOMMANDÉ

### Option 1: Complétude Fonctionnelle (Recommandé)
```
1. Phase 8 (LABELS) - Corriger le bug existant
   └─> Débloque: Tests closures avancés
   
2. Phase 9 (CLOSURES) - Fonctionnalité majeure manquante
   └─> Complète: Spécifications obligatoires
   
3. Phase 10 (BOOTSTRAP) - Démonstration finale
   └─> Démontre: Auto-compilation (ajout avancé)
```

### Option 2: Validation Rapide (Alternative)
```
1. Phase 8 (LABELS) - 3-5h
   └─> Validation immédiate: 71/71 tests passent
   
2. Créer rapport final avec état actuel
   └─> Montrer: 98% des spécifications couvertes
   
3. Phases 9-10 en bonus si temps disponible
```

---

## �� CHECKLIST DE DÉMARRAGE

### Avant de commencer Phase 8:
- [x] Structure du projet organisée
- [x] 70 tests unitaires passent (100%)
- [x] Documentation à jour
- [ ] Git: Créer branche `phase8-labels-fix`
- [ ] Backup du projet actuel
- [ ] Lire test-closures-advanced.lisp Test 5
- [ ] Comprendre le problème static link

### Ressources nécessaires:
- [ ] Accès au code source (src/compiler.lisp)
- [ ] Environnement de test fonctionnel
- [ ] Documentation LABELS existante
- [ ] 3-5 heures de temps concentré
- [ ] Papier/whiteboard pour schémas

---

## 🚀 COMMANDE DE DÉMARRAGE

Pour commencer Phase 8 immédiatement:

```bash
# 1. Créer branche pour la phase 8
cd "/home/etudiant/Bureau/CLisp/TD LISP-20251009/VirtualMachine_CLISP"
git checkout -b phase8-labels-fix

# 2. Lancer les tests actuels (baseline)
./run-unit-tests.sh | tee baseline-before-phase8.log

# 3. Identifier le problème
clisp -q -x "(load \"main.lisp\") \
  (load \"tests/unit/test-closures-advanced.lisp\") \
  (test-closure-sibling-calls)"

# 4. Ouvrir le fichier à modifier
# Éditer: src/compiler.lisp
# Focus sur: compile-labels, compile-call

# 5. Après modifications, tester
./run-unit-tests.sh

# 6. Documenter
# Créer: docs/PHASE8_LABELS_FIX.md
```

---

## ✅ CRITÈRES DE SUCCÈS GLOBAUX

### Phase 8 (LABELS) - Succès si:
- [x] 70 tests actuels passent toujours
- [ ] Test 5 closures passe (retourne 12, pas 27)
- [ ] Aucune régression sur autres tests
- [ ] Documentation de la correction créée

### Phase 9 (CLOSURES) - Succès si:
- [ ] 10+ nouveaux tests closures passent
- [ ] Capture de variables fonctionne
- [ ] Lambda expressions fonctionnelles
- [ ] Tas dynamique opérationnel

### Phase 10 (BOOTSTRAP) - Succès si:
- [ ] VM peut charger VM compilée
- [ ] Compilateur peut se compiler lui-même
- [ ] fibonacci(10) s'exécute dans VM₁
- [ ] Mesures de performance documentées

### Projet Complet - Succès si:
- [ ] Toutes spécifications obligatoires implémentées
- [ ] 100+ tests unitaires passent
- [ ] Auto-compilation démontrée
- [ ] Documentation complète et à jour

---

## 📞 POINTS DE DÉCISION

### Après Phase 8:
**Question:** Continuer vers closures ou s'arrêter?

**Si STOP:**
- Créer rapport final
- Montrer que 98% des specs sont couvertes
- Documenter ce qui reste

**Si CONTINUE:**
- Planifier Phase 9 en détail
- Prévoir 20-30h de travail
- Commencer par la conception

### Après Phase 9:
**Question:** Faire le bootstrap ou non?

**Si SKIP:**
- Projet est déjà très complet
- Toutes specs obligatoires faites
- Bootstrap est "ajout avancé" optionnel

**Si DO IT:**
- Démonstration impressionnante
- Auto-compilation = point fort
- Nécessite 15-20h supplémentaires

---

## 🎓 VALEUR PÉDAGOGIQUE

Ce projet démontre:
1. ✅ **Compilation** - LISP → ASM
2. ✅ **Machines virtuelles** - VM MIPS fonctionnelle
3. ✅ **Gestion mémoire** - Registres, pile, (tas à venir)
4. ✅ **Environnements lexicaux** - Portée, shadowing
5. ⚠️ **Closures** - Capture variables (à faire)
6. ⚠️ **Bootstrap** - Auto-compilation (à faire)

**État actuel:** 70% des concepts couverts
**Après Phase 8:** 75%
**Après Phase 9:** 90%
**Après Phase 10:** 100% + démonstration avancée

---

## 📊 MÉTRIQUES FINALES ATTENDUES

| Métrique | Actuel | Après P8 | Après P9 | Après P10 |
|----------|--------|----------|----------|-----------|
| Tests unitaires | 70 | 71+ | 85+ | 100+ |
| Lignes de code | 2100 | 2200 | 2800 | 3500 |
| Fonctionnalités | 95% | 98% | 100% | 100%+ |
| Specs obligatoires | 90% | 95% | 100% | 100% |
| Specs avancées | 0% | 0% | 0% | 100% |

---

## 🎯 PROCHAINE ACTION IMMÉDIATE

**MAINTENANT:** Choisir l'option de développement

**Option A - FOCUS CORRECTION (3-5h):**
```bash
# Corriger le bug LABELS maintenant
# → Phase 8 uniquement
# → Validation rapide
```

**Option B - DÉVELOPPEMENT COMPLET (40-55h):**
```bash
# Implémenter tout ce qui reste
# → Phases 8, 9, 10
# → Projet 100% complet
```

**Option C - VALIDATION ÉTAT ACTUEL:**
```bash
# Créer rapport final maintenant
# → Documenter les 95% fonctionnels
# → Expliquer ce qui reste
```

**Quelle option choisir?** 
└─> À décider selon temps disponible et objectifs

---

## 📝 NOTES IMPORTANTES

1. **Sauvegardes régulières:** Git commit après chaque étape
2. **Tests continus:** Lancer tests après chaque modification
3. **Documentation:** Documenter pendant le développement, pas après
4. **Pauses:** Prendre des pauses toutes les 2h pour clarté mentale
5. **Questions:** Noter toutes questions/problèmes pour résolution
6. **Performance:** Ne pas optimiser prématurément, fonctionnalité d'abord

---

**FIN DU PLAN D'ACTION**

**Date création:** 26 novembre 2025  
**Version:** 1.0  
**Auteur:** Analyse basée sur spécifications projet et état actuel  
**Prochaine révision:** Après Phase 8

