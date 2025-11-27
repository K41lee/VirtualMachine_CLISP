# 🚀 Compilateur LISP → MIPS + Machine Virtuelle

Un système complet de compilation et d'exécution développé en Common LISP, comprenant un compilateur LISP vers assembleur MIPS et une machine virtuelle MIPS.

## 🎯 Objectif du Projet

Développer un système permettant de :
1. **Compiler** du code LISP en assembleur MIPS
2. **Exécuter** le code MIPS sur une machine virtuelle
3. **Supporter** les closures et la récursivité
4. **Comparer** les performances avec LISP natif

**Statut** : ✅ **PHASE 10 COMPLÉTÉE** - Bootstrap 100% fonctionnel  
**Phase 11** : 🚀 **VM₁ COMPILATION EN COURS** - Compiler la VM en MIPS (27 nov. 2025)

## 📦 Structure du Projet

Voir [STRUCTURE_PROJET.md](STRUCTURE_PROJET.md) pour la structure complète détaillée.

```
VirtualMachine_CLISP/
├── src/                          # Code source principal
│   ├── vm.lisp                   # Machine virtuelle MIPS (686 lignes)
│   ├── compiler.lisp             # Compilateur LISP → MIPS (1886 lignes)
│   ├── loader.lisp               # Chargeur avec HALT automatique
│   ├── asm-ops.lisp              # 55 instructions MIPS + config 4 Mo
│   ├── utils.lisp                # Fonctions utilitaires
│   └── bootstrap/                # Phase 10 (historique, non utilisé en Phase 11)
│
├── tests/                        # Tests organisés par type
│   ├── unit/                     # Tests unitaires
│   ├── integration/              # Tests d'intégration  
│   ├── validation/               # Tests de validation finale
│   ├── performance/              # Tests de performance
│   │   └── results/              # Résultats benchmarks
│   └── debug/                    # Tests de débogage
│
├── docs/                         # Documentation complète
│   ├── CHANGELOG.md              # Historique des changements
│   ├── history/                  # Documents datés
│   ├── phases/                   # Documentation par phase
│   │   ├── phase10/              # 13 documents Phase 10
│   │   └── phase11/              # Plan Phase 11
│   ├── FichierTexteSuivi/        # Suivi détaillé
│   └── Ressource_externe/        # Spécifications MIPS
│
├── scripts/                      # Scripts de build et test
│   ├── run-all-tests.sh          # Tous les tests
│   ├── run-unit-tests.sh         # Tests unitaires
│   └── phase10/                  # Scripts Phase 10
│
├── bootstrap/                    # Code bootstrap Phase 10 (historique)
│   └── src/                      # Sources bootstrap
│
├── examples/                     # Exemples de programmes LISP
├── archive/                      # Fichiers obsolètes archivés
├── logs/                         # Fichiers de log
├── output/                       # Sorties temporaires
├── test-results/                 # Résultats de tests
│
├── main.lisp                     # Point d'entrée principal
├── README.md                     # Ce fichier
└── STRUCTURE_PROJET.md           # Structure détaillée du projet
```

## 🚀 Installation et Lancement

### Prérequis
- **Common LISP** (CLISP, SBCL, ou autre implémentation)

### Démarrage Rapide
```bash
clisp main.lisp
```

## 💻 Utilisation

### Compiler et Exécuter
```lisp
(compile-and-run '(+ 5 3))
; => 8

(compile-and-run '(let ((y 10))
                    (let ((f (lambda (x) (+ x y))))
                      (+ (f 1) (f 2)))))
; => 23 ✓
```

## 🎯 Fonctionnalités

### ✅ Phase 9 : CLOSURES (COMPLÉTÉE)

#### Tests Closures (5/5) ✅
| Test | Description | Résultat |
|------|-------------|----------|
| 1 | Sans capture | 6 ✓ |
| 2 | Avec capture | 15 ✓ |
| 3 | Retournée | 8 ✓ |
| 4 | Multiples captures | 18 ✓ |
| 5 | Appels multiples | 23 ✓ |

### Structures Complètes
- ✅ IF, COND, WHEN, UNLESS, CASE
- ✅ AND, OR, NOT (court-circuit)
- ✅ LOOP, DOTIMES
- ✅ LET, SETQ, LABELS, LAMBDA
- ✅ +, -, *, /, mod, abs, max, min

## �� Bug Critique Résolu : Format LW

**Problème** : Incohérence format LW entre compilateur et VM  
**Solution** : 21 corrections pour unifier vers `(LW dest base offset)`  
**Résultat** : ✅ 100% des tests passent

## 📊 Tests : 84/84 (100%) ✅

| Catégorie | Tests |
|-----------|-------|
| Closures | 5/5 ✅ |
| Heap | 4/4 ✅ |
| Variables libres | 17/17 ✅ |
| Math | 21/21 ✅ |
| Autres | 37/37 ✅ |

## 📚 Documentation

### Phase 9 : Closures
- [PHASE9_PROGRESS.md](docs/PHASE9_PROGRESS.md) - Progression Phase 9
- [CLOSURES_DESIGN.md](docs/CLOSURES_DESIGN.md) - Design closures

### Phase 10 : Bootstrap (✅ COMPLÉTÉ)
- [SUCCES_TOTAL_100%.md](docs/phases/phase10/SUCCES_TOTAL_100%.md) - Validation 7/7 tests
- [PERFORMANCE_TESTS.md](docs/phases/phase10/PERFORMANCE_TESTS.md) - Benchmarks
- [RAPPORT_FINAL_PHASE10.md](docs/phases/phase10/RAPPORT_FINAL_PHASE10.md) - Rapport complet

### Phase 11 : VM₁ Compilation (🚀 EN COURS)
- [PLAN_ACTION_VM1.txt](docs/phases/phase11/PLAN_ACTION_VM1.txt) - Plan détaillé simplifié (8 phases)
- [ARCHITECTURE_VM0_VM1.txt](docs/phases/phase11/ARCHITECTURE_VM0_VM1.txt) - Architecture clarifiée
- [CHECKLIST_PHASE11.txt](docs/phases/phase11/CHECKLIST_PHASE11.txt) - Checklist détaillée
- **Objectif** : Compiler src/vm.lisp (686 lignes) en MIPS pour l'exécuter dans VM₀
- **Note** : ⚠️ VM₀ = VM native (LISP), VM₁ = **MÊME VM** compilée (MIPS)
- **Durée estimée** : 25-32 heures (8 phases)
- **Extensions nécessaires** : loops, arrays, case, simplification VM

## 📈 Performances Actuelles (Mémoire 4 Mo)

| Plateforme | Temps | Overhead |
|------------|-------|----------|
| LISP natif | 62 µs | 1x (référence) |
| VM₀ native | 1.448 s | 23,356x |
| VM₁ (simulation) | 522 ms | 8,429x |

**Configuration**: 1,048,576 mots (4 Mo) - Upgrade pour VM-on-VM  
Voir rapport complet : `tests/performance/results/RAPPORT_PERFORMANCE_FINALE.txt`

## 🎯 Prochaines Étapes

1. [ ] Implémenter extensions compilateur (Phase 11)
   - [ ] Boucles (while/loop)
   - [ ] Arrays (make-array, aref)
   - [ ] Case/switch
   - [ ] Simplifier hash-tables
   - [ ] Defstruct basique

2. [ ] Simplifier VM pour compilation
3. [ ] Compiler VM en MIPS
4. [ ] Tester VM₁ dans VM₀
5. [ ] Mesurer performances VM₁ réelle

## 👥 Contribution

**Auteur** : K41lee  
**Date** : Novembre 2025  
**Statut** : 
- ✅ Phase 9 Complétée (Closures)
- ✅ Phase 10 Complétée (Bootstrap)
- 🚀 Phase 11 En Cours (VM₁ Compilation)

---

**Dernière mise à jour** : 27 novembre 2025
