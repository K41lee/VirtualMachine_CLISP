# Changelog - VirtualMachine_CLISP

## [Phase 9 Complétée] - 26 novembre 2025

### ✅ Ajouts Majeurs

#### Closures et Lambdas
- Implémentation complète des expressions LAMBDA
- Capture de variables libres via static links
- Support des appels multiples de closures
- Structure de fermeture sur le tas : `[Label][Size][Var1]...[VarN]`

#### Instructions MIPS
- `JALR` : Jump And Link Register (appel de closure)
- `JR` : Jump Register (retour de fonction)
- `LABEL` : Pseudo-instruction pour labels symboliques
- `MALLOC` : Allocation dynamique sur le tas
- `LOAD-HEAP` : Lecture depuis le tas
- `STORE-HEAP` : Écriture sur le tas

#### Analyse Statique
- Fonction `free-variables` pour identifier les variables à capturer
- Support de toutes les formes spéciales (LET, LAMBDA, LABELS, etc.)
- 17/17 tests de variables libres passants

### 🐛 Corrections Majeures

#### Bug Critique : Format LW Inconsistant
**Problème** : L'instruction LW avait un format différent entre le compilateur et la VM
- Compilateur : `(LW dest base offset)`
- VM : `(LW base offset dest)`

**Impact** :
- Crash lors de l'exécution de lambdas
- Restauration incorrecte des registres
- Appels multiples de closures impossibles

**Solution** : 21 corrections dans le code
- `src/vm.lisp` ligne ~365 : Format LW unifié
- `src/compiler.lisp` lignes 201, 575, 580, 599, 642, 644, 717, 750, 788, 1099, 1253-1256, 1296-1297, 1428-1429, 1768, 1787-1794
- **Ligne 1576** : Bug final critique - `(LW $FP 0 $FP)` → `(LW $FP $FP 0)`

**Résultat** : 100% des tests de closures passent maintenant

### 📊 Tests

#### Nouveaux Tests (26 tests)
- `test-closure-call.lisp` : 5 tests d'appels de closures (100%)
- `test-heap.lisp` : 4 tests du tas dynamique (100%)
- `test-free-variables.lisp` : 17 tests de variables libres (100%)

#### Résultats Globaux
- **Total** : 84/84 tests passants (100%)
- **Closures** : 5/5 ✅
- **Heap** : 4/4 ✅
- **Variables libres** : 17/17 ✅
- **Math** : 21/21 ✅
- **Autres** : 37/37 ✅

### 🗂️ Réorganisation

#### Fichiers Déplacés
- `test-closure-debug.lisp` → `tests/debug/`
- `test-closure-trace.lisp` → `tests/debug/`
- `test-jalr-simple.lisp` → `tests/debug/`
- `test-jalr-verbose.lisp` → `tests/debug/`
- `test-results.log` → `test-results/`
- `test-summary.txt` → `test-results/`

#### Documentation Mise à Jour
- `README.md` : Version simplifiée et à jour avec Phase 9
- `docs/PHASE9_PROGRESS.md` : Documentation complète de la Phase 9
- Ajout section détaillée sur le bug LW et sa résolution

### 📈 Statistiques Phase 9

- **Temps de développement** : ~20h
- **Lignes de code ajoutées** : ~800 lignes
- **Bugs critiques résolus** : 1 (format LW)
- **Tests créés** : 26
- **Taux de réussite** : 100%

### 🎯 Prochaines Étapes

#### Phase 10 : BOOTSTRAP (En attente)
- Auto-compilation du compilateur
- VM exécutant le compilateur compilé
- Durée estimée : 15-20h

### 🔗 Références

- [PHASE9_PROGRESS.md](docs/PHASE9_PROGRESS.md) - Progression détaillée
- [CLOSURES_DESIGN.md](docs/CLOSURES_DESIGN.md) - Design des closures
- [README.md](README.md) - Documentation principale

---

## Historique Antérieur

### Phase 8 : LABELS et Récursivité (Complétée)
### Phase 7 : DOTIMES (Complétée)
### Phase 6 : Fonctions Mathématiques (Complétée)
### Phase 5 : Structures de Contrôle (Complétée)
### Phases 1-4 : VM et Compilateur de Base (Complétées)

---

**Dernière mise à jour** : 26 novembre 2025
