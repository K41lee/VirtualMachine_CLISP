# Structure Organisée du Projet

Date de réorganisation : 11 décembre 2025

## 📂 Nouvelle Organisation

### Dossiers Principaux

```
VirtualMachine_CLISP/
├── src/                    # Code source de la VM et du compilateur
├── benchmarks/             # Tous les benchmarks
├── tests/                  # Tous les tests (integration/unit/debug)
├── tools/                  # Outils de développement
├── documentation/          # Documentation complète (.md)
├── output/                 # Fichiers générés (vm-executable.mips)
├── docs/                   # Documentation technique
├── examples/               # Exemples de code
├── scripts/                # Scripts utilitaires
├── logs/                   # Logs d'exécution
├── archive/                # Anciens fichiers
└── bootstrap/              # Ancien code bootstrap
```

## 📁 Détail des Dossiers

### `src/` - Code Source Principal
- `vm.lisp` - Machine virtuelle MIPS principale
- `vm-compilable.lisp` - Version compilable de la VM
- `compiler.lisp` - Compilateur LISP → MIPS
- `asm-ops.lisp` - Opérations et registres MIPS
- `loader.lisp` - Chargeur de code MIPS
- `utils.lisp` - Fonctions utilitaires
- `bootstrap/` - Code pour VM1 bootstrap

### `benchmarks/` - Système de Benchmarks
- `run-benchmark.lisp` - ⭐ Benchmark principal (3 scénarios)
- `benchmark-multi-level.lisp` - Benchmark multi-niveaux
- `benchmark-performance.lisp` - Mesures de performance
- `benchmark-simple.lisp` - Benchmark simple
- `demo-benchmark.lisp` - Démonstration

### `tests/` - Tests Complets

#### `tests/integration/` - Tests d'Intégration
- `test-bootstrap-mod.lisp` - ⭐ Test bootstrap avec fibo(20)
- `test-vm1-bootstrap.lisp` - Test VM1
- `test-fibo-recursive.lisp` - Tests Fibonacci

#### `tests/unit/` - Tests Unitaires
- `test-compiler-vm0.lisp` - Test du compilateur
- `test-vm-compilable.lisp` - Test de la VM compilable
- `test-compilation-rate.lisp` - Taux de compilation
- `test-real-compilation.lisp` - Compilation réelle
- `test-source-compilation.lisp` - Compilation source
- `test-vm-executable.lisp` - Test de VM1
- Et autres tests unitaires...

#### `tests/debug/` - Tests de Debug
- `test-debug-deep.lisp` - Debug profond
- `test-let-debug.lisp` - Debug LET
- `test-backtrace.lisp` - Backtrace

### `tools/` - Outils de Développement
- `generate-vm-executable.lisp` - ⭐ Génère VM1 (MIPS)
- `compile-vm-simple.lisp` - Compilation simplifiée

### `documentation/` - Documentation Markdown
- `README.md` - Documentation complète
- `TODO-VRAI-BOOTSTRAP.md` - ⭐ Guide du bootstrap complet
- `BENCHMARK-README.md` - Guide des benchmarks
- `STRUCTURE_PROJET.md` - Structure technique
- `CHANGELOG_PHASE11.md` - Historique des changements
- `BENCHMARK-FINAL.md` - Résultats finaux
- `NAVIGATION.md` - Guide de navigation
- `README_VM1.md` - Documentation VM1
- `rapport-session4.lisp` - Rapport de session

### `output/` - Fichiers Générés
- `vm-executable.mips` - ⭐ VM1 compilée (1605 instructions, 27 fonctions)

## 🚀 Commandes Principales

### Tests d'Intégration
```bash
# Test bootstrap complet (fibo(20))
clisp tests/integration/test-bootstrap-mod.lisp

# Test VM1
clisp tests/integration/test-vm1-bootstrap.lisp
```

### Benchmarks
```bash
# Benchmark interactif
clisp
> (load "benchmarks/run-benchmark.lisp")
> (benchmark-code '(+ 10 20 30))

# Benchmark multi-level
clisp benchmarks/benchmark-multi-level.lisp
```

### Génération de VM1
```bash
# Compiler src/vm-compilable.lisp → output/vm-executable.mips
clisp tools/generate-vm-executable.lisp
```

### Tests Unitaires
```bash
# Test du compilateur
clisp tests/unit/test-compiler-vm0.lisp

# Test de la VM compilable
clisp tests/unit/test-vm-compilable.lisp
```

## 📊 Statistiques

### Code Source (`src/`)
- 9 fichiers principaux
- ~5000 lignes de code
- 27 fonctions VM1 compilées

### Tests (`tests/`)
- 3 tests d'intégration
- 15+ tests unitaires
- 5+ tests de debug

### Benchmarks (`benchmarks/`)
- 5 fichiers de benchmark
- 3 scénarios (natif/VM0/VM1→VM2)

### Documentation (`documentation/`)
- 8 fichiers Markdown
- 1 rapport de session

## 🎯 Fichiers Clés

### Essentiels
1. ⭐ `src/vm.lisp` - Machine virtuelle principale
2. ⭐ `src/compiler.lisp` - Compilateur LISP → MIPS
3. ⭐ `benchmarks/run-benchmark.lisp` - Benchmark principal
4. ⭐ `tests/integration/test-bootstrap-mod.lisp` - Test bootstrap
5. ⭐ `tools/generate-vm-executable.lisp` - Générateur VM1
6. ⭐ `documentation/TODO-VRAI-BOOTSTRAP.md` - Guide bootstrap

### Configuration
- `main.lisp` - Point d'entrée principal
- `output/vm-executable.mips` - VM1 compilée
- `README.md` - Documentation principale

## 🔄 Workflow de Développement

1. **Modifier la VM** : Éditer `src/vm-compilable.lisp`
2. **Régénérer VM1** : `clisp tools/generate-vm-executable.lisp`
3. **Tester** : `clisp tests/integration/test-bootstrap-mod.lisp`
4. **Benchmarker** : `clisp benchmarks/run-benchmark.lisp`

## ✅ Avantages de la Nouvelle Structure

- ✅ **Lisibilité** : Chaque type de fichier dans son dossier
- ✅ **Navigation** : Structure logique et intuitive
- ✅ **Tests** : Séparation intégration/unit/debug
- ✅ **Documentation** : Centralisée dans `documentation/`
- ✅ **Maintenance** : Facile de trouver et modifier les fichiers
- ✅ **Compatibilité** : Tous les chemins mis à jour

## 📝 Notes

- Tous les chemins ont été mis à jour dans les fichiers
- `tests/integration/test-bootstrap-mod.lisp` charge maintenant `benchmarks/run-benchmark.lisp`
- La racine ne contient plus que `README.md` et `main.lisp`
- Documentation complète dans `documentation/`

## 🎓 Pour Contribuer

1. Code source → `src/`
2. Tests → `tests/` (choisir integration/unit/debug)
3. Benchmarks → `benchmarks/`
4. Documentation → `documentation/`
5. Outils → `tools/`
