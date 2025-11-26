# 🚀 Compilateur LISP → MIPS + Machine Virtuelle

Un système complet de compilation et d'exécution développé en Common LISP, comprenant un compilateur LISP vers assembleur MIPS et une machine virtuelle MIPS.

## 🎯 Objectif du Projet

Développer un système permettant de :
1. **Compiler** du code LISP en assembleur MIPS
2. **Exécuter** le code MIPS sur une machine virtuelle
3. **Supporter** les closures et la récursivité
4. **Comparer** les performances avec LISP natif

**Statut** : ✅ **PHASE 9 COMPLÉTÉE** - Closures fonctionnelles avec 100% des tests passants

## 📦 Structure du Projet

```
VirtualMachine_CLISP/
├── src/                          # Code source
│   ├── compiler.lisp             # Compilateur LISP → MIPS
│   ├── vm.lisp                   # Machine virtuelle MIPS
│   ├── loader.lisp               # Chargeur avec résolution labels
│   ├── asm-ops.lisp              # Opcodes et registres MIPS
│   └── utils.lisp                # Fonctions utilitaires
│
├── tests/                        # Tests
│   ├── unit/                     # Tests unitaires (84 tests)
│   └── debug/                    # Tests de débogage
│
├── docs/                         # Documentation
│   ├── PHASE9_PROGRESS.md        # Progression Phase 9 détaillée
│   ├── CLOSURES_DESIGN.md        # Design des closures
│   └── ...
│
├── main.lisp                     # Point d'entrée principal
└── README.md                     # Ce fichier
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

- [PHASE9_PROGRESS.md](docs/PHASE9_PROGRESS.md) - Progression Phase 9
- [CLOSURES_DESIGN.md](docs/CLOSURES_DESIGN.md) - Design closures

## 👥 Contribution

**Auteur** : K41lee  
**Date** : Novembre 2025  
**Statut** : ✅ Phase 9 Complétée

---

**Dernière mise à jour** : 26 novembre 2025
