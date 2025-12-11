# Structure du Projet - Virtual Machine CLISP

## Vue d'ensemble
Projet de machine virtuelle MIPS avec compilateur LISP → MIPS.

**Date de réorganisation**: 27 novembre 2025  
**Phase actuelle**: Phase 11 - Compilation de la VM en MIPS  
**Phase 10**: Bootstrap 100% complet (7/7 tests réussis)

---

## Arborescence principale

```
VirtualMachine_CLISP/
├── main.lisp                   # Point d'entrée principal
├── README.md                   # Documentation principale
├── STRUCTURE_PROJET.md         # Ce fichier
│
├── src/                        # Code source principal
│   ├── vm.lisp                 # Machine virtuelle (686 lignes)
│   ├── compiler.lisp           # Compilateur LISP → MIPS (1886 lignes)
│   ├── loader.lisp             # Chargeur de code MIPS
│   ├── asm-ops.lisp            # Définitions des instructions MIPS
│   ├── utils.lisp              # Utilitaires généraux
│   └── bootstrap/              # Code bootstrap Phase 10 (historique)
│       ├── vm-bootstrap.lisp
│       ├── compiler-bootstrap.lisp
│       ├── loader-bootstrap.lisp
│       └── primitives.lisp
│
├── tests/                      # Tests organisés par type
│   ├── unit/                   # Tests unitaires
│   ├── integration/            # Tests d'intégration
│   ├── validation/             # Tests de validation finale
│   │   └── test-final-validation.lisp
│   ├── performance/            # Tests de performance
│   │   ├── test-performance-real.lisp
│   │   └── results/            # Résultats des benchmarks
│   └── debug/                  # Tests de débogage
│
├── scripts/                    # Scripts de build et test
│   ├── run-all-tests.sh        # Tous les tests
│   ├── run-unit-tests.sh       # Tests unitaires
│   ├── run-tests.sh            # Tests généraux
│   ├── validate-phase8.sh      # Validation phase 8
│   └── phase10/                # Scripts Phase 10
│       └── START_PHASE10.sh
│
├── docs/                       # Documentation du projet
│   ├── CHANGELOG.md            # Historique des changements
│   ├── history/                # Documents historiques datés
│   │   ├── CHANGEMENTS_27NOV2025.txt
│   │   └── ETAT_PROJET_27NOV2025.txt
│   ├── phases/                 # Documentation par phase
│   │   ├── phase10/            # Phase 10 Bootstrap
│   │   │   ├── RAPPORT_FINAL_PHASE10.md
│   │   │   ├── SUCCES_TOTAL_100%.md
│   │   │   └── ... (13 documents)
│   │   └── phase11/            # Phase 11 VM Compilation
│   │       ├── PLAN_ACTION_VM1.txt
│   │       ├── ARCHITECTURE_VM0_VM1.txt
│   │       └── CHECKLIST_PHASE11.txt
│   ├── FichierTexteSuivi/      # Suivi détaillé
│   └── Ressource_externe/      # Ressources externes
│
├── bootstrap/                  # Code bootstrap historique
│   └── src/                    # Sources bootstrap Phase 10
│
├── examples/                   # Exemples de programmes
│   └── ... (fichiers LISP d'exemple)
│
├── archive/                    # Fichiers obsolètes archivés
│   ├── PLAN_ACTION_COMPLET.md
│   ├── REORGANISATION.md
│   ├── RESUME_PLAN.txt
│   └── README.md.backup
│
├── logs/                       # Fichiers de log
│   └── baseline-phase10.log
│
├── output/                     # Sorties temporaires
├── test-results/               # Résultats de tests
└── .git/                       # Contrôle de version Git
```

---

## Description des répertoires

### `src/` - Code source principal
**Rôle**: Contient tous les fichiers sources actifs du projet.

- **vm.lisp**: Machine virtuelle MIPS complète (686 lignes)
  - Exécution d'instructions MIPS
  - Gestion mémoire (4 Mo = 1,048,576 mots)
  - Registres, pile, tas
  
- **compiler.lisp**: Compilateur LISP vers MIPS (1886 lignes)
  - Support: if, cond, let, labels, progn, quote, setq
  - Closures lexicales (Phase 9)
  - Labels dynamiques (Phase 8)
  
- **loader.lisp**: Chargeur de code
  - Charge le code MIPS en mémoire
  - Ajoute automatiquement HALT
  - Preprocessing des labels
  
- **asm-ops.lisp**: Définitions MIPS
  - 55 instructions MIPS supportées
  - *maxmem* = 1,048,576 (4 Mo)
  
- **bootstrap/**: Code Phase 10 (historique)
  - Versions sans messages de debug
  - Référence uniquement, non utilisé en Phase 11

### `tests/` - Suite de tests
**Organisation par type de test**:

- **unit/**: Tests unitaires des fonctions individuelles
- **integration/**: Tests d'intégration de bout en bout
- **validation/**: Tests de validation finale du projet
- **performance/**: Benchmarks et comparaisons de performance
  - `results/`: Résultats sauvegardés
- **debug/**: Tests pour le débogage

### `scripts/` - Scripts d'automatisation
**Scripts de build, test et validation**:

- `run-all-tests.sh`: Lance tous les tests
- `run-unit-tests.sh`: Tests unitaires uniquement
- `phase10/START_PHASE10.sh`: Script de démarrage Phase 10

### `docs/` - Documentation
**Documentation complète du projet**:

- **CHANGELOG.md**: Historique des changements
- **history/**: Documents datés (états du projet)
- **phases/**: Documentation organisée par phase
  - **phase10/**: Bootstrap complet (13 documents)
  - **phase11/**: Compilation VM (plan d'action, architecture)
- **FichierTexteSuivi/**: Suivi détaillé du projet
- **Ressource_externe/**: Spécifications et références

### `bootstrap/` - Code historique
**Code bootstrap Phase 10 (référence uniquement)**:
- Sources originales sans debug
- Non modifié pour Phase 11

### `examples/` - Exemples
**Programmes d'exemple LISP**:
- Démonstrations des fonctionnalités
- Tests manuels

### `archive/` - Archives
**Fichiers obsolètes conservés**:
- Plans d'action anciens
- README backup
- Documents remplacés

### `logs/` - Logs
**Fichiers de log du projet**:
- Logs de tests
- Traces d'exécution

---

## Configuration mémoire

**Mémoire VM**: 4 Mo (1,048,576 mots de 4 octets)
- Upgrade effectué pour Phase 11 (VM-on-VM)
- Permet d'exécuter VM₁ (compilée) dans VM₀ (native)

**Fichier**: `src/asm-ops.lisp` ligne 8
```lisp
(defparameter *maxmem* 1048576 ...) ; 4 Mo
```

---

## Phases du projet

### ✅ Phase 10: Bootstrap (100% complet)
**Objectif**: Compiler la VM avec elle-même
- vm-bootstrap.lisp: 643 lignes (sans debug)
- 7/7 tests passés
- Documentation complète dans `docs/phases/phase10/`

### 🔄 Phase 11: Compilation VM (en cours)
**Objectif**: Compiler la vraie VM (vm.lisp) en MIPS
- **Durée estimée**: 25-32 heures
- **8 sous-phases**:
  1. Analyse de vm.lisp
  2. LOOP/WHILE (3-4h)
  3. ARRAYS (2-3h)
  4. CASE (2-3h)
  5. Simplification VM (3-4h)
  6. Compilation MIPS (4-5h)
  7. Tests (3-4h)
  8. Documentation (2h)

**Plan détaillé**: `docs/phases/phase11/PLAN_ACTION_VM1.txt`

---

## Architecture VM₀ / VM₁

**VM₀ (Native)**:
- src/vm.lisp exécuté en Common LISP natif
- Plateforme hôte

**VM₁ (Compiled)**:
- src/vm.lisp compilé en MIPS
- Chargé et exécuté dans VM₀

**Même VM, deux implémentations !**

Voir: `docs/phases/phase11/ARCHITECTURE_VM0_VM1.txt`

---

## Instructions MIPS supportées

**55 instructions** définies dans `src/asm-ops.lisp`:

### Arithmétique
ADD, ADDI, SUB, SUBI, MUL, MULI, DIV, DIVI

### Logique
AND, ANDI, OR, ORI, XOR, XORI, NOT, SHL, SHR

### Comparaison
CMP, CMPI

### Contrôle
JMP, JEQ, JNE, JLT, JLE, JGT, JGE, CALL, RET

### Mémoire
LOAD, LOADI, STORE, STOREI, PUSH, PUSHI, POP

### Système
HALT, NOP, SYSCALL, DBG, LABEL

### Spéciales
CLOSURE, CLOSURE-REF, ENV-NEW, ENV-GET, ENV-SET

---

## Commandes principales

### Lancer tous les tests
```bash
./scripts/run-all-tests.sh
```

### Tests unitaires
```bash
./scripts/run-unit-tests.sh
```

### Charger le projet en SBCL
```bash
sbcl --load main.lisp
```

### Compiler et exécuter un programme
```lisp
(load "main.lisp")
(let* ((code (compile-expr '(+ 1 2)))
       (asm-code (preprocess-labels code)))
  (load-code asm-code)
  (run-vm *maxcycles*))
```

---

## État actuel

**Phase 10**: ✅ 100% complet (7/7 tests)  
**Phase 11**: ⏸️ Prêt à démarrer  
**Mémoire**: ✅ 4 Mo configuré  
**Structure**: ✅ Réorganisée (27 nov 2025)  

**Prochaine étape**: Phase 11.1 - Analyse de vm.lisp

---

## Notes importantes

1. **Bootstrap vs Production**:
   - `bootstrap/src/`: Code Phase 10 (historique)
   - `src/`: Code production actuel

2. **HALT automatique**:
   - Tous les programmes compilés reçoivent automatiquement HALT
   - Évite les erreurs "Instruction nulle"

3. **Git**:
   - Projet sous contrôle de version Git
   - `.git/` contient l'historique complet

4. **Documentation**:
   - Toute la documentation dans `docs/`
   - Organisée par phase dans `docs/phases/`

---

**Dernière mise à jour**: 27 novembre 2025  
**Maintainer**: Projet VirtualMachine CLISP
