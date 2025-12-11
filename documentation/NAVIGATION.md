# Guide de Navigation Rapide

**Dernière mise à jour**: 27 novembre 2025

## 🚀 Démarrage Rapide

### Je découvre le projet
1. Lire [README.md](README.md) - Vue d'ensemble
2. Consulter [STRUCTURE_PROJET.md](STRUCTURE_PROJET.md) - Structure détaillée

### Je veux travailler sur Phase 11
1. [docs/phases/phase11/PLAN_ACTION_VM1.txt](docs/phases/phase11/PLAN_ACTION_VM1.txt) - Plan d'action (8 phases, 25-32h)
2. [docs/phases/phase11/ARCHITECTURE_VM0_VM1.txt](docs/phases/phase11/ARCHITECTURE_VM0_VM1.txt) - Architecture clarifiée
3. [docs/phases/phase11/CHECKLIST_PHASE11.txt](docs/phases/phase11/CHECKLIST_PHASE11.txt) - Checklist détaillée

### Je cherche un document
- [docs/INDEX.md](docs/INDEX.md) - Index complet de tous les documents

### Je veux comprendre Phase 10
- [docs/phases/phase10/RAPPORT_FINAL_PHASE10.md](docs/phases/phase10/RAPPORT_FINAL_PHASE10.md) - Rapport complet
- [docs/phases/phase10/SUCCES_TOTAL_100%.md](docs/phases/phase10/SUCCES_TOTAL_100%.md) - Validation 7/7 tests

---

## 📁 Structure Principale

```
VirtualMachine_CLISP/
├── src/                    # Code source (vm.lisp, compiler.lisp, etc.)
├── tests/                  # Tests organisés par type
├── docs/                   # Documentation complète
│   ├── phases/            # Documentation par phase
│   │   ├── phase10/      # Phase 10 Bootstrap (✅ 100%)
│   │   └── phase11/      # Phase 11 VM Compilation (🚀 en cours)
│   └── INDEX.md          # Index de tous les documents
├── scripts/               # Scripts de build et test
├── examples/              # Exemples de code
├── bootstrap/             # Code bootstrap Phase 10 (historique)
├── logs/                  # Fichiers de log
└── archive/               # Fichiers obsolètes
```

---

## 🎯 État Actuel

**Phase 10**: ✅ Bootstrap 100% complet (7/7 tests)  
**Phase 11**: ⏸️ Prêt à démarrer  
**Mémoire**: 4 Mo (1,048,576 mots)  
**Structure**: ✅ Réorganisée (27 nov 2025)

**Prochaine étape**: Phase 11.1 - Analyse de src/vm.lisp

---

## 📖 Documents Essentiels

| Document | Description |
|----------|-------------|
| [README.md](README.md) | Documentation principale |
| [STRUCTURE_PROJET.md](STRUCTURE_PROJET.md) | Structure détaillée (400+ lignes) |
| [docs/INDEX.md](docs/INDEX.md) | Index de tous les documents |
| [docs/CHANGELOG.md](docs/CHANGELOG.md) | Historique des changements |

---

## 🔧 Commandes Utiles

### Lancer tous les tests
```bash
./scripts/run-all-tests.sh
```

### Tests unitaires uniquement
```bash
./scripts/run-unit-tests.sh
```

### Charger le projet en CLISP
```bash
clisp main.lisp
```

### Charger le projet en SBCL
```bash
sbcl --load main.lisp
```

---

## 📝 Notes Importantes

1. **Code actif**: `src/` contient le code principal
2. **Bootstrap**: `bootstrap/src/` est historique (Phase 10, non utilisé pour Phase 11)
3. **Documentation**: Tout dans `docs/`, organisée par phase
4. **Tests**: Organisés par type dans `tests/`
5. **Scripts**: Centralisés dans `scripts/`

---

**Pour plus de détails, voir [STRUCTURE_PROJET.md](STRUCTURE_PROJET.md)**
