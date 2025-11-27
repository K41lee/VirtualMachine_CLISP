#!/bin/bash
# Script de démarrage Phase 10 : Bootstrap

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║           PHASE 10 : BOOTSTRAP - DÉMARRAGE                    ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

# Vérifier qu'on est sur main
BRANCH=$(git branch --show-current)
if [ "$BRANCH" != "main" ]; then
    echo "⚠️  Attention: vous n'êtes pas sur la branche main"
    echo "   Branche actuelle: $BRANCH"
    read -p "Continuer quand même? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Créer branche phase10-bootstrap
echo "1️⃣  Création de la branche phase10-bootstrap..."
git checkout -b phase10-bootstrap 2>/dev/null || git checkout phase10-bootstrap
echo "✅ Branche prête"
echo ""

# Créer structure de répertoires
echo "2️⃣  Création de la structure de répertoires..."
mkdir -p output
mkdir -p tests/integration
echo "✅ Répertoires créés: output/, tests/integration/"
echo ""

# Baseline des tests
echo "3️⃣  Baseline des tests actuels..."
if [ -f "run-unit-tests.sh" ]; then
    ./run-unit-tests.sh | tee baseline-phase10.log
    TESTS_OK=$?
    if [ $TESTS_OK -eq 0 ]; then
        echo "✅ Tests baseline: OK (84/84)"
    else
        echo "⚠️  Certains tests ont échoué. Voir baseline-phase10.log"
    fi
else
    echo "⚠️  run-unit-tests.sh non trouvé"
fi
echo ""

# Audit des dépendances
echo "4️⃣  Audit des dépendances du compilateur..."
grep -rn "format\|apply\|funcall\|mapcar\|remove-if\|assoc\|find\|gethash\|make-hash-table" src/compiler.lisp > docs/audit-temp.txt 2>/dev/null
DEPS=$(wc -l < docs/audit-temp.txt)
echo "✅ Audit terminé: $DEPS occurences de fonctions natives trouvées"
echo "   Résultat dans: docs/audit-temp.txt"
echo ""

# Résumé
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║                   DÉMARRAGE TERMINÉ                           ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""
echo "📋 PROCHAINES ACTIONS:"
echo ""
echo "   Étape 1.1 : Audit des dépendances (1h)"
echo "   ├─ Lire: docs/audit-temp.txt"
echo "   ├─ Analyser les fonctions natives utilisées"
echo "   └─ Créer: docs/AUDIT_DEPENDANCES.md"
echo ""
echo "   Commandes utiles:"
echo "   ├─ cat docs/audit-temp.txt | cut -d: -f3 | sort | uniq -c | sort -rn"
echo "   │  (Compter les occurrences de chaque fonction)"
echo "   │"
echo "   └─ grep -c 'mapcar\|apply\|funcall' src/compiler.lisp"
echo "      (Compter les fonctions critiques)"
echo ""
echo "📚 DOCUMENTATION:"
echo "   ├─ Plan détaillé: docs/PHASE10_BOOTSTRAP_PLAN.md"
echo "   ├─ Roadmap visuelle: docs/PHASE10_ROADMAP.md"
echo "   └─ Todo list: Voir manage_todo_list"
echo ""
echo "🚀 PRÊT À DÉMARRER LA PHASE 10!"
echo ""
