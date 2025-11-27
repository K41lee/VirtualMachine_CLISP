#!/bin/bash

echo "╔════════════════════════════════════════╗"
echo "║  Suite de Tests - Compilateur LISP    ║"
echo "╚════════════════════════════════════════╝"
echo ""

TOTAL=0
PASSED=0
FAILED=0

# Fonction pour exécuter un fichier de test
run_test() {
    local test_file=$1
    local test_name=$(basename "$test_file" .lisp)
    
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "📝 Test: $test_name"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    # Exécuter le test et capturer le résultat
    output=$(clisp -q -x "(load \"main.lisp\") (load \"$test_file\")" 2>&1)
    
    # Compter les tests réussis et échoués
    passed=$(echo "$output" | grep -c "✓" || true)
    failed=$(echo "$output" | grep -c "✗" || true)
    
    echo "$output" | tail -30
    
    TOTAL=$((TOTAL + passed + failed))
    PASSED=$((PASSED + passed))
    FAILED=$((FAILED + failed))
    
    echo ""
}

# Tests unitaires
echo "══════════════════════════════════════"
echo "  TESTS UNITAIRES"
echo "══════════════════════════════════════"
echo ""

for test in tests/unit/test-*.lisp; do
    if [ -f "$test" ]; then
        run_test "$test"
    fi
done

# Résumé final
echo "╔════════════════════════════════════════╗"
echo "║         RÉSUMÉ DES TESTS               ║"
echo "╚════════════════════════════════════════╝"
echo ""
echo "Total de tests : $TOTAL"
echo "Tests réussis  : $PASSED ✓"
echo "Tests échoués  : $FAILED ✗"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "🎉 Tous les tests sont passés avec succès!"
    exit 0
else
    echo "⚠️  Certains tests ont échoué."
    exit 1
fi
