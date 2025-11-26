#!/bin/bash

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║     Exécution de Tous les Tests Unitaires                ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
TOTAL_FILES=0
PASSED_FILES=0

# Couleurs
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Fonction pour exécuter un test
run_test_file() {
    local test_file=$1
    local test_name=$(basename "$test_file" .lisp)
    
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "📝 Test: $test_name"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    # Créer un wrapper temporaire qui charge main.lisp puis le test
    # mais supprime les lignes (load ...) du test
    temp_test="/tmp/test_wrapper_$$.lisp"
    
    cat > "$temp_test" << 'WRAPPER'
(load "main.lisp")
WRAPPER
    
    # Ajouter le contenu du test en filtrant les (load ...)
    grep -v '(load "' "$test_file" >> "$temp_test"
    
    # Exécuter le test avec timeout
    output=$(timeout 30 clisp -q "$temp_test" 2>&1)
    exit_code=$?
    
    # Compter les résultats
    if [ $exit_code -eq 124 ]; then
        echo -e "${RED}⏱️  TIMEOUT${NC}"
        echo ""
        rm -f "$temp_test"
        return 1
    fi
    
    passed=$(echo "$output" | grep -o "✓" | wc -l)
    failed=$(echo "$output" | grep -o "✗" | wc -l)
    
    # Afficher les dernières lignes significatives
    echo "$output" | grep -E "(Test|✓|✗|TOTAL|Passed|Failed)" | tail -20
    
    TOTAL_TESTS=$((TOTAL_TESTS + passed + failed))
    PASSED_TESTS=$((PASSED_TESTS + passed))
    FAILED_TESTS=$((FAILED_TESTS + failed))
    TOTAL_FILES=$((TOTAL_FILES + 1))
    
    if [ $failed -eq 0 ] && [ $passed -gt 0 ]; then
        PASSED_FILES=$((PASSED_FILES + 1))
        echo -e "${GREEN}✅ Fichier: $passed tests réussis${NC}"
    else
        echo -e "${RED}❌ Fichier: $failed tests échoués${NC}"
    fi
    
    echo ""
    rm -f "$temp_test"
}

# Exécuter tous les tests
for test_file in tests/unit/test-*.lisp; do
    if [ -f "$test_file" ]; then
        run_test_file "$test_file"
    fi
done

# Résumé final
echo "╔═══════════════════════════════════════════════════════════╗"
echo "║                  RÉSUMÉ FINAL                             ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""
echo "Fichiers testés  : $TOTAL_FILES"
echo "Fichiers réussis : $PASSED_FILES"
echo ""
echo "Tests totaux     : $TOTAL_TESTS"
echo -e "Tests réussis    : ${GREEN}$PASSED_TESTS ✓${NC}"
echo -e "Tests échoués    : ${RED}$FAILED_TESTS ✗${NC}"
echo ""

if [ $TOTAL_TESTS -gt 0 ]; then
    percentage=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    echo "Taux de réussite : $percentage%"
    echo ""
fi

if [ $FAILED_TESTS -eq 0 ] && [ $TOTAL_TESTS -gt 0 ]; then
    echo -e "${GREEN}🎉 Tous les tests sont passés avec succès!${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠️  Certains tests ont échoué ou aucun test n'a été exécuté.${NC}"
    exit 1
fi
