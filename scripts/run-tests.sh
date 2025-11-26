#!/bin/bash
###############################################################################
# run-tests.sh - Script pour exécuter tous les tests du projet
# Usage: ./scripts/run-tests.sh [category]
#   category: all (défaut), unit, debug, specific
###############################################################################

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}╔════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║       Tests Compilateur LISP → MIPS               ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════╝${NC}"
echo ""

CATEGORY=${1:-all}

run_test() {
    local test_file=$1
    local test_name=$(basename "$test_file" .lisp)
    
    echo -e "${YELLOW}➤ Test: $test_name${NC}"
    
    if timeout 10 clisp "$test_file" 2>&1 | tail -5; then
        echo -e "${GREEN}  ✓ OK${NC}"
        return 0
    else
        echo -e "${RED}  ✗ ÉCHEC${NC}"
        return 1
    fi
}

# Tests unitaires
if [ "$CATEGORY" = "all" ] || [ "$CATEGORY" = "unit" ]; then
    echo -e "${BLUE}═══ Tests Unitaires ═══${NC}"
    echo ""
    
    TESTS=(
        "tests/unit/test-cond.lisp"
        "tests/unit/test-when-unless.lisp"
        "tests/unit/test-logical.lisp"
        "tests/unit/test-case.lisp"
        "tests/unit/test-dotimes.lisp"
        "tests/unit/test-math.lisp"
        "tests/unit/test-let.lisp"
        "tests/unit/test-loop.lisp"
        "tests/unit/test-labels.lisp"
    )
    
    PASSED=0
    FAILED=0
    
    for test in "${TESTS[@]}"; do
        if [ -f "$test" ]; then
            if run_test "$test"; then
                ((PASSED++))
            else
                ((FAILED++))
            fi
            echo ""
        fi
    done
    
    echo -e "${BLUE}═══════════════════════${NC}"
    echo -e "Tests réussis: ${GREEN}$PASSED${NC}"
    echo -e "Tests échoués: ${RED}$FAILED${NC}"
    echo ""
fi

# Tests de debug
if [ "$CATEGORY" = "all" ] || [ "$CATEGORY" = "debug" ]; then
    echo -e "${BLUE}═══ Tests de Debug ═══${NC}"
    echo ""
    
    DEBUG_TESTS=(
        "tests/debug/test-dotimes6-debug.lisp"
        "tests/debug/test-closure5-debug.lisp"
    )
    
    for test in "${DEBUG_TESTS[@]}"; do
        if [ -f "$test" ]; then
            echo -e "${YELLOW}➤ Debug: $(basename "$test")${NC}"
            timeout 5 clisp "$test" 2>&1 | tail -10
            echo ""
        fi
    done
fi

echo -e "${GREEN}✓ Tests terminés${NC}"
