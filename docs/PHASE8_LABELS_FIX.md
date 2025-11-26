# Phase 8: Correction Bug LABELS (Static Link)

**Date:** 26 novembre 2025  
**Statut:** ✅ COMPLÉTÉE  
**Durée:** ~2 heures  
**Tests:** 70/70 (100%)

---

## 🎯 Objectif

Corriger le bug dans la gestion des static links pour les fonctions locales LABELS, spécifiquement quand des fonctions siblings (même niveau lexical) s'appellent entre elles.

## ❌ Problème Initial

**Test cas problématique:**
```lisp
(labels ((outer (x)
          (labels ((mult (n) (* x n))
                   (twice (n) (mult (mult n))))
            (twice 3))))
  (outer 2))
```

**Résultat attendu:** `12`  
- `outer(2)` : x=2
- `twice(3)` appelle `mult(mult(3))`
- `mult(3)` = 2*3 = 6
- `mult(6)` = 2*6 = 12 ✓

**Résultat obtenu:** Crash (erreur mémoire) ou valeur incorrecte (27)

### Analyse du Bug

Le problème se situait à **deux niveaux**:

1. **Dans `compile-call`**: Tous les appels de fonctions locales passaient `$S0` comme static link, sans distinction entre:
   - **Siblings** (fonctions au même niveau lexical) → doivent recevoir `$S0` (static link du parent commun)
   - **Enfants** (fonctions dans un LABELS imbriqué) → doivent recevoir `$FP` (frame pointer actuel)

2. **Dans `compile-labels`**: Le corps d'un LABELS n'initialisait pas `$S0` correctement pour les appels aux fonctions locales définies dans ce LABELS.

### Diagnostic Détaillé

Chaîne d'appels problématique:
```
LABELS_BODY_0 (global)
  └→ outer(2)  [$FP_outer créé, x=2 sauvegardé]
      └→ LABELS_BODY_2 (corps de outer)
          └→ twice(3)  [reçoit $S0 = 0 ou invalide ❌]
              └→ mult(3)  [reçoit $S0 = 0 ou invalide ❌]
                  └→ Accès à x via static link: CRASH ❌
```

Le problème : `LABELS_BODY_2` n'initialisait pas `$S0` avec `$FP_outer`, donc les fonctions `mult` et `twice` recevaient un static link invalide.

---

## ✅ Solution Implémentée

### 1. Modification de `compile-call`

**Fichier:** `src/compiler.lisp` (ligne 1274)

**Changements:**
- Ajout du calcul de la relation entre fonction appelante et appelée (sibling vs enfant)
- Distinction du static link à passer selon la relation

**Code ajouté:**
```lisp
(defun compile-call (func-name args env)
  (let* ((code '())
    (arg-regs (list *reg-a0* *reg-a1* *reg-a2* *reg-a3*))
    (fn-info (lookup-function-def-info env func-name))
    (target-label (if fn-info (car fn-info) func-name))
    (is-local-fn fn-info)
    ;; PHASE 8 FIX: Déterminer la relation lexicale
    (fn-depth (if fn-info (cdr fn-info) nil))
    (current-depth (compiler-env-lexical-depth env))
    (is-sibling (and fn-depth (= fn-depth current-depth))))
    
    ;; ...
    
    ;; PHASE 8 FIX: Passer le bon static link
    (when is-local-fn
      (if is-sibling
          ;; Sibling: passer $S0 tel quel (static link du parent commun)
          (setf code (append code (list (list :MOVE *reg-s0* *reg-t3*))))
          ;; Enfant: passer $FP (notre frame devient leur static link)
          (setf code (append code (list (list :MOVE (get-reg :fp) *reg-t3*))))))
    
    ;; ...
```

**Logique:**
- **Si sibling** (même profondeur lexicale): `$T3 = $S0` (partager le static link du parent)
- **Si enfant** (profondeur différente): `$T3 = $FP` (passer notre frame)

### 2. Modification de `compile-labels`

**Fichier:** `src/compiler.lisp` (ligne 1189)

**Changements:**
- Initialisation de `$S0` au début du corps du LABELS
- `$S0` reçoit `$FP` du scope englobant pour être passé aux fonctions locales

**Code ajouté:**
```lisp
;; ÉTAPE 4: Label du corps principal et compilation
(setf code (append code (list (list :LABEL body-label))))

;; PHASE 8 FIX: Le corps d'un LABELS doit initialiser $S0 correctement
;; Si on est dans une fonction (parent-lexical non-nil), les fonctions locales
;; doivent recevoir $FP du scope actuel comme static link.
(when (compiler-env-parent-lexical new-env)
  ;; Initialiser $S0 = $FP du parent
  (setf code (append code (list (list :MOVE (get-reg :fp) *reg-s0*)))))

;; Compiler le corps principal
(dolist (expr body)
  (setf code (append code (compile-expr expr new-env))))
```

**Logique:**
- Le corps d'un LABELS n'a pas son propre frame
- Il s'exécute dans le contexte de la fonction englobante
- `$S0 = $FP` permet aux fonctions locales de recevoir le bon static link

---

## 📊 Résultats

### Avant la Correction

```lisp
(compile-and-run '(labels ((outer (x) 
                            (labels ((mult (n) (* x n)) 
                                     (twice (n) (mult (mult n)))) 
                              (twice 3)))) 
                    (outer 2)))
```

**Résultat:** `ERREUR: Adresse mémoire hors limites: -4` ❌

### Après la Correction

```lisp
(compile-and-run '(labels ((outer (x) 
                            (labels ((mult (n) (* x n)) 
                                     (twice (n) (mult (mult n)))) 
                              (twice 3)))) 
                    (outer 2)))
```

**Résultat:** `12` ✅

**Registres finaux:**
- `$V0 = 12` (résultat)
- `$T0 = 2` (valeur de x)
- `$T1 = 6` (intermédiaire mult(3))
- `$LO = 12` (dernière multiplication)

### Tests Complets

```bash
$ ./run-unit-tests.sh

Fichiers testés  : 11
Fichiers réussis : 11

Tests totaux     : 70
Tests réussis    : 70 ✓
Tests échoués    : 0 ✗

Taux de réussite : 100%

🎉 Tous les tests sont passés avec succès!
```

**Aucune régression** : Les 70 tests existants continuent de passer ✓

---

## 🔍 Code Assembleur Généré

### Avant (Bug)

```asm
(LABEL LABELS_BODY_2)
;; PAS D'INITIALISATION DE $S0 ❌
(ADDI $SP -8 $SP)
(SW $S0 $SP 0)
(SW $RA $SP 4)
(MOVE $S0 $T3)        ; $S0 reste 0 ou invalide
(LI 3 $V0)
(MOVE $V0 $A0)
(MOVE $T3 $S0)        ; Passe 0 à twice ❌
(JAL LOCAL_TWICE_4)
```

### Après (Correct)

```asm
(LABEL LABELS_BODY_2)
(MOVE $FP $S0)        ; ✓ Initialise $S0 = $FP (frame de outer)
(ADDI $SP -8 $SP)
(SW $S0 $SP 0)
(SW $RA $SP 4)
(MOVE $S0 $T3)        ; Sauvegarde $S0
(LI 3 $V0)
(MOVE $V0 $A0)
(MOVE $T3 $S0)        ; Passe $FP_outer à twice ✓
(JAL LOCAL_TWICE_4)
```

---

## 📚 Concepts Clés

### Static Link (Chaînage Statique)

Le **static link** permet aux fonctions imbriquées d'accéder aux variables de leurs scopes englobants.

**Structure d'un frame avec static link:**
```
Frame Layout:
+------------------+
| Old FP           | (FP+0)
| Return Address   | (FP+4)
| Static Link      | (FP+8)  ← Pointe vers frame parent
| Param 1          | (FP-4)
| Param 2          | (FP-8)
| ...              |
+------------------+
```

### Règles de Passage du Static Link

1. **Appel de fonction sibling** (même niveau lexical):
   - Passer `$S0` tel quel
   - Les siblings partagent le même environnement parent
   - Exemple: `twice` appelle `mult` (toutes deux dans LABELS_BODY_2)

2. **Appel de fonction enfant** (niveau imbriqué):
   - Passer `$FP` (notre frame)
   - L'enfant doit accéder à notre environnement
   - Exemple: `LABELS_BODY_2` appelle `twice`

3. **Appel de fonction globale**:
   - Pas de static link nécessaire
   - Accès seulement aux variables globales

### Profondeur Lexicale

```
Niveau 0 (global)
  └─ LABELS_BODY_0
      └─ outer (depth=1)
          └─ LABELS_BODY_2
              ├─ mult (depth=2) ← siblings
              └─ twice (depth=2) ←
```

---

## 🎓 Leçons Apprises

### 1. Importance du Static Link

Le static link est **crucial** pour l'implémentation correcte des closures et des fonctions locales imbriquées. Sans lui, les variables des scopes englobants sont inaccessibles.

### 2. Distinction Sibling vs Enfant

Il est **essentiel** de distinguer:
- **Siblings**: Fonctions au même niveau lexical (partagent le parent)
- **Enfants**: Fonctions à un niveau plus profond (notre frame est leur parent)

### 3. Initialisation dans le Corps des LABELS

Le corps d'un LABELS **n'a pas de frame propre**, mais doit quand même initialiser `$S0` pour que les appels aux fonctions locales fonctionnent.

### 4. Tests de Non-Régression

Après chaque modification, il est **impératif** de ré-exécuter tous les tests pour détecter d'éventuelles régressions.

---

## 🔧 Fichiers Modifiés

| Fichier | Lignes Modifiées | Description |
|---------|-----------------|-------------|
| `src/compiler.lisp` | 1274-1322 | `compile-call`: Distinction siblings/enfants |
| `src/compiler.lisp` | 1189-1206 | `compile-labels`: Initialisation $S0 |

**Commits:**
- Phase 8: Correction bug LABELS static link (compile-call)
- Phase 8: Initialisation $S0 dans corps LABELS

---

## ✅ Validation

### Critères de Succès

- [x] Test 5 closures retourne 12 au lieu de 27
- [x] Aucune régression dans les 70 tests existants
- [x] Code assembleur correct généré
- [x] Static links correctement passés

### Tests Spécifiques

1. **Test siblings basique:**
   ```lisp
   (labels ((f (x) (g x))
            (g (x) (* x 2)))
     (f 5))
   → 10 ✓
   ```

2. **Test LABELS imbriqués:**
   ```lisp
   (labels ((outer (x)
             (labels ((mult (n) (* x n))
                      (twice (n) (mult (mult n))))
               (twice 3))))
     (outer 2))
   → 12 ✓
   ```

3. **Test multiples niveaux:**
   ```lisp
   (labels ((a (x)
             (labels ((b (y)
                       (labels ((c (z) (+ x y z)))
                         (c 3))))
               (b 2))))
     (a 1))
   → 6 ✓
   ```

---

## 🚀 Prochaines Étapes

**Phase 8 terminée avec succès!** ✅

**Prochaine phase:** Phase 9 - Implémentation des CLOSURES
- Durée estimée: 20-30h
- Extension de la VM (tas dynamique)
- Support de LAMBDA
- Capture de variables libres

---

**Conclusion:** La Phase 8 a corrigé avec succès le bug des static links dans LABELS. Le système gère maintenant correctement les appels entre fonctions siblings et entre différents niveaux lexicaux. Les 70 tests passent à 100%, sans aucune régression.
