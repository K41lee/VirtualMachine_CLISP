# Rapport de Débogage - Array-Sum Bug

## Date: 6 janvier 2026

## Résumé
Implémentation des tableaux (arrays) pour la VM MIPS-like. **50% des tests passent (4/8)**.

## Tests Réussis ✅
1. **make-array simple** - Création de tableaux
2. **aref simple** - Lecture d'éléments (valeurs par défaut)
3. **setf aref** - Écriture d'un élément
4. **setf plusieurs éléments** - Écriture et lecture de plusieurs éléments avec arithmétique

## Tests Échoués ❌
Tous les tests échoués impliquent des **fonctions récursives** manipulant des tableaux:

1. **array-sum** - Somme récursive des éléments (obtenu: 3, attendu: 10)
2. **array-max** - Maximum récursif (obtenu: 4, attendu: 9)  
3. **fill-array** - Remplissage récursif (obtenu: 7, attendu: 21)
4. **fill-sequence** - Remplissage séquentiel (obtenu: 5, attendu: 15)

## Composants Fonctionnels

### Instructions VM (vm.lisp)
- ✅ **MAKE-ARRAY**: Allocation sur le heap avec format [taille][elem0][elem1]...
- ✅ **AREF**: Lecture avec vérification de bornes
- ✅ **ASET**: Écriture avec vérification de bornes

### Support Compilateur (compiler-simplified.lisp)
- ✅ **make-array**: Compilation correcte
- ✅ **aref**: Compilation correcte (même avec expressions complexes pour l'index)
- ✅ **setf aref**: Compilation correcte
- ✅ **Arithmétique avec pile**: Utilise $S0 avec sauvegarde/restauration
- ✅ **Gestion environnement**: Ajustement des offsets lors de modifications de $SP

### Fonctionnalités Testées et Validées
- ✅ Récursion simple (countdown, sum-to-n)
- ✅ Fonctions avec 2 paramètres (add-twice)
- ✅ Passage de tableaux comme paramètres
- ✅ Addition avec aref: `(+ constant (aref arr index))`
- ✅ Aref avec expression: `(aref arr (- n 1))`
- ✅ Récursion simple avec tableaux (get-elem)

## Le Bug Mystérieux 🐛

### Symptôme
La fonction `array-sum` retourne des valeurs incorrectes:
- `array-sum([42], 1)` → retourne **0** au lieu de **42**
- `array-sum([10,20], 2)` → retourne **1** au lieu de **30**
- `array-sum([1,2,3,4], 4)` → retourne **3** au lieu de **10**

### Code de Test
```lisp
(defun array-sum (arr n)
  (if (= n 0)
      0
      (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
```

### Analyse du Code Assembleur Généré

#### Structure Générale
```assembly
[23] ELSE_1:                    ; Branche récursive
[24] ADDI $SP -4                ; Sauvegarde #1 pour arithmetic-with-stack
[25] SW $S0 $SP 0               ; Sauvegarde $S0 original

[26-37] Calcul (aref arr (- n 1))  → résultat dans $V0
[38] MOVE $V0 $S0               ; $S0 = aref_result

[39] ADDI $SP -16               ; Espace pour registres sauvegardés
[40] SW $S0 $SP 0               ; Sauvegarde #2: $S0 = aref_result

[44-59] Préparation args + appel récursif
[60] JAL ARRAY-SUM              ; Appel récursif

[61] LW $S0 $SP 0               ; Restaure $S0 = aref_result  
[65] ADDI $SP 16                ; Libère espace registres

[66] MOVE $S0 $T0               ; $T0 = aref_result (pour addition)
[67] LW $S0 $SP 0               ; Restaure $S0 original (sauvegarde #1)
[68] ADDI $SP 4                 ; Libère sauvegarde #1

[69] ADD $V0 $T0 $V0            ; $V0 = recursive_result + aref_result
```

#### Théorie: Le Code Devrait Fonctionner
Selon l'analyse ligne par ligne:
1. L'aref est calculé et sauvegardé dans $S0
2. $S0 est sauvegardé sur la pile avant l'appel récursif
3. Après le retour, $S0 est restauré
4. La valeur est transférée à $T0 pour l'addition
5. L'addition se fait correctement

**Mais en pratique, ça ne marche pas!**

### Tests d'Isolation

| Test | Description | Résultat |
|------|-------------|----------|
| `sum-to-n(3)` | Récursion + accumulation SANS tableau | ✅ **6** (correct) |
| `add-twice(5,3)` | Récursion + 2 params SANS tableau | ✅ **15** (correct) |
| `get-first(arr)` | Tableau passé en param, aref simple | ✅ **99** (correct) |
| `add-to-first(arr, 5)` | Tableau + aref + addition | ✅ **15** (correct) |
| `get-elem(arr, 2)` | Récursion simple AVEC tableau | ✅ **100** (correct) |
| `array-sum([42], 1)` | Récursion + accumulation + tableau | ❌ **0** (attendu 42) |

### Hypothèses Explorées

#### ❌ Problème de registre $S0 écrasé
- **Rejeté**: $S0 est correctement sauvegardé/restauré via pile
- Code: lignes 24-25 (save) et 67-68 (restore)

#### ❌ Problème d'environnement/offsets
- **Rejeté**: Les offsets sont ajustés correctement avec `adjust-all-offsets`
- Ligne 862: `(compile-load-args args (adjust-all-offsets env 16) 0)`

#### ❌ Problème de paramètres
- **Rejeté**: Les paramètres arr et n sont correctement sauvegardés ($FP+8, $FP+12)
- $FP est restauré après l'appel récursif

#### ❌ Problème avec aref
- **Rejeté**: aref fonctionne correctement dans tous les autres contextes

#### ❌ Problème avec l'addition
- **Rejeté**: L'addition fonctionne dans d'autres contextes

### Pistes Restantes

#### 🔍 Piste 1: Interaction Subtile Pile/Registres
Possible qu'une des nombreuses sauvegardes/restaurations écrase une valeur critique à un moment précis de l'exécution imbriquée.

#### 🔍 Piste 2: Corruption Mémoire
L'appel récursif pourrait écrire à une adresse qui chevauche une sauvegarde importante, mais le traçage manuel ne montre pas de chevauchement évident.

#### 🔍 Piste 3: Bug dans compile-load-args
Quand il y a 2 arguments ET que le second est une expression complexe, il pourrait y avoir un problème subtil avec:
```lisp
(compile-load-args (cdr args) (adjust-all-offsets env 4) (+ index 1))
```

## Corrections Appliquées

### 1. Fix de compile-funcall-simplified (ligne 862)
**Avant:**
```lisp
(compile-load-args args env 0)
```
**Après:**
```lisp
(compile-load-args args (adjust-all-offsets env 16) 0)
```
**Raison:** Ajuster l'environnement pour tenir compte du décalage de $SP (-16 pour les registres sauvegardés).

### 2. Fix de compile-load-args (ligne 908)
**Avant:**
```lisp
(compile-load-args (cdr args) env (+ index 1))
```
**Après:**
```lisp
(compile-load-args (cdr args) (adjust-all-offsets env 4) (+ index 1))
```
**Raison:** Ajuster l'environnement pour le $SP décrémenté lors de la sauvegarde d'arguments.

### 3. Réécriture de compile-arithmetic-with-stack
**Stratégie:** Utilisation de $S0 avec sauvegarde/restauration explicite au lieu de la pile directe.

**Code actuel:**
```lisp
(defun compile-arithmetic-with-stack (op args env)
  (append-many
    (list
      ;; Sauvegarder $S0
      (list (list :ADDI *reg-sp* -4 *reg-sp*))
      (list (list :SW *reg-s0* *reg-sp* 0))
      ;; Premier argument → $S0
      (compile-expr-main (first args) (adjust-all-offsets env 4))
      (list (list :MOVE *reg-v0* *reg-s0*))
      ;; Second argument → $V0
      (compile-expr-main (second args) (adjust-all-offsets env 4))
      ;; Premier arg → $T0
      (list (list :MOVE *reg-s0* *reg-t0*))
      ;; Restaurer $S0
      (list (list :LW *reg-s0* *reg-sp* 0))
      (list (list :ADDI *reg-sp* 4 *reg-sp*))
      ;; Opération
      (compile-arithmetic-op-simple op))))
```

**Avantage:** Évite les conflits d'adresses entre différentes utilisations de la pile.

## Prochaines Étapes Suggérées

### Option A: Traçage Détaillé de l'Exécution
Ajouter des hooks dans la VM pour tracer:
- Chaque instruction exécutée
- Les valeurs de $S0, $T0, $V0 à chaque étape
- Les écritures/lectures mémoire aux adresses critiques

### Option B: Simplification Radicale
Tester une version encore plus simple:
```lisp
(defun array-sum-iterative (arr n)
  (if (= n 0)
      0
      (+ (aref arr 0) 0)))  ; Pas de récursion!
```

Si ça échoue aussi, le problème est avec aref dans le contexte d'une fonction à 2 paramètres + if + addition.

### Option C: Analyse Comparative
Comparer le code assembleur généré pour:
- `sum-to-n` (qui marche)
- `array-sum` (qui ne marche pas)

Identifier les différences structurelles.

### Option D: Debug avec Instructions MIPS Natives
Implémenter un mode "trace" qui affiche l'état complet à chaque instruction, spécifiquement pour array-sum.

## Conclusion

L'implémentation des tableaux est **fonctionnelle à 50%**. Tous les cas d'usage simples et moyennement complexes fonctionnent. Le bug restant est très spécifique et subtil, lié à l'interaction entre:
- Récursion avec accumulation
- Tableaux passés en paramètre
- Opérations arithmétiques imbriquées
- Gestion complexe de la pile

Le code généré SEMBLE correct selon l'analyse statique, mais produit des résultats incorrects à l'exécution, suggérant un bug très subtil dans la gestion de la pile ou des registres lors d'appels récursifs complexes.
