# Plan de Compilation du Loader Complet

## Objectif
Compiler intégralement `src/loader.lisp` en MIPS en déléguant les structures de données complexes à Lisp via des primitives VM.

## Analyse du fichier loader.lisp

### Fonctions à compiler
1. `collect-labels` - Collecte les labels et leurs positions
2. `resolve-labels` - Résout les références symboliques
3. `keyword-to-symbol` - Conversion keyword → symbole
4. `normalize-instruction` - Normalise une instruction
5. `normalize-code` - Normalise tout le code
6. `parse-asm` - Parse et valide le code
7. `preprocess-code` - Prétraite le code assembleur

### Structures de données complexes à déléguer

#### Hash-tables
```lisp
(make-hash-table :test 'equal)
(gethash key table)
(setf (gethash key table) value)
(hash-table-count table)
(maphash func table)
```

**→ Primitives VM nécessaires:**
- `(vm-make-hash-table test)` → retourne handle (adresse heap)
- `(vm-hash-get handle key)` → retourne valeur ou nil
- `(vm-hash-set handle key value)` → stocke valeur
- `(vm-hash-count handle)` → retourne nombre d'entrées
- `(vm-hash-iterate handle callback)` → itère sur les paires

#### Listes et itération
```lisp
(dolist (var list) body)
(mapcar func list)
(every predicate list)
```

**→ Compilation possible:**
- `dolist` → macro transformée en `while` avec `car`/`cdr`
- `mapcar` → fonction récursive ou boucle
- `every` → boucle avec test

#### Fonctions d'ordre supérieur
```lisp
(lambda (x) ...)
```

**→ Déjà supporté:** closures (PHASE 9)

## Étapes d'implémentation

### Étape 1: Ajouter les primitives hash-table au compilateur
- [ ] Définir les primitives dans `*vm-primitives*`
- [ ] Implémenter `compile-vm-hash-table-prim`
- [ ] Créer instructions spéciales: `HASH-MAKE`, `HASH-GET`, `HASH-SET`, etc.

### Étape 2: Implémenter l'interprétation des primitives dans la VM
- [ ] Intercepter les instructions `HASH-*` dans `execute-instruction`
- [ ] Gérer un heap séparé pour les hash-tables
- [ ] Mapping handle → hash-table Lisp

### Étape 3: Compiler les macros manquantes
- [ ] `dolist` → expansion en `while` + `car`/`cdr`
- [ ] `loop` (version simplifiée)
- [ ] `multiple-value-bind` (simplification: ignorer valeurs multiples)

### Étape 4: Tester progressivement
1. Compiler `keyword-to-symbol` (simple, pas de hash-table)
2. Compiler `collect-labels` (utilise hash-table)
3. Compiler `resolve-labels` (utilise hash-table + mapcar)
4. Compiler `preprocess-code` (orchestre tout)

### Étape 5: Intégration finale
- [ ] Remplacer `*loader-lisp-source*` par `(load "src/loader.lisp")`
- [ ] Compiler toutes les fonctions du loader
- [ ] Tester avec fibonacci(20)

## Primitives VM proposées

### Format des instructions
```lisp
;; Création
(:HASH-MAKE test-fn)          ; Crée hash-table, retourne handle dans $V0

;; Accès
(:HASH-GET handle key)        ; Lit valeur, retourne dans $V0 (0 si absent)
(:HASH-SET handle key value)  ; Écrit valeur

;; Info
(:HASH-COUNT handle)          ; Retourne nombre d'entrées dans $V0
(:HASH-HAS-KEY handle key)    ; Retourne 1/0 dans $V0

;; Itération (complexe, peut-être pas nécessaire)
(:HASH-FOREACH handle fn-addr) ; Appelle fn pour chaque paire (key, value)
```

### Gestion des handles
- Handle = adresse dans le heap VM
- La VM maintient une table: handle → hash-table Lisp native
- Permet de profiter de l'implémentation Lisp efficace

## Extensions futures
- `format` avec primitives d'impression
- `error` avec gestion d'exceptions
- Structures (`defstruct`)
- Arrays associatifs

## Exemple de transformation

### Code source
```lisp
(defun collect-labels (asm-code code-start)
  (let ((labels (make-hash-table :test 'equal))
        (position 0))
    (dolist (instr asm-code)
      (if (and (listp instr) (eq (first instr) :LABEL))
          (setf (gethash (second instr) labels) (+ code-start position))
          (incf position)))
    labels))
```

### Code compilé (conceptuel)
```lisp
(defun collect-labels (asm-code code-start)
  (let ((labels (vm-make-hash-table 'equal))
        (position 0)
        (lst asm-code))
    (while lst
      (let ((instr (car lst)))
        (if (and (listp instr) (eq (car instr) :LABEL))
            (vm-hash-set labels (cadr instr) (+ code-start position))
            (setq position (+ position 1)))
        (setq lst (cdr lst))))
    labels))
```

### Instructions MIPS générées
```
HASH-MAKE 'equal          ; Crée hash-table
MOVE $V0 $S2              ; Sauvegarder handle dans $S2
...
HASH-SET $S2 key value    ; Stocker dans hash-table
...
```

## Avantages de cette approche
1. **Simplicité**: Pas besoin d'implémenter hash-tables en MIPS
2. **Performance**: Utilise l'implémentation native Lisp (très optimisée)
3. **Extensibilité**: Facile d'ajouter d'autres primitives
4. **Débogage**: Les structures restent inspectables côté Lisp

## Inconvénients
1. Dépendance à l'interpréteur Lisp (pas de code MIPS pur)
2. Performances réduites pour les appels primitives (overhead)
3. Sérialisation complexe si on veut sauvegarder l'état VM

## Priorité
**Phase 1:** Hash-tables uniquement (suffisant pour loader.lisp)
**Phase 2:** Macros `dolist`, `loop`
**Phase 3:** Autres primitives selon besoins
