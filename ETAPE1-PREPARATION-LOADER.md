# Étape 1 : Préparation pour la Compilation du Loader Complet

## 📁 Fichiers créés

### 1. `simple-loader.lisp`
Version simplifiée du loader, entièrement compilable en MIPS.
- **Fonction**: `simple-loader(code-addr, data-addr, count)`
- **Taille compilée**: 66 instructions MIPS
- **Performance**: Charge et exécute fibonacci(20) en ~10 secondes
- **Status**: ✅ **TESTÉ ET FONCTIONNEL**

### 2. `plan-compilation-loader.md`
Plan détaillé pour compiler `src/loader.lisp` complet.
- Analyse des structures de données nécessaires
- Stratégie de délégation à Lisp
- Liste des primitives à implémenter
- Étapes d'implémentation progressives

## 🔧 Modifications du compilateur

### Ajout des primitives VM dans `src/compiler.lisp`

#### Primitives Hash-Table
```lisp
(make-hash-table :test 'equal)  → :HASH-MAKE
(gethash key table)             → :HASH-GET
(hash-set table key value)      → :HASH-SET
(hash-table-count table)        → :HASH-COUNT
(hash-has-key table key)        → :HASH-HAS-KEY
```

**Fonctionnement:**
- Le compilateur génère des instructions spéciales `:HASH-*`
- La VM intercepte ces instructions
- La VM délègue à des hash-tables Lisp natives
- Les handles (adresses heap) référencent les tables Lisp

#### Primitives de Prédicats de Type
```lisp
(listp x)    → :TYPE-CHECK listp $V0
(symbolp x)  → :TYPE-CHECK symbolp $V0
(keywordp x) → :TYPE-CHECK keywordp $V0
(consp x)    → :TYPE-CHECK consp $V0
(atom x)     → :TYPE-CHECK atom $V0
```

#### Primitives sur Listes
```lisp
(car lst)    → :LIST-CAR $V0
(cdr lst)    → :LIST-CDR $V0
(first lst)  → :LIST-CAR $V0
(second lst) → :LIST-CDR $V0, :LIST-CAR $V0
(rest lst)   → :LIST-CDR $V0
(cons a b)   → :LIST-CONS $T0 $T1
```

**Avantages:**
- Réutilise l'implémentation Lisp efficace
- Pas besoin d'encoder les listes en MIPS
- Compatibilité avec les structures Lisp existantes

## 📊 État actuel

### ✅ Compilateur étendu
- [x] Primitives hash-table définies dans le compilateur
- [x] Primitives de type définies
- [x] Primitives sur listes définies
- [x] Code de compilation pour toutes les primitives

### ⏳ VM (À faire)
- [ ] Interception des instructions `:HASH-*` dans `execute-instruction`
- [ ] Gestion du heap pour les handles de hash-tables
- [ ] Mapping handle → hash-table Lisp
- [ ] Interception des instructions `:TYPE-CHECK`
- [ ] Interception des instructions `:LIST-*`

### ⏳ Macros (À faire)
- [ ] Expansion de `dolist` → `while` + `car`/`cdr`
- [ ] Support de `loop` (version simplifiée)
- [ ] Support de `every`, `mapcar`

## 🎯 Prochaines étapes

### Étape 2: Implémenter l'interprétation des primitives dans la VM

#### Dans `src/vm.lisp`:
1. Ajouter une table globale `*vm-hash-tables*` (handle → hash-table)
2. Modifier `execute-instruction` pour intercepter:
   - `:HASH-MAKE`, `:HASH-GET`, `:HASH-SET`, `:HASH-COUNT`
   - `:TYPE-CHECK`
   - `:LIST-CAR`, `:LIST-CDR`, `:LIST-CONS`

#### Structure du code VM:
```lisp
(defparameter *vm-hash-tables* (make-hash-table)
  "Mapping: handle (adresse heap) → hash-table Lisp")

(defparameter *vm-hash-handle-counter* 1000
  "Compteur pour générer des handles uniques")

(defun execute-hash-make (vm test-fn)
  "Crée une hash-table et retourne un handle"
  (let* ((handle (incf *vm-hash-handle-counter*))
         (ht (make-hash-table :test test-fn)))
    (setf (gethash handle *vm-hash-tables*) ht)
    (set-register vm *reg-v0* handle)
    handle))
```

### Étape 3: Tester progressivement

1. **Test primitif**: Créer et accéder à une hash-table simple
   ```lisp
   (let ((ht (make-hash-table :test 'equal)))
     (hash-set ht "key" 42)
     (gethash "key" ht))
   ```

2. **Test avec boucle**: Remplir une hash-table
   ```lisp
   (let ((ht (make-hash-table))
         (i 0))
     (while (< i 10)
       (hash-set ht i (* i i))
       (setq i (+ i 1)))
     (hash-table-count ht))
   ```

3. **Test `keyword-to-symbol`**: Première vraie fonction de loader.lisp
   ```lisp
   (defun keyword-to-symbol (kw)
     (if (keywordp kw)
         (intern (symbol-name kw))
         kw))
   ```

4. **Test `collect-labels`**: Fonction avec hash-table
   ```lisp
   (defun collect-labels (asm-code code-start)
     (let ((labels (make-hash-table :test 'equal))
           (position 0))
       (dolist (instr asm-code)
         (if (and (listp instr) (eq (first instr) :LABEL))
             (hash-set labels (second instr) (+ code-start position))
             (setq position (+ position 1))))
       labels))
   ```

### Étape 4: Compilation complète de loader.lisp

Une fois les primitives fonctionnelles:
- Compiler `collect-labels`
- Compiler `resolve-labels`
- Compiler `preprocess-code`
- Tester avec le fichier complet

## 📝 Notes importantes

### Limitations actuelles
- `maphash` non supporté (nécessite closures en paramètres)
- `mapcar` nécessitera une implémentation récursive ou primitive
- `multiple-value-bind` simplifié (ignorer valeurs multiples)

### Solutions de contournement
- `maphash` → convertir hash-table en liste de paires
- `mapcar` → boucle while avec accumulation
- `multiple-value-bind` → utiliser seulement la première valeur

### Performance attendue
- Overhead des appels primitives: ~10-20%
- Hash-tables Lisp natives: très performantes
- Total: acceptable pour un loader compilé

## 🎓 Apprentissages

Cette approche hybride (compilation + primitives) est une technique classique:
- **SBCL**: utilise des intrinsèques pour opérations complexes
- **LuaJIT**: FFI pour structures externes
- **PyPy**: RPython avec primitives Python

**Avantage principal**: Permet de compiler du code complexe sans réimplémenter toutes les structures de données en langage cible.
