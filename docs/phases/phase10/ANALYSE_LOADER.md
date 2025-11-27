# 📋 Analyse du Loader pour Bootstrap (Étape 2.1)

**Date**: 27 novembre 2025  
**Fichier analysé**: `src/loader.lisp` (182 lignes)  
**Objectif**: Identifier les fonctions à compiler et les dépendances natives à remplacer

---

## 🔍 Structure du Loader

### Fonctions Principales

#### 1. **`collect-labels`** (ligne ~9)
```lisp
(defun collect-labels (asm-code code-start)
  "Collecte tous les labels et leurs positions ABSOLUES dans le code"
  (let ((labels (make-hash-table :test 'equal))
        (position 0))
    (dolist (instr asm-code)
      (if (and (listp instr) (eq (first instr) :LABEL))
          (setf (gethash (second instr) labels) (+ code-start position))
          (incf position)))
    labels))
```
**Complexité**: Basse  
**Dépendances natives**:
- `make-hash-table` - ❌ Non compilable directement
- `dolist` - ✅ Peut être transformé en récursion
- `gethash`, `setf` - ❌ Manipulation de hash-table

**Stratégie**: **Remplacer hash-table par liste d'association** `((label . adresse) ...)`

---

#### 2. **`resolve-labels`** (ligne ~22)
```lisp
(defun resolve-labels (asm-code labels)
  "Remplace les références symboliques par des adresses"
  (let ((resolved-code '()))
    (dolist (instr asm-code)
      (when (not (and (listp instr) (eq (first instr) :LABEL)))
        (let ((resolved-instr 
               (mapcar (lambda (element)
                         (if (and (symbolp element)
                                  (gethash element labels))
                             (gethash element labels)
                             element))
                       instr)))
          (push resolved-instr resolved-code))))
    (nreverse resolved-code)))
```
**Complexité**: Moyenne  
**Dépendances natives**:
- `mapcar` - ✅ Déjà remplacé par `my-mapcar`
- `lambda` - ✅ Supporté par le compilateur
- `gethash` - ❌ À remplacer par `my-assoc`

**Stratégie**: Utiliser `my-mapcar` + `my-assoc` pour lookup des labels

---

#### 3. **`parse-asm`** (ligne ~45)
```lisp
(defun parse-asm (code)
  "Parse et valide le code assembleur"
  (cond
    ((and (listp code) (every #'listp code))
     code)
    ((listp code)
     (list code))
    (t (error "Format de code invalide: ~A" code))))
```
**Complexité**: Basse  
**Dépendances natives**:
- `every` - ❌ Prédicat sur liste
- `error` - ❌ Gestion d'erreur

**Stratégie**: 
- Créer `my-every` dans primitives
- Remplacer `error` par retour de NIL ou valeur sentinelle

---

#### 4. **`preprocess-code`** (ligne ~57)
```lisp
(defun preprocess-code (asm-code code-start)
  "Prétraite le code assembleur (résolution des labels, etc.)"
  (let* ((parsed (parse-asm asm-code))
         (labels (collect-labels parsed code-start))
         (resolved (resolve-labels parsed labels)))
    (values resolved labels)))
```
**Complexité**: Basse (composition)  
**Dépendances natives**:
- `values` - ❌ Retour multiple

**Stratégie**: Retourner une cons `(resolved . labels)`

---

#### 5. **`load-code`** (ligne ~74)
```lisp
(defun load-code (vm asm-code &key (verbose nil))
  "Charge le code assembleur dans la mémoire de la VM"
  ...)
```
**Complexité**: Haute  
**Dépendances natives**:
- `&key` arguments - ❌ Non supporté par compilateur actuel
- `format` - ✅ Peut être désactivé (debug)
- `maphash` - ❌ Itération sur hash-table
- `multiple-value-bind` - ❌ Décomposition retour multiple

**Stratégie**: 
- Simplifier signature: `(load-code vm asm-code)` (retirer verbose)
- Utiliser `car`/`cdr` pour décomposer cons au lieu de `multiple-value-bind`

---

#### 6. **`calculate-code-start`** (ligne ~66)
```lisp
(defun calculate-code-start (vm)
  "Calcule l'adresse de début de la zone code"
  (- *maxmem* *code-size*))
```
**Complexité**: Triviale  
**Dépendances**: Aucune (pure arithmétique)

**Stratégie**: ✅ **Compilable directement**

---

### Fonctions Utilitaires (Non prioritaires)

- **`append-code`** (ligne ~115) - Chargement incrémental (pas essentiel pour bootstrap)
- **`dump-code`** (ligne ~143) - Debug (pas nécessaire)
- **`load-and-run`** (ligne ~153) - Wrapper de convenance (pas essentiel)

---

## 🚧 Dépendances Natives à Remplacer

### Nouvelles primitives nécessaires

| Fonction Native | Primitive Bootstrap | Implémentation |
|----------------|-------------------|----------------|
| `make-hash-table` | → **Liste assoc** | `'()` |
| `gethash` | → **`my-assoc`** | Déjà implémenté ✅ |
| `setf (gethash ...)` | → **`my-acons`** | `(cons (cons key val) alist)` |
| `every` | → **`my-every`** | Récursion sur liste |
| `maphash` | → **`my-map-alist`** | Itération sur liste assoc |
| `error` | → **Retour NIL** | Simplification |
| `values` | → **cons** | `(cons val1 val2)` |
| `multiple-value-bind` | → **let** | `(let ((val (car res)))` |

---

## 📝 Plan de Compilation

### Phase A : Compléter les Primitives (~30min)

Ajouter à `src/primitives.lisp`:

```lisp
;; Prédicat every
(defun my-every (predicate lst)
  "Vérifie que tous les éléments satisfont le prédicat"
  (cond
    ((null lst) t)
    ((funcall predicate (car lst)) (my-every predicate (cdr lst)))
    (t nil)))

;; Construction liste assoc
(defun my-acons (key value alist)
  "Ajoute (key . value) à une liste d'association"
  (cons (cons key value) alist))

;; Itération sur liste assoc
(defun my-map-alist (fn alist)
  "Applique fn à chaque paire (key . value) d'une alist"
  (cond
    ((null alist) nil)
    (t (funcall fn (caar alist) (cdar alist))
       (my-map-alist fn (cdr alist)))))
```

### Phase B : Créer loader-bootstrap.lisp (~1h)

Créer `src/loader-bootstrap.lisp` avec adaptations:

1. Remplacer hash-tables par listes d'association
2. Supprimer arguments `&key` 
3. Remplacer `values` par cons
4. Simplifier gestion d'erreurs
5. Retirer code de debug (format)

### Phase C : Compiler les Fonctions (~1.5h)

Ordre de compilation (du plus simple au plus complexe):

1. ✅ **`calculate-code-start`** - Trivial, pur arithmétique
2. ✅ **`collect-labels`** - Boucle simple avec accumulation
3. ✅ **`parse-asm`** - Conditions simples
4. ✅ **`resolve-labels`** - Transformation de liste
5. ✅ **`preprocess-code`** - Composition
6. ✅ **`load-code`** - Fonction principale

**Commandes de compilation**:
```lisp
(load "main.lisp")
(load "src/compiler-bootstrap.lisp")
(load "src/loader-bootstrap.lisp")

;; Test individuel
(compile-lisp '(defun calculate-code-start (vm) (- *maxmem* *code-size*)))
```

### Phase D : Tests et Validation (~1h)

Tests progressifs:

```lisp
;; Test 1: calculate-code-start
(compile-and-run '(calculate-code-start vm))

;; Test 2: collect-labels avec code simple
(compile-and-run '(collect-labels '((:LABEL START) (:LI 1 :$V0) (:HALT)) 5000))

;; Test 3: resolve-labels
(compile-and-run '(resolve-labels '((:J START) (:LABEL START) (:HALT)) 
                                   '((START . 5001))))

;; Test 4: Chargement complet
(compile-and-run '(load-code vm '((:LI 5 :$V0) (:HALT))))
```

---

## ⚠️ Limitations et Contraintes

### Fonctionnalités Non Compilables (à retirer)

1. **Arguments keyword (`&key`)** - Le compilateur ne les supporte pas
   - Solution: Paramètres positionnels uniquement
   
2. **Retours multiples (`values`, `multiple-value-bind`)** - Non supporté
   - Solution: Retourner cons ou liste
   
3. **Hash-tables** - Structure de données non primitive
   - Solution: Listes d'association (performance réduite mais acceptable)
   
4. **Messages debug (`format`)** - Dépendance native
   - Solution: Désactiver complètement ou utiliser `my-format-*`
   
5. **Gestion d'erreurs (`error`, `handler-case`)** - Non supporté
   - Solution: Retour de valeurs sentinelles (NIL, -1, etc.)

### Fonctionnalités Conservées

✅ **Opérations supportées**:
- Arithmétique: `+`, `-`, `*`, `/`
- Comparaisons: `<`, `>`, `=`, `eq`
- Listes: `car`, `cdr`, `cons`, `list`, `append`
- Conditions: `if`, `cond`, `when`, `unless`
- Boucles: Récursion (pas de `loop` ou `dolist` natif)
- Fonctions locales: `labels`, `let`
- Lambdas: `lambda` supporté

---

## 🎯 Critères de Succès

### Étape 2 Complète Si:

✅ `src/loader-bootstrap.lisp` créé avec toutes dépendances natives retirées  
✅ Nouvelles primitives ajoutées à `src/primitives.lisp` (my-every, my-acons, etc.)  
✅ Au moins 3 fonctions compilées avec succès (calculate-code-start, collect-labels, parse-asm)  
✅ Test de chargement: Code simple (`:LI + :HALT`) charge et s'exécute dans VM  
✅ Fichier `bootstrap/loader-compiled.asm` généré (même partiel)  

---

## 📊 Estimation Révisée

| Sous-tâche | Estimation initiale | Estimation révisée | Raison |
|------------|---------------------|-------------------|---------|
| 2.1 Analyse | 30min | **30min** ✅ | Conforme |
| 2.2 Primitives | - | **30min** | Ajout my-every, my-acons, etc. |
| 2.3 Adaptation loader | 1h | **1h** | Remplacement hash-tables |
| 2.4 Compilation | 1.5h | **1-2h** | 6 fonctions à compiler |
| 2.5 Tests | 1h | **1h** | Validation progressive |
| **TOTAL** | **3-4h** | **3.5-4.5h** | Légèrement plus long |

---

## 🚀 Prochaine Action

**Immédiate**: Ajouter les nouvelles primitives à `src/primitives.lisp`

```bash
# Commande suivante
# Éditer src/primitives.lisp pour ajouter:
# - my-every
# - my-acons  
# - my-map-alist
```

Puis créer `src/loader-bootstrap.lisp` avec adaptations.

---

**Document créé**: 27/11/2025  
**Temps analyse**: 30min ✅  
**Prêt pour**: Étape 2.2 (Ajout primitives)
