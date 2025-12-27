# ANALYSE DES BESOINS POUR COMPILER LE COMPILATEUR

**Date** : 26 décembre 2025  
**Objectif** : Identifier toutes les fonctionnalités manquantes pour rendre le compilateur auto-compilable

---

## 🔍 FONCTIONS UTILISÉES PAR LE COMPILATEUR

### 1. Hash Tables (CRITIQUE)
Le compilateur utilise massivement les hash tables pour :
- `*global-constants*` : Table des constantes globales
- `*global-variables*` : Table des variables globales

**Fonctions nécessaires** :
- ✅ `make-hash-table` : Créer une table de hachage
- ✅ `gethash` : Récupérer une valeur (9 occurrences)
- ✅ `setf` + `gethash` : Modifier une valeur (2 occurrences)
- ⚠️ `clrhash` : Réinitialiser (1 occurrence dans reset-global-tables)
- ⚠️ `hash-table-count` : Optionnel (pas critique)

### 2. Listes Associatives (HAUTE PRIORITÉ)
Utilisées pour l'environnement de compilation :
- ✅ `assoc` : Recherche par clé (5+ occurrences)
- ✅ `member` : Test d'appartenance (3+ occurrences)

### 3. Fonctions de Liste (MOYENNE PRIORITÉ)
- ✅ `append` : Concaténation (10+ occurrences)
- ✅ `mapcar` : Transformation (5+ occurrences)
- ⚠️ `reverse` : Inversion (1 occurrence)
- ⚠️ `remove` : Filtrage
- ⚠️ `union` : Union d'ensembles (1 occurrence)

### 4. Prédicats (MOYENNE PRIORITÉ)
- ✅ `every` : Test universel
- ✅ `some` : Test existentiel
- ✅ `listp` : Test de liste

### 5. Fonctions d'Ordre Supérieur (HAUTE PRIORITÉ)
- ✅ `lambda` : Déjà supporté
- ⚠️ `funcall` : Appel dynamique
- ⚠️ `apply` : Application de liste

---

## 📊 PRIORISATION DES IMPLÉMENTATIONS

### Phase 1 : Hash Tables (BLOQUANT)
**Temps estimé** : 4-6 heures

Sans hash tables, impossible de compiler le compilateur car :
- Stockage des constantes globales
- Stockage des variables globales
- Utilisé dès le début du compilateur

**Implémentations requises** :
1. ✅ Structure de données pour hash table
2. ✅ `make-hash-table`
3. ✅ `gethash`
4. ✅ `setf` avec `gethash` (SETF-GETHASH)
5. ⚠️ `clrhash` (optionnel)

### Phase 2 : Listes Associatives (HAUTE)
**Temps estimé** : 2-3 heures

Nécessaire pour l'environnement de compilation.

**Implémentations requises** :
1. ✅ `assoc` : (assoc key alist) → (key . value) ou nil
2. ✅ `member` : (member item list) → sous-liste ou nil

### Phase 3 : Fonctions de Liste Avancées (MOYENNE)
**Temps estimé** : 3-4 heures

**Implémentations requises** :
1. ✅ `append` : (append list1 list2 ...)
2. ✅ `mapcar` : (mapcar fn list)
3. ⚠️ `reverse` : (reverse list)

### Phase 4 : Prédicats et HOF (BASSE)
**Temps estimé** : 2-3 heures

Peut-être contourné par refactoring du compilateur.

---

## 🎯 STRATÉGIE D'IMPLÉMENTATION

### Approche 1 : Implémentation Minimale (Recommandée)
Implémenter uniquement ce qui est **absolument nécessaire** :
1. Hash tables basiques (make, get, set)
2. assoc et member
3. append (déjà peut-être supporté partiellement)

**Avantage** : Rapide, focus sur l'essentiel  
**Temps** : 6-8 heures

### Approche 2 : Implémentation Complète
Implémenter toutes les fonctions utilisées par le compilateur.

**Avantage** : Compilateur entièrement auto-compilable  
**Temps** : 12-15 heures

---

## 🔧 PLAN D'IMPLÉMENTATION PHASE 1 : HASH TABLES

### Représentation en Mémoire

Une hash table sera représentée comme une structure :
```
Hash-Table Structure:
[0] : Type marker (ex: 42 pour hash-table)
[1] : Size (nombre de buckets)
[2] : Count (nombre d'entrées)
[3] : Pointer vers tableau de buckets
```

Chaque bucket est une liste associative (alist) :
```
Bucket = Liste de (KEY . VALUE)
```

### Fonction de Hachage

Pour simplifier, utiliser modulo sur l'adresse/valeur :
```lisp
hash(key) = key mod size
```

### Implémentations

#### 1. MAKE-HASH-TABLE
```lisp
(defun compile-make-hash-table (args env)
  "Crée une hash table avec N buckets"
  ;; Allouer structure hash-table (4 mots)
  ;; Allouer tableau de buckets (N mots)
  ;; Initialiser chaque bucket à NIL
  ;; Retourner adresse de la structure
  )
```

**Code MIPS** :
- Allouer 4 mots sur le heap pour la structure
- Allouer N mots pour les buckets (N=16 par défaut)
- Initialiser tous les buckets à 0 (NIL)
- Retourner adresse dans $V0

#### 2. GETHASH
```lisp
(defun compile-gethash (key-expr table-expr env)
  "Recherche key dans table"
  ;; 1. Calculer hash = key mod size
  ;; 2. Accéder au bucket[hash]
  ;; 3. Parcourir la liste associative
  ;; 4. Retourner valeur si trouvé, sinon NIL
  )
```

**Code MIPS** :
- Calculer hash avec DIV/MFHI (reste de division)
- Charger pointeur du bucket
- Boucle sur la liste associative avec CAR/CDR
- Comparer clés avec =
- Retourner valeur ou 0

#### 3. SETF-GETHASH
```lisp
(defun compile-setf-gethash (key-expr value-expr table-expr env)
  "Insère/modifie (key . value) dans table"
  ;; 1. Calculer hash = key mod size
  ;; 2. Accéder au bucket[hash]
  ;; 3. Chercher clé dans liste
  ;; 4. Si trouvé : modifier valeur
  ;; 5. Sinon : ajouter (key . value) en tête
  )
```

**Code MIPS** :
- Calculer hash
- Charger bucket
- Chercher clé existante
- Si trouvé : SW pour modifier
- Sinon : CONS pour ajouter

---

## 📝 ÉTAPES D'IMPLÉMENTATION

### Étape 1 : Structures de Données (2h)
- [ ] Définir format hash-table en mémoire
- [ ] Créer fonctions d'allocation
- [ ] Tester allocation basique

### Étape 2 : MAKE-HASH-TABLE (1h)
- [ ] Implémenter compile-make-hash-table
- [ ] Ajouter au parser
- [ ] Ajouter au dispatcher compile-expr
- [ ] Tests basiques

### Étape 3 : GETHASH (2h)
- [ ] Implémenter compile-gethash
- [ ] Fonction de hachage (MOD)
- [ ] Parcours de bucket (alist)
- [ ] Tests avec make-hash-table

### Étape 4 : SETF-GETHASH (2h)
- [ ] Implémenter compile-setf-gethash
- [ ] Gestion cas : nouvelle clé vs existante
- [ ] Tests insertion/modification

### Étape 5 : Tests Intégration (1h)
- [ ] Test : créer table, insérer, récupérer
- [ ] Test : collision de hash
- [ ] Test : modification de valeur existante

### Étape 6 : Version Simplifiée du Compilateur (2h)
- [ ] Créer simple-compiler.lisp
- [ ] Utiliser uniquement hash tables
- [ ] Tester compilation

---

## 🚀 ALTERNATIVE : VERSION ULTRA-SIMPLIFIÉE

Si les hash tables sont trop complexes, utiliser des **listes associatives globales** :

```lisp
;; Au lieu de :
(gethash 'my-const *global-constants*)

;; Utiliser :
(cdr (assoc 'my-const *global-constants*))
```

Avantages :
- Plus simple à implémenter
- ASSOC déjà compilable (à implémenter)
- Pas de fonction de hachage nécessaire

Inconvénients :
- Performance O(n) au lieu de O(1)
- Mais pour un compilateur bootstrap, acceptable

---

## 💡 DÉCISION

**Recommandation** : Commencer par **ASSOC** et **MEMBER**

1. Implémenter ASSOC (1h)
2. Implémenter MEMBER (1h)
3. Refactorer le compilateur pour utiliser alists au lieu de hash-tables (1h)
4. Tester compilation (1h)

**Total** : 4 heures au lieu de 8 heures pour hash tables

Une fois le compilateur bootstrap fonctionnel, on pourra ajouter les hash tables pour l'optimisation.

---

## 📋 CHECKLIST RAPIDE

### Pour Compiler le Compilateur (Version Minimale)

**Absolument nécessaire** :
- [ ] ASSOC
- [ ] MEMBER
- [ ] APPEND (vérifier si déjà supporté)
- [ ] Refactorer compilateur pour éviter hash tables

**Optionnel** (peut contourner) :
- [ ] MAPCAR (peut dérouler à la main)
- [ ] EVERY/SOME (peut remplacer par boucles)
- [ ] Hash tables (peut remplacer par alists)

**Temps estimé version minimale** : 4-5 heures

---

**Prochaine action recommandée** : Implémenter ASSOC et MEMBER
