# 📊 AUDIT DES DÉPENDANCES - Phase 10 Bootstrap

**Date :** 27 novembre 2025  
**Fichier analysé :** `src/compiler.lisp` (1887 lignes)  
**Objectif :** Identifier les dépendances LISP natives pour auto-compilation

---

## 🔍 RÉSUMÉ EXÉCUTIF

**Total occurrences :** 21 utilisations de fonctions natives  
**Fonctions uniques :** 3 types (format, assoc, mapcar)  
**Criticité :** 🟢 BASSE - Peu de dépendances externes  
**Complexité remplacement :** ★★☆☆☆ (Facile à moyenne)

---

## 📋 FONCTIONS NATIVES IDENTIFIÉES

### Catégorie 1 : Affichage et Debug (format)

| Fonction | Occurrences | Priorité | Criticité | Remplacement |
|----------|-------------|----------|-----------|--------------|
| `format` | 13 | 🟢 BASSE | Debug uniquement | Retirer ou simplifier |

**Détails :**
- **Lignes :** 78, 141, 346, 1353, 1868-1886
- **Usage :** Principalement pour debug et messages utilisateur
- **Action :** Peut être **retiré complètement** pour version bootstrap
- **Impact :** Aucun sur fonctionnalité de compilation

**Exemples d'usage :**
```lisp
;; Ligne 78 : Génération de labels
(format nil "~A_~A" prefix (car counter))

;; Ligne 141 : Génération nom registre
(format nil ":$T~A" reg-num)

;; Lignes 1868-1886 : Messages de debug (à retirer)
(format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
```

**Stratégie :**
1. Garder `format nil` pour génération de strings (lignes 78, 141, 1353)
2. Retirer tous les `format t` (lignes 1868-1886)
3. Alternative : `concatenate 'string` ou `princ-to-string`

---

### Catégorie 2 : Recherche dans listes d'association (assoc)

| Fonction | Occurrences | Priorité | Criticité | Remplacement |
|----------|-------------|----------|-----------|--------------|
| `assoc` | 5 | 🟡 MOYENNE | Fonctionnel | `my-assoc` (simple) |

**Détails :**
- **Lignes :** 88, 96, 105, 178, 1367
- **Usage :** Recherche dans environnements (variables, fonctions)
- **Action :** Implémenter `my-assoc` en LISP pur
- **Impact :** Critique pour environnements de compilation

**Exemples d'usage :**
```lisp
;; Ligne 88 : Recherche variable dans environnement
(cdr (assoc var (compiler-env-variables env)))

;; Ligne 96 : Recherche fonction
(cdr (assoc fn-name (compiler-env-functions env)))

;; Ligne 1367 : Recherche info fonction locale
(cdr (assoc fn-name fn-infos))
```

**Implémentation requise :**
```lisp
(defun my-assoc (key alist)
  "Chercher key dans alist"
  (cond
    ((null alist) nil)
    ((equal key (caar alist)) (car alist))
    (t (my-assoc key (cdr alist)))))
```

**Complexité :** O(n) - Identique à assoc natif  
**Test requis :** ✅ Vérifier avec environnements de compilation

---

### Catégorie 3 : Transformation de listes (mapcar)

| Fonction | Occurrences | Priorité | Criticité | Remplacement |
|----------|-------------|----------|-----------|--------------|
| `mapcar` | 3 | ⚠️ HAUTE | Fonctionnel | `my-mapcar` (récursif) |

**Détails :**
- **Lignes :** 408 (2 fois), 427
- **Usage :** Extraction de variables/valeurs dans LET et LABELS
- **Action :** Implémenter `my-mapcar` en LISP pur
- **Impact :** Critique pour LET et LABELS

**Exemples d'usage :**
```lisp
;; Ligne 408 : Extraction variables et valeurs de LET
(let-vars (mapcar #'first bindings))   ; Noms variables
(let-vals (mapcar #'second bindings))  ; Valeurs initiales

;; Ligne 427 : Extraction noms de fonctions LABELS
(func-names (mapcar #'first definitions))
```

**Implémentation requise :**
```lisp
(defun my-mapcar (fn lst)
  "Version simplifiée de mapcar pour une seule liste"
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (my-mapcar fn (cdr lst)))))
```

**Note importante :** Utilise `funcall` qui est lui-même une fonction native.  
**Solution :** Les lambda/functions peuvent être appelées directement en Common Lisp.

**Alternative sans funcall :**
```lisp
;; Au lieu de:
(mapcar #'first bindings)

;; Écrire directement:
(labels ((extract-first (lst)
           (if (null lst)
               nil
               (cons (car (car lst))
                     (extract-first (cdr lst))))))
  (extract-first bindings))
```

**Complexité :** O(n) - Parcours simple  
**Test requis :** ✅ Vérifier avec LET et LABELS

---

## 🎯 PLAN DE REMPLACEMENT

### Phase 1 : Primitives essentielles (1h)

**Fichier :** `src/primitives.lisp`

```lisp
;;;; primitives.lisp
;;;; Primitives LISP pour bootstrap Phase 10

;;; ============================================================================
;;; ASSOCIATION LISTS
;;; ============================================================================

(defun my-assoc (key alist)
  "Chercher key dans alist"
  (cond
    ((null alist) nil)
    ((equal key (caar alist)) (car alist))
    (t (my-assoc key (cdr alist)))))

;;; ============================================================================
;;; TRANSFORMATION DE LISTES
;;; ============================================================================

(defun my-mapcar (fn lst)
  "Version simplifiée de mapcar pour une seule liste"
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (my-mapcar fn (cdr lst)))))

;; Alternative sans funcall (plus verbeux mais plus portable)
(defun my-map-first (lst)
  "Extrait le premier élément de chaque sous-liste"
  (if (null lst)
      nil
      (cons (car (car lst))
            (my-map-first (cdr lst)))))

(defun my-map-second (lst)
  "Extrait le second élément de chaque sous-liste"
  (if (null lst)
      nil
      (cons (car (cdr (car lst)))
            (my-map-second (cdr lst)))))

;;; ============================================================================
;;; GÉNÉRATION DE STRINGS (remplace format simple)
;;; ============================================================================

(defun my-concat-string (&rest strings)
  "Concaténation de strings"
  (apply #'concatenate 'string strings))

(defun my-int-to-string (n)
  "Convertit un entier en string"
  (princ-to-string n))

(defun my-format-label (prefix counter)
  "Génère un label (remplace format nil \"~A_~A\" ...)"
  (concatenate 'string 
               (string prefix) 
               "_" 
               (princ-to-string counter)))
```

**Tests :**
```lisp
;; Test my-assoc
(assert (equal (my-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
(assert (null (my-assoc 'd '((a 1) (b 2) (c 3)))))

;; Test my-mapcar
(assert (equal (my-mapcar #'car '((a 1) (b 2) (c 3))) '(a b c)))

;; Test alternatives
(assert (equal (my-map-first '((a 1) (b 2))) '(a b)))
(assert (equal (my-map-second '((a 1) (b 2))) '(1 2)))
```

---

### Phase 2 : Adaptation du compilateur (1-2h)

**Fichier :** `src/compiler-bootstrap.lisp`

#### Étape 2.1 : Copier le compilateur
```bash
cp src/compiler.lisp src/compiler-bootstrap.lisp
```

#### Étape 2.2 : Charger les primitives
```lisp
;; En début de compiler-bootstrap.lisp (après les commentaires)
(load "src/primitives.lisp")
```

#### Étape 2.3 : Remplacements à effectuer

**1. Remplacer `assoc` par `my-assoc` (5 occurrences) :**

```lisp
;; Ligne 88 : AVANT
(cdr (assoc var (compiler-env-variables env)))

;; APRÈS
(cdr (my-assoc var (compiler-env-variables env)))

;; Même chose pour lignes 96, 105, 178, 1367
```

**2. Remplacer `mapcar` (3 occurrences) :**

Option A - Avec my-mapcar :
```lisp
;; Ligne 408 : AVANT
(let-vars (mapcar #'first bindings))
(let-vals (mapcar #'second bindings))

;; APRÈS
(let-vars (my-mapcar #'first bindings))
(let-vals (my-mapcar #'second bindings))
```

Option B - Sans funcall (plus robuste) :
```lisp
;; APRÈS (alternative)
(let-vars (my-map-first bindings))
(let-vals (my-map-second bindings))
```

**3. Simplifier ou retirer `format` :**

```lisp
;; Ligne 78 : Génération label
;; AVANT
(label (intern (format nil "~A_~A" prefix (car counter))))

;; APRÈS
(label (intern (my-format-label prefix (car counter))))

;; Lignes 1868-1886 : Debug
;; AVANT
(format t "~%=== CODE ASSEMBLEUR GÉNÉRÉ ===~%")
(dolist (instr asm)
  (format t "~A~%" instr))

;; APRÈS (optionnel, peut être complètement retiré)
(when *debug-bootstrap*
  (princ "=== CODE ASSEMBLEUR GÉNÉRÉ ===")
  (terpri)
  (dolist (instr asm)
    (print instr)))
```

---

## 📊 STATISTIQUES FINALES

| Métrique | Valeur | Statut |
|----------|--------|--------|
| **Total dépendances** | 21 occurrences | 🟢 Peu |
| **Fonctions uniques** | 3 types | 🟢 Simple |
| **Critiques (mapcar, assoc)** | 8 | 🟡 Gérable |
| **Non-critiques (format)** | 13 | 🟢 Facile |
| **Fonctions à implémenter** | 2-3 | 🟢 Simple |
| **Temps implémentation** | 2-3h | 🟢 Rapide |
| **Complexité** | ★★☆☆☆ | 🟢 Facile |

---

## ✅ CHECKLIST ÉTAPE 1.1 COMPLÉTÉE

- [x] **Audit automatique effectué**
  - Script START_PHASE10.sh exécuté
  - 21 occurrences identifiées dans audit-temp.txt

- [x] **Analyse détaillée réalisée**
  - 3 fonctions natives : format (13), assoc (5), mapcar (3)
  - Toutes les lignes localisées et documentées
  - Criticité évaluée : BASSE à MOYENNE

- [x] **Document AUDIT_DEPENDANCES.md créé**
  - Analyse complète de chaque fonction
  - Stratégies de remplacement définies
  - Code d'implémentation fourni
  - Tests de validation spécifiés

---

## 🚀 PROCHAINE ÉTAPE

**Étape 1.2 : Implémentation des primitives (2h)**

**Actions immédiates :**
1. Créer `src/primitives.lisp` avec le code fourni ci-dessus
2. Tester chaque primitive isolément
3. Valider avec assertions

**Commande de démarrage :**
```bash
# Créer le fichier
touch src/primitives.lisp

# Copier le code des primitives (voir section "Phase 1" ci-dessus)

# Tester les primitives
clisp -q -x "(load \"src/primitives.lisp\") \
  (assert (equal (my-assoc 'b '((a 1) (b 2))) '(b 2))) \
  (format t \"✅ Primitives testées avec succès!~%\")"
```

---

## 📝 NOTES IMPORTANTES

### Fonctions natives NON trouvées (bon signe !) ✅

Les fonctions suivantes ne sont **PAS** utilisées dans le compilateur :
- `apply` ❌ (0 occurrences)
- `funcall` ❌ (0 occurrences directes, seulement dans mapcar)
- `remove-if` ❌ (0 occurrences)
- `find` ❌ (0 occurrences)
- `gethash` / `make-hash-table` ❌ (0 occurrences)

**Conclusion :** Le compilateur est déjà très "pur" ! Seulement 3 types de fonctions à remplacer.

### Complexité réelle vs estimée

**Estimation initiale :** 4-5h pour l'étape 1  
**Réalité :** ~2-3h suffisent vu le faible nombre de dépendances

**Gain de temps :** 1-2h (peut être réinvesti dans tests ou étapes suivantes)

---

## 🎯 IMPACT SUR LE BOOTSTRAP

**Facilité d'auto-compilation :** ⭐⭐⭐⭐⭐ (5/5)

Le compilateur a très peu de dépendances externes, ce qui rend l'auto-compilation:
1. **Plus facile** que prévu
2. **Plus rapide** à implémenter
3. **Plus robuste** (moins de points de défaillance)

**Risques identifiés :** 🟢 FAIBLES
- Implémentations de primitives simples
- Peu de cas limites à gérer
- Tests faciles à écrire

---

**Document créé le :** 27 novembre 2025  
**Temps d'audit :** ~1h  
**Auteur :** Analyse automatique + revue manuelle  
**Status :** ✅ ÉTAPE 1.1 TERMINÉE

**Prochaine action :** Créer `src/primitives.lisp` (Étape 1.2)
