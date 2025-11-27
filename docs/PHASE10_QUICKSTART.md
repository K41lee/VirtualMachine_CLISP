# 🚀 PHASE 10 : DÉMARRAGE RAPIDE

**Pour ceux qui veulent démarrer immédiatement la Phase 10**

---

## ⚡ Démarrage en 3 commandes

```bash
# 1. Lancer le script de démarrage automatique
./START_PHASE10.sh

# 2. Lire le plan détaillé
cat docs/PHASE10_BOOTSTRAP_PLAN.md | less

# 3. Commencer l'étape 1.1
cat docs/audit-temp.txt
```

---

## 📖 Documents à lire AVANT de coder

### Priorité 1 (OBLIGATOIRE)
1. **PHASE10_BOOTSTRAP_PLAN.md** - Plan complet (19 KB, 15 min de lecture)
   - Concept du bootstrap
   - 6 étapes détaillées
   - Livrables et critères de succès

### Priorité 2 (RECOMMANDÉ)
2. **PHASE10_ROADMAP.md** - Roadmap visuelle (16 KB, 10 min)
   - Checkboxes par sous-tâche
   - Scénario de démonstration
   - Métriques et progression

### Priorité 3 (OPTIONNEL)
3. **PLAN_ACTION_COMPLET.md** - Vue d'ensemble (mis à jour)
   - Contexte Phase 9 complétée
   - Place de Phase 10 dans le projet

---

## 🎯 Étape 1.1 : Audit des dépendances (1h)

**Objectif :** Lister toutes les fonctions LISP natives utilisées par le compilateur

### Actions

1. **Examiner l'audit automatique :**
   ```bash
   cat docs/audit-temp.txt
   ```

2. **Compter les occurrences :**
   ```bash
   cat docs/audit-temp.txt | cut -d: -f3 | sort | uniq -c | sort -rn
   ```

3. **Identifier les fonctions critiques :**
   ```bash
   grep -c 'mapcar\|apply\|funcall' src/compiler.lisp
   grep -c 'remove-if\|find\|assoc' src/compiler.lisp
   grep -c 'gethash\|make-hash-table' src/compiler.lisp
   ```

4. **Créer le document d'audit :**
   ```bash
   # Copier le template ci-dessous dans docs/AUDIT_DEPENDANCES.md
   ```

### Template AUDIT_DEPENDANCES.md

```markdown
# 📊 AUDIT DES DÉPENDANCES - Phase 10

**Date :** 27 novembre 2025  
**Fichier analysé :** src/compiler.lisp (1887 lignes)

## Fonctions LISP natives utilisées

### Catégorie 1 : Manipulation de listes

| Fonction | Occurrences | Priorité | Remplacement |
|----------|-------------|----------|--------------|
| `mapcar` | XX | ⚠️ HAUTE | `my-mapcar` |
| `append` | XX | ⚠️ HAUTE | `my-append` |
| `reverse` | XX | 🟡 MOYENNE | `my-reverse` |
| `remove-if` | XX | 🟡 MOYENNE | `my-remove-if` |
| `find` | XX | 🟡 MOYENNE | `my-find` |

### Catégorie 2 : Structures de données

| Fonction | Occurrences | Priorité | Remplacement |
|----------|-------------|----------|--------------|
| `make-hash-table` | XX | ⚠️ HAUTE | Hash-table LISP pur |
| `gethash` | XX | ⚠️ HAUTE | `my-gethash` |
| `assoc` | XX | 🟢 BASSE | `my-assoc` |

### Catégorie 3 : Fonctions d'ordre supérieur

| Fonction | Occurrences | Priorité | Remplacement |
|----------|-------------|----------|--------------|
| `funcall` | XX | ⚠️ HAUTE | Appel direct |
| `apply` | XX | ⚠️ HAUTE | `my-apply` |

### Catégorie 4 : Autres

| Fonction | Occurrences | Priorité | Remplacement |
|----------|-------------|----------|--------------|
| `format` | XX | 🟢 BASSE | Retirer (debug) |

## Plan de remplacement

### Priorité 1 : CRITIQUE (bloque compilation)
- [ ] `mapcar` → `my-mapcar`
- [ ] `make-hash-table` / `gethash` → Hash-table en LISP pur
- [ ] `apply` → `my-apply`
- [ ] `funcall` → Appels directs

### Priorité 2 : IMPORTANT (peut simplifier)
- [ ] `remove-if` → `my-remove-if`
- [ ] `find` → `my-find`
- [ ] `reverse` → `my-reverse`

### Priorité 3 : OPTIONNEL (peut garder si possible)
- [ ] `format` → Retirer (seulement debug)
- [ ] `assoc` → Peut utiliser version native

## Statistiques

- **Total fonctions natives :** XX occurrences
- **Fonctions uniques :** XX
- **Critiques à remplacer :** XX
- **Estimation temps :** 2-3h pour implémentation

## Prochaine étape

Étape 1.2 : Implémentation des primitives (2h)
```

### Compléter le template

1. Remplacer tous les `XX` par les vraies valeurs
2. Ajouter d'autres fonctions si nécessaire
3. Estimer la complexité de chaque remplacement

---

## 🛠️ Étape 1.2 : Implémentation des primitives (2h)

**Objectif :** Créer `src/primitives.lisp` avec versions réécrites

### Template primitives.lisp

```lisp
;;;; primitives.lisp
;;;; Primitives LISP réécrites pour auto-compilation (Phase 10)

;;; ============================================================================
;;; MANIPULATION DE LISTES
;;; ============================================================================

(defun my-mapcar (fn lst)
  "Version simplifiée de mapcar pour une seule liste"
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (my-mapcar fn (cdr lst)))))

(defun my-append (lst1 lst2)
  "Concaténation de deux listes"
  (if (null lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))

(defun my-reverse (lst)
  "Inversion de liste (tail-recursive)"
  (labels ((rev-aux (lst acc)
             (if (null lst)
                 acc
                 (rev-aux (cdr lst) (cons (car lst) acc)))))
    (rev-aux lst nil)))

(defun my-length (lst)
  "Longueur d'une liste"
  (if (null lst)
      0
      (+ 1 (my-length (cdr lst)))))

(defun my-nth (n lst)
  "N-ième élément d'une liste (0-indexé)"
  (if (= n 0)
      (car lst)
      (my-nth (- n 1) (cdr lst))))

(defun my-remove-if (pred lst)
  "Retirer les éléments satisfaisant pred"
  (cond
    ((null lst) nil)
    ((funcall pred (car lst))
     (my-remove-if pred (cdr lst)))
    (t (cons (car lst)
             (my-remove-if pred (cdr lst))))))

(defun my-find (item lst)
  "Trouver item dans lst"
  (cond
    ((null lst) nil)
    ((equal item (car lst)) item)
    (t (my-find item (cdr lst)))))

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
;;; FONCTIONS D'ORDRE SUPÉRIEUR
;;; ============================================================================

(defun my-apply (fn args)
  "Application d'une fonction à une liste d'arguments"
  ;; Version simplifiée, suppose fn est un symbole
  (eval (cons fn args)))

;;; ============================================================================
;;; TESTS DES PRIMITIVES
;;; ============================================================================

(defun test-primitives ()
  "Teste toutes les primitives"
  (format t "Test my-mapcar: ~A~%" 
          (my-mapcar (lambda (x) (* x 2)) '(1 2 3)))
  ;; Attendu: (2 4 6)
  
  (format t "Test my-append: ~A~%" 
          (my-append '(1 2) '(3 4)))
  ;; Attendu: (1 2 3 4)
  
  (format t "Test my-reverse: ~A~%" 
          (my-reverse '(1 2 3)))
  ;; Attendu: (3 2 1)
  
  (format t "Test my-remove-if: ~A~%" 
          (my-remove-if (lambda (x) (> x 2)) '(1 2 3 4)))
  ;; Attendu: (1 2)
  
  (format t "Test my-assoc: ~A~%" 
          (my-assoc 'b '((a 1) (b 2) (c 3))))
  ;; Attendu: (B 2)
  
  t)

;; Lancer les tests
;; (test-primitives)
```

### Créer et tester

```bash
# Créer le fichier
# (copier le template ci-dessus)

# Tester les primitives
clisp -q -x "(load \"src/primitives.lisp\") (test-primitives)"
```

---

## 🎯 Étape 1.3 : Adaptation compilateur (1-2h)

**Objectif :** Créer `src/compiler-bootstrap.lisp`

### Stratégie

1. **Copier le compilateur original :**
   ```bash
   cp src/compiler.lisp src/compiler-bootstrap.lisp
   ```

2. **Remplacer les appels :**
   ```bash
   # Dans compiler-bootstrap.lisp :
   # Remplacer: mapcar → my-mapcar
   # Remplacer: append → my-append
   # Remplacer: reverse → my-reverse
   # etc.
   ```

3. **Tester la version adaptée :**
   ```lisp
   (load "main.lisp")
   (load "src/primitives.lisp")
   (load "src/compiler-bootstrap.lisp")
   
   ;; Tester compilation simple
   (compile-and-run '(+ 2 3))
   ;; Attendu: 5
   ```

4. **Valider avec les tests :**
   ```bash
   ./run-unit-tests.sh
   # Attendu: 84/84 tests passent toujours
   ```

---

## ✅ Checklist Étape 1 complète

- [ ] **1.1 : Audit terminé**
  - [ ] audit-temp.txt analysé
  - [ ] docs/AUDIT_DEPENDANCES.md créé
  - [ ] Fonctions critiques identifiées

- [ ] **1.2 : Primitives implémentées**
  - [ ] src/primitives.lisp créé
  - [ ] Toutes primitives testées
  - [ ] Tests passent individuellement

- [ ] **1.3 : Compilateur adapté**
  - [ ] src/compiler-bootstrap.lisp créé
  - [ ] Appels natifs remplacés
  - [ ] 84/84 tests passent toujours ✅

**Temps total Étape 1 :** 4-5h  
**Prochaine étape :** Étape 2 - Compilation du loader (3-4h)

---

## 📞 Besoin d'aide ?

### Ressources

- **Plan complet :** `docs/PHASE10_BOOTSTRAP_PLAN.md`
- **Roadmap :** `docs/PHASE10_ROADMAP.md`
- **Todo list :** Gestion intégrée VS Code

### Commandes utiles

```bash
# Voir l'état actuel
git status
git branch

# Voir les fichiers créés
ls -lh docs/PHASE10*.md
ls -lh START_PHASE10.sh

# Lancer les tests
./run-unit-tests.sh

# Compiler un programme simple
clisp -q -x "(load \"main.lisp\") (compile-and-run '(+ 2 3))"
```

---

## 🎉 C'est parti !

**Prochaine action immédiate :** Lancer `./START_PHASE10.sh`

**Temps estimé total :** 19-25 heures sur 6 jours

**Objectif final :** Auto-compilation complète avec point fixe vérifié

**Bonne chance ! 🚀**
