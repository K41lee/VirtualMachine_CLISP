# Système de Délégation FFI (Foreign Function Interface)

## 📚 Vue d'ensemble

Le système de délégation FFI permet à la VM MIPS d'appeler automatiquement des fonctions CLISP pour les opérations qu'elle ne peut pas exécuter nativement. C'est un **pont automatique** entre le code MIPS compilé et l'environnement CLISP hôte.

## 🎯 Objectif

**Problème** : Le compilateur bootstrappé a besoin de fonctions de base (listes, I/O, strings) qui ne sont pas implémentées en MIPS.

**Solution** : Déléguer automatiquement ces appels à CLISP, qui les exécute et retourne le résultat à la VM.

```
┌─────────────────────────────────────────────────────────┐
│  Code MIPS dans la VM                                   │
│                                                          │
│  (JAL READ-EXPRESSION-FROM-VM)  ← symbole inconnu      │
│         │                                                │
│         ↓                                                │
│  ❓ VM ne trouve pas cette fonction en MIPS             │
│         │                                                │
│         ↓                                                │
│  ✅ Détecte que c'est un symbole + vm-can-delegate     │
│         │                                                │
│         ↓                                                │
│  ⚡ Appelle la fonction CLISP avec les args ($A0-$A3)  │
│         │                                                │
│         ↓                                                │
│  📦 CLISP exécute et retourne le résultat               │
│         │                                                │
│         ↓                                                │
│  💾 Résultat placé dans $V0                             │
│         │                                                │
│         ↓                                                │
│  ▶️  Exécution continue normalement                     │
└─────────────────────────────────────────────────────────┘
```

## 🛠️ Implémentation

### 1. Variables globales (vm.lisp)

```lisp
(defparameter *vm-delegate-to-lisp* t
  "Active/désactive la délégation CLISP")

(defparameter *vm-delegated-functions* (make-hash-table)
  "Table: symbole → fonction CLISP")

(defparameter *vm-delegation-stats* (make-hash-table :test 'equal)
  "Statistiques: nom-fonction → compteur")
```

### 2. API d'enregistrement

```lisp
(vm-register-delegate "MY-FUNC" #'my-func)
  "Enregistre une fonction CLISP pour délégation"

(vm-can-delegate 'MY-FUNC)
  "Vérifie si une fonction peut être déléguée"
  "Retourne T si: fonction enregistrée OU fboundp"

(vm-delegate-call vm 'MY-FUNC '(arg1 arg2 ...))
  "Exécute la fonction CLISP et retourne le résultat"
```

### 3. Modification de l'instruction JAL

**Avant** (vm.lisp ligne ~761):
```lisp
(:JAL (let ((label (first args)))
        ;; Saut direct à l'adresse
        (set-register vm pc-reg (+ code-start label))))
```

**Après** (avec délégation):
```lisp
(:JAL (let ((label (first args)))
        ;; Cas 1: Label est un symbole et peut être délégué
        (when (and (symbolp label) (vm-can-delegate label))
          ;; Extraire arguments depuis $A0-$A3
          (let* ((a0 (get-register vm (get-reg :a0)))
                 (a1 (get-register vm (get-reg :a1)))
                 (a2 (get-register vm (get-reg :a2)))
                 (a3 (get-register vm (get-reg :a3)))
                 (args-list (list a0 a1 a2 a3))
                 (result (vm-delegate-call vm label args-list)))
            ;; Résultat dans $V0
            (set-register vm (get-reg :v0) result)
            ;; Continue à l'instruction suivante
            (set-register vm pc-reg (1+ (get-register vm pc-reg)))
            (return-from execute-instruction)))
        
        ;; Cas 2: Adresse numérique → saut MIPS normal
        (set-register vm pc-reg (+ code-start label))))
```

## 📊 Fonctionnement détaillé

### Étape 1: Enregistrement des fonctions

```lisp
;; Fonctions arithmétiques
(vm-register-delegate "+" #'+)
(vm-register-delegate "-" #'-)
(vm-register-delegate "*" #'*)
(vm-register-delegate "/" #'/)

;; Fonctions de listes
(vm-register-delegate "CAR" #'car)
(vm-register-delegate "CDR" #'cdr)
(vm-register-delegate "CONS" #'cons)

;; Fonctions personnalisées
(defun my-log (x)
  (format t "[LOG] ~A~%" x)
  x)
(vm-register-delegate "MY-LOG" #'my-log)
```

### Étape 2: Compilation du code

```lisp
(defparameter *code*
  '(defun test (n)
     (my-log n)  ; ← Cette fonction sera déléguée
     (* n 2)))

(defparameter *compiled*
  (compile-lisp-to-mips-simplified *code*))

;; Génère (entre autres):
;; (JAL MY-LOG)  ; ← Symbole, pas adresse numérique
```

### Étape 3: Exécution avec délégation

```lisp
(defparameter *vm* (make-new-vm))
(load-code *vm* *compiled*)

;; Lors de l'exécution de (JAL MY-LOG):
;; 1. VM détecte symbole MY-LOG
;; 2. Vérifie *vm-delegated-functions*
;; 3. Trouve #'my-log
;; 4. Extrait args: $A0=42, $A1=0, $A2=0, $A3=0
;; 5. Appelle (my-log 42 0 0 0)
;; 6. [LOG] 42  ← Affichage CLISP
;; 7. Retourne 42
;; 8. Place 42 dans $V0
;; 9. Continue l'exécution

(call-function *vm* 'TEST 42)
;; → [LOG] 42
;; → 84
```

## ⚠️ Limitations actuelles

### 1. **Passage d'arguments**

**Problème** : La VM passe toujours 4 arguments ($A0-$A3) même si la fonction n'en attend qu'un.

**Solution actuelle** : Fonctions CLISP acceptent paramètres optionnels :
```lisp
(defun my-func (required &optional opt1 opt2 opt3)
  (declare (ignore opt1 opt2 opt3))
  ...)
```

**Solution future** : Encoder le nombre d'arguments dans le code MIPS.

### 2. **Retour de valeurs complexes**

**Problème** : $V0 ne contient qu'un entier. Comment retourner :
- Listes ?
- Chaînes ?
- Structures ?

**Solution actuelle** : Utiliser `*vm-lisp-objects*` avec handles :
```lisp
(defun my-func-returning-list (...)
  (let ((result '(1 2 3))
        (handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) result)
    handle))  ; ← Retourne le handle dans $V0
```

**Solution future** : Marshalling automatique.

### 3. **Appels récursifs entre VM et CLISP**

**Exemple problématique** :
```lisp
;; Fonction CLISP qui appelle une fonction VM
(defun clisp-func ()
  (call-function *vm* 'VM-FUNC))  ; ← VM-FUNC appelle CLISP-FUNC

;; → Boucle infinie potentielle
```

**Solution** : Tracer les appels et détecter les cycles.

### 4. **Performance**

**Coût de la délégation** :
- Vérification symbole : ~1µs
- Extraction arguments : ~1µs  
- Appel CLISP : ~10-100µs
- **Total : ~12-102µs par appel**

**Comparaison** :
- Appel MIPS natif : ~0.1µs
- **→ 100-1000x plus lent**

**Quand utiliser** :
- ✅ Bootstrap et développement
- ✅ Fonctions I/O (déjà lentes)
- ❌ Boucles internes (optimiser en MIPS)

## 🎯 Cas d'utilisation

### 1. **Compilation bootstrappée**

```lisp
;; Le compilateur a besoin de fonctions de construction
(vm-register-delegate "READ-EXPRESSION-FROM-VM" 
                      #'read-expression-from-vm)
(vm-register-delegate "CONVERT-KEYWORDS-TO-SYMBOLS"
                      #'convert-keywords-to-symbols)
(vm-register-delegate "COMPILE-LISP-TO-MIPS-SIMPLIFIED"
                      #'compile-lisp-to-mips-simplified)

;; Maintenant le compilateur MIPS peut compiler du code !
(call-function *compiler-vm* 'COMPILE-FROM-HANDLE handle)
;; → Appelle les fonctions CLISP pour construire le code
;; → Retourne le code MIPS compilé
```

### 2. **Fonctions I/O**

```lisp
(vm-register-delegate "PRINT" 
  (lambda (obj &rest ignore)
    (declare (ignore ignore))
    (print obj)
    obj))

(vm-register-delegate "READ" 
  (lambda (&rest ignore)
    (declare (ignore ignore))
    (read)))

;; Code Lisp peut faire de l'I/O
'(defun interactive ()
   (print "Entrez un nombre: ")
   (let ((n (read)))
     (print (* n n))))
```

### 3. **Fonctions de listes avancées**

```lisp
(vm-register-delegate "MAPCAR" #'mapcar)
(vm-register-delegate "FILTER" #'remove-if-not)
(vm-register-delegate "REDUCE" #'reduce)

;; Code fonctionnel sans implémenter tout en MIPS
'(defun process-list (lst)
   (mapcar (lambda (x) (* x 2)) lst))
```

## 📈 Statistiques et débogage

### Activer le verbose

```lisp
(setf (vm-verbose *vm*) t)
(call-function *vm* 'TEST 42)

;; Affiche:
;;   [10480760] Exécution: (JAL MY-SQUARE)
;;   ⚡ DÉLÉGATION à CLISP: MY-SQUARE(5, 0, 0, 0)
;;   ⚡ RÉSULTAT CLISP: 25
;;   [10480761] Exécution: (MOVE $V0 $T0)
```

### Consulter les statistiques

```lisp
(vm-show-delegation-stats)

;; Affiche:
;; Statistiques de délégation CLISP:
;; ──────────────────────────────────
;;   MY-SQUARE: 5 appels
;;   READ-EXPRESSION-FROM-VM: 12 appels
;;   CONVERT-KEYWORDS-TO-SYMBOLS: 12 appels
;; ──────────────────────────────────
;; Total: 29 appels délégués
```

### Réinitialiser les stats

```lisp
(vm-reset-delegation-stats)
```

## 🚀 Évolutions futures

### 1. **Marshalling automatique**

```lisp
;; Convertir automatiquement entre formats
(defun vm-marshal (value)
  "Convertit valeur Lisp → handle VM si nécessaire"
  (cond
    ((numberp value) value)          ; Nombres directs
    ((listp value) (store-list-in-vm value))  ; Listes → handles
    ((stringp value) (store-string-in-vm value))
    ...))

(defun vm-unmarshal (handle)
  "Convertit handle VM → valeur Lisp"
  (if (< handle 1000)
      handle  ; Nombre direct
      (gethash handle *vm-lisp-objects*)))  ; Handle → objet
```

### 2. **Signature de fonctions**

```lisp
;; Déclarer le nombre d'arguments
(vm-register-delegate "MY-FUNC" #'my-func :args 2)

;; Lors du JAL, extraire seulement les bons registres
(case arg-count
  (1 (list (get-register vm :a0)))
  (2 (list (get-register vm :a0) (get-register vm :a1)))
  ...)
```

### 3. **Cache de code JIT**

```lisp
;; Compiler les fonctions déléguées en MIPS à la volée
(defparameter *jit-cache* (make-hash-table))

(defun vm-delegate-call (vm func args)
  ;; Première fois: compiler la fonction CLISP en MIPS
  (unless (gethash func *jit-cache*)
    (setf (gethash func *jit-cache*)
          (compile-lisp-to-mips-simplified 
            `(defun ,func ,args ...))))
  
  ;; Ensuite: appeler la version MIPS
  (call-function vm func args))
```

### 4. **Mode debugging avec traçage**

```lisp
(setf *vm-delegate-trace* t)

;; Affiche un arbre d'appels:
;; ┌─ MAIN
;; │  ├─ MY-FUNC (délégué CLISP)
;; │  │  └─ HELPER (délégué CLISP)
;; │  └─ PROCESS (natif MIPS)
;; └─ END
```

## 💡 Résumé

Le système FFI permet de :

✅ **Utiliser le compilateur bootstrappé** sans implémenter toutes les fonctions en MIPS

✅ **Développement progressif** : commencer avec délégation, optimiser en MIPS ensuite

✅ **Débogage facile** : voir quelles fonctions sont vraiment appelées

✅ **Flexibilité** : ajouter de nouvelles fonctions sans recompiler la VM

⚠️ **Attention** : Performance réduite (100-1000x plus lent)

🎯 **Idéal pour** : Bootstrap, prototypage, I/O, fonctions rares

❌ **Éviter pour** : Boucles internes, calculs intensifs

---

**Fichiers modifiés** :
- [`src/vm.lisp`](src/vm.lisp) : Système FFI et modification JAL
- [`exec-code-bootstrap.lisp`](exec-code-bootstrap.lisp) : Exemple d'utilisation
- [`demo-delegation.lisp`](demo-delegation.lisp) : Démonstration complète

**Prochaine étape** : Implémenter le marshalling pour structures complexes.
