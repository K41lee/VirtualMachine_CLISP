# Machine Virtuelle MIPS en Common Lisp

## 📖 Description

Ce projet implémente une **machine virtuelle MIPS** complète avec un **compilateur Lisp → MIPS** et un système de **bootstrap auto-hébergé**. Le compilateur peut se compiler lui-même et s'exécuter dans la VM !

### 🎯 Caractéristiques principales

- **VM MIPS complète** : Interprète d'instructions MIPS avec 10 Mo de mémoire
- **Compilateur Lisp → MIPS** : Transforme des fonctions Lisp en code machine MIPS
- **Bootstrap complet** : Le compilateur peut se compiler lui-même dans la VM
- **FFI (Foreign Function Interface)** : Délégation d'appels de la VM vers Lisp natif
- **Marshalling** : Retour de structures complexes (listes) depuis la VM

---

## 🚀 Démarrage rapide

### Prérequis

- **CLISP** (ou autre implémentation Common Lisp)

### Utilisation

Il existe **4 programmes principaux** que vous pouvez exécuter :

```bash
# 1. Compiler et exécuter une fonction avec le compilateur natif
clisp exec-code.lisp

# 2. Afficher le code MIPS généré (compilateur natif)
clisp show-compile.lisp

# 3. Compiler et exécuter avec le compilateur bootstrappé dans la VM
clisp exec-code-bootstrap.lisp

# 4. Afficher le code MIPS généré par le compilateur bootstrappé
clisp show-compile-bootstrap.lisp
```

### Tester une autre fonction

**Toutes les fonctions sont définies dans un seul fichier : `code.lisp`**

1. Éditez `code.lisp`
2. Modifiez les paramètres :
   - `*function-definition*` : la fonction à compiler
   - `*function-name*` : nom pour l'exécution
   - `*function-args*` : arguments de test
   - `*expected-result*` : résultat attendu
3. Relancez n'importe quel programme (`exec-code.lisp`, `show-compile.lisp`, etc.)

**Exemple** :

```lisp
(defparameter *function-definition*
  '(defun fact (n)
     (if (<= n 1)
         1
         (* n (fact (- n 1))))))

(defparameter *function-name* 'FACT)
(defparameter *function-args* '(10))
(defparameter *expected-result* 3628800)
```

Des exemples de fonctions (fibonacci, factorielle, ackermann, etc.) sont inclus en commentaires dans `code.lisp`.

---

## 📁 Structure du projet

### Fichiers principaux (racine)

| Fichier | Description | Usage |
|---------|-------------|-------|
| `code.lisp` | **Définition centralisée** de la fonction à compiler | Modifiez ce fichier pour tester d'autres fonctions |
| `exec-code.lisp` | Compile et **exécute** une fonction (compilateur natif) | `clisp exec-code.lisp` |
| `exec-code-bootstrap.lisp` | Compile et exécute avec le **compilateur bootstrappé** | `clisp exec-code-bootstrap.lisp` |
| `show-compile.lisp` | **Affiche le code MIPS** généré (compilateur natif) | `clisp show-compile.lisp` |
| `show-compile-bootstrap.lisp` | Affiche le code MIPS du **compilateur bootstrappé** | `clisp show-compile-bootstrap.lisp` |

### Dossier `src/` - Composants du système

| Fichier | Lignes | Description |
|---------|--------|-------------|
| `vm.lisp` | ~1480 | **Machine virtuelle MIPS** : interpréteur d'instructions, gestion mémoire, registres, FFI |
| `compiler-simplified.lisp` | ~1000 | **Compilateur Lisp → MIPS** : analyse, génération de code, optimisations |
| `loader.lisp` | ~250 | **Chargeur de code** : charge les instructions MIPS en mémoire VM |
| `asm-ops.lisp` | ~600 | **Définitions MIPS** : opcodes, registres, opérations assembleur |
| `utils-bootstrap.lisp` | ~230 | **Utilitaires bootstrap** : fonctions pour compiler le compilateur dans la VM |
| `utils.lisp` | ~300 | **Utilitaires généraux** : fonctions auxiliaires, affichage, debug |

**Total : ~3800 lignes de code**

---

## 🔧 Architecture détaillée

### Principe de fonctionnement

```
┌─────────────┐
│  code.lisp  │ ← Vous modifiez uniquement ce fichier
└──────┬──────┘
       │
       ├─────────────────────────────────────────────────────┐
       │                                                     │
       v                                                     v
┌────────────────────┐                          ┌──────────────────────────┐
│  Compilation       │                          │  Compilation             │
│  Native            │                          │  Bootstrappée            │
├────────────────────┤                          ├──────────────────────────┤
│ compiler-          │                          │ 1. Compiler le           │
│ simplified.lisp    │                          │    compilateur en MIPS   │
│ (natif Lisp)       │                          │ 2. Charger dans VM0      │
│                    │                          │ 3. Exécuter VM0 pour     │
│ ↓                  │                          │    compiler la fonction  │
│ Instructions MIPS  │                          │ 4. Code MIPS résultant   │
└────────┬───────────┘                          └──────────┬───────────────┘
         │                                                 │
         v                                                 v
┌────────────────────┐                          ┌──────────────────────────┐
│  vm.lisp           │                          │  vm.lisp                 │
│  Exécute le code   │                          │  VM0 compile             │
│  MIPS généré       │                          │  puis exécute            │
└────────────────────┘                          └──────────────────────────┘
```

### Flux d'exécution : `exec-code.lisp`

1. **Chargement** : Charge `code.lisp` (fonction à compiler)
2. **Compilation** : `compiler-simplified.lisp` génère du code MIPS
3. **Chargement VM** : Les instructions MIPS sont chargées en mémoire
4. **Exécution** : La VM exécute le code et retourne le résultat

### Flux d'exécution : `exec-code-bootstrap.lisp`

1. **Meta-compilation** : Compile le compilateur lui-même en MIPS (~2600 instructions)
2. **Chargement VM0** : Charge le compilateur compilé dans une VM
3. **Construction** : Construit l'expression à compiler en mémoire VM
4. **Compilation dans VM** : Le compilateur (en MIPS) compile la fonction
5. **Extraction** : Récupère le code généré via marshalling
6. **Exécution VM1** : Charge et exécute le code dans une nouvelle VM

**Résultat** : Le code a été compilé par un compilateur s'exécutant dans la VM !

---

## 🧩 Fonctionnalités avancées

### FFI (Foreign Function Interface)

La VM peut **déléguer** l'exécution de fonctions inconnues au Lisp natif :

```lisp
;; Dans la VM MIPS :
JAL print-debug  ; La VM ne connaît pas cette fonction

;; Le système FFI intercepte l'appel et :
;; 1. Détecte que "print-debug" est un symbole
;; 2. Cherche la fonction Lisp native correspondante
;; 3. Exécute la fonction native avec les arguments de la VM
;; 4. Retourne le résultat dans $V0
```

### Marshalling

Le système de **marshalling** permet de retourner des structures complexes (listes) depuis la VM :

```lisp
;; Problème : $V0 ne peut contenir qu'un entier, pas une liste

;; Solution : Handles
(defun compile-from-handle-with-handle-return (handle)
  (let ((code (compile-from-handle handle)))  ; Génère une liste
    (vm-store-lisp-object code)))             ; Retourne un handle (entier)

;; Récupération :
(let ((result-handle (vm-call-function ...)))
  (vm-get-lisp-object result-handle))  ; Récupère la liste depuis le handle
```

---

## 📊 Exemples de sortie

### `exec-code.lisp` - Fibonacci(20)

```
════════════════════════════════════════════════════════════════
ÉTAPE 4: Résultat
════════════════════════════════════════════════════════════════

Résultat: fibo(20) = 6765
✓ Exécution terminée en 11.532 secondes
```

### `show-compile.lisp` - Affichage du code MIPS

```
════════════════════════════════════════════════════════════════
CODE MIPS GÉNÉRÉ POUR: FIBO
════════════════════════════════════════════════════════════════

Total instructions: 79

Analyse du code:
  ├─ Sauts/Branches:   8
  ├─ Arithmétique:     17
  ├─ Loads (LW):       18
  ├─ Stores (SW):      15
  └─ Autres:           15
```

### `exec-code-bootstrap.lisp` - Bootstrap complet

```
════════════════════════════════════════════════════════════════
ÉTAPE 6: Résultat et statistiques
════════════════════════════════════════════════════════════════

Résultat: fibo(20) = 6765
✓ RÉSULTAT CORRECT! (attendu: 6765)

Statistiques de délégation CLISP:
──────────────────────────────────
Total: 0 appels délégués
```

### `show-compile-bootstrap.lisp` - Vérification

```
════════════════════════════════════════════════════════════════
RÉSUMÉ:
════════════════════════════════════════════════════════════════

✓ Compilateur compilé et chargé (2598 instructions)
✓ Expression Fibonacci construite en mémoire (handle 1027)
✓ Compilation avec marshalling (handle → liste)
✓ Handle résultat: 5003 → Liste de 79 instructions
✅ Code identique au compilateur natif
```

---

## 🎓 Comprendre le code

### Anatomie de `code.lisp`

```lisp
;;;; 1. DÉFINITION DE LA FONCTION (obligatoire)
(defparameter *function-definition*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

;;;; 2. PARAMÈTRES D'EXÉCUTION (pour exec-code.lisp et exec-code-bootstrap.lisp)
(defparameter *function-name* 'FIBO)          ; Nom de la fonction
(defparameter *function-args* '(20))          ; Arguments à passer
(defparameter *expected-result* 6765)         ; Résultat attendu (validation)
```

**Utilisé par** :
- `show-compile.lisp` : utilise `*function-definition*` pour compiler
- `show-compile-bootstrap.lisp` : idem
- `exec-code.lisp` : utilise tout (compile + exécute + vérifie)
- `exec-code-bootstrap.lisp` : idem

### Comprendre les fichiers `exec-*` vs `show-*`

| Type | Fonction | Compilateur | Sortie |
|------|----------|-------------|--------|
| `exec-code.lisp` | Compile + **Exécute** | Natif | Résultat du calcul |
| `show-compile.lisp` | Compile + **Affiche** | Natif | Instructions MIPS |
| `exec-code-bootstrap.lisp` | Compile + **Exécute** | **Bootstrappé** (dans VM) | Résultat du calcul |
| `show-compile-bootstrap.lisp` | Compile + **Affiche** | **Bootstrappé** (dans VM) | Instructions MIPS + vérification |

**Natif** = Le compilateur Lisp s'exécute directement en Lisp  
**Bootstrappé** = Le compilateur est d'abord compilé en MIPS et s'exécute dans la VM

---

## 🔍 Détail des fichiers `src/`

### `vm.lisp` - La Machine Virtuelle

**Responsabilités** :
- Interpréter les instructions MIPS (ADD, SUB, LW, SW, JAL, BEQ, etc.)
- Gérer 32 registres ($0-$31) + PC + HI/LO
- Gérer la mémoire (10 Mo divisée en segments : code, données, pile, tas)
- Implémenter le FFI (délégation vers Lisp natif)
- Système de marshalling (handles pour structures complexes)

**Structures principales** :
```lisp
(defstruct vm
  memory          ; Vecteur de 10M mots
  registers       ; 32 registres MIPS
  pc              ; Program Counter
  running         ; État d'exécution
  ...)
```

**Instructions supportées** : ADD, ADDI, SUB, MUL, DIV, AND, OR, XOR, SLT, LW, SW, BEQ, BNE, J, JAL, JR, SYSCALL

### `compiler-simplified.lisp` - Le Compilateur

**Responsabilités** :
- Analyser le code Lisp (AST)
- Gérer les environnements (variables locales, portée)
- Générer du code MIPS optimisé
- Compiler : `if`, `let`, `defun`, appels de fonction, récursivité

**Structures principales** :
```lisp
(defstruct compiler-env
  bindings        ; Variables locales
  parent          ; Environnement parent (pour portée)
  frame-size      ; Taille du frame de pile
  ...)
```

**Processus de compilation** :
1. **Analyse** : Parcourt l'AST Lisp
2. **Génération** : Produit des instructions MIPS
3. **Optimisation** : Allocation registres, gestion pile
4. **Linking** : Résout les labels et adresses

### `loader.lisp` - Le Chargeur

**Responsabilités** :
- Charger le code MIPS en mémoire VM
- Résoudre les adresses et labels
- Initialiser le compteur programme (PC)

**Fonction principale** :
```lisp
(defun load-code (vm code)
  "Charge une liste d'instructions MIPS dans la mémoire de la VM"
  ...)
```

### `asm-ops.lisp` - Définitions MIPS

**Contenu** :
- Définition des 32 registres MIPS ($0-$31, $sp, $ra, $v0, etc.)
- Opcodes des instructions (ADD=32, SUB=34, LW=35, SW=43, etc.)
- Fonctions de création d'instructions (make-add, make-lw, make-sw, etc.)

**Exemple** :
```lisp
(defparameter $sp 29 "Stack pointer")
(defparameter $ra 31 "Return address")
(defparameter $v0 2  "Return value")

(defun make-add (rd rs rt)
  "Crée instruction: ADD rd, rs, rt"
  (list 'ADD rd rs rt))
```

### `utils-bootstrap.lisp` - Outils Bootstrap

**Responsabilités** :
- Fonctions pour compiler le compilateur dans la VM
- Construire des expressions Lisp en mémoire VM
- Marshalling avancé (handle-based returns)

**Fonctions clés** :
```lisp
(defun compile-from-handle (handle)
  "Compile une expression stockée via handle")

(defun compile-from-handle-with-handle-return (handle)
  "Compile et retourne un handle vers le résultat")

(defun build-expression-in-vm (expr)
  "Construit une expression Lisp en mémoire VM")
```

### `utils.lisp` - Utilitaires Généraux

**Contenu** :
- Fonctions d'affichage formaté
- Outils de debug
- Helpers divers

---

## 🧪 Développement et tests

### Ajouter une nouvelle fonction

1. Éditez `code.lisp` :
```lisp
(defparameter *function-definition*
  '(defun ma-fonction (x y)
     (+ (* x x) (* y y))))

(defparameter *function-name* 'MA-FONCTION)
(defparameter *function-args* '(3 4))
(defparameter *expected-result* 25)
```

2. Testez :
```bash
clisp exec-code.lisp              # Vérifie que ça fonctionne
clisp show-compile.lisp           # Examine le code généré
clisp exec-code-bootstrap.lisp    # Vérifie le bootstrap
```

### Debug

Si un programme échoue :
1. Vérifiez `code.lisp` (syntaxe, résultat attendu)
2. Lancez `show-compile.lisp` pour voir le code MIPS
3. Utilisez les statistiques de délégation pour identifier les problèmes FFI

---

## 📚 Ressources

### Concepts clés

- **MIPS** : Architecture RISC (Reduced Instruction Set Computer)
- **Bootstrap** : Un compilateur qui se compile lui-même
- **VM** : Machine virtuelle, interpréteur d'un jeu d'instructions
- **FFI** : Foreign Function Interface, appel de fonctions externes
- **Marshalling** : Sérialisation de données complexes

### Architecture MIPS simplifiée

```
Registres:
  $0      : toujours zéro
  $v0-$v1 : valeurs de retour
  $a0-$a3 : arguments de fonction
  $t0-$t9 : temporaires
  $sp     : stack pointer
  $ra     : return address

Mémoire:
  0x00000000 - 0x00100000 : Code
  0x00100000 - 0x00500000 : Données/Tas
  0x00500000 - 0x00A00000 : Pile (croît vers le bas)
```

---

## 🎯 Pour résumer

- **Vous modifiez** : `code.lisp` uniquement
- **Vous exécutez** : `exec-code.lisp` ou `exec-code-bootstrap.lisp`
- **Vous visualisez** : `show-compile.lisp` ou `show-compile-bootstrap.lisp`
- **Le système fait** : Compilation, exécution, vérification automatique

**C'est tout ! Le reste est géré par les fichiers dans `src/`.**
