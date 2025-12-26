# PLAN D'ACTION : COMPILER LE CHARGEUR (LOADER.LISP)

**Objectif** : Augmenter les capacités du compilateur pour qu'il puisse compiler le chargeur (`simple-load-code`)

**Date** : 26 décembre 2025

---

## 📊 ANALYSE DE LA SITUATION ACTUELLE

### Code du chargeur à compiler

```lisp
(defun simple-load-code (code-list start-addr)
  "Chargeur simplifié : charge une liste d'instructions à partir d'une adresse"
  (let ((addr start-addr)
        (i 0))
    (while (< i (length code-list))
      (let ((instr (nth i code-list)))
        (mem-write addr instr)
        (setq addr (+ addr 1))
        (setq i (+ i 1))))
    (set-register (get-reg :pc) start-addr)
    t))
```

### Constructions utilisées

1. ✅ **DEFUN** - Déjà supporté
2. ✅ **LET** - Déjà supporté (compile-let)
3. ✅ **WHILE** - Déjà supporté (compile-while, ligne 1385)
4. ✅ **< (comparaison)** - Déjà supporté (compile-comparison)
5. ❓ **LENGTH** - À vérifier / implémenter
6. ❓ **NTH** - À vérifier / implémenter
7. ❌ **MEM-WRITE** - Fonction VM, pas encore compilable
8. ✅ **SETQ** - Déjà supporté (compile-setq, ligne 1807)
9. ✅ **+ (addition)** - Déjà supporté (compile-arithmetic)
10. ❌ **SET-REGISTER** - Fonction VM, pas encore compilable
11. ❌ **GET-REG** - Fonction VM, pas encore compilable
12. ✅ **T (constante)** - Déjà supporté

---

## 🎯 BLOCAGES IDENTIFIÉS

### Blocage Principal #1 : Fonctions VM non compilables
**Fonctions concernées** :
- `mem-write` : Écriture en mémoire VM
- `set-register` : Modification registre VM
- `get-reg` : Lecture de constante registre

**Problème** : Ces fonctions manipulent directement l'état de la VM. Elles ne peuvent pas être compilées en MIPS car elles sont des fonctions **natives** de la VM hôte.

**Solution** : Remplacer par des primitives MIPS natives ou des appels système

### Blocage #2 : Fonctions de liste absentes
**Fonctions concernées** :
- `length` : Calcul de longueur d'une liste
- `nth` : Accès au n-ième élément d'une liste

**État** : Le compilateur supporte CAR, CDR, CONS, NULL (ligne 2363-2376) mais pas LENGTH ni NTH

### Blocage #3 : Système de types et représentation
**Problème** : Le chargeur manipule des **listes d'instructions** (structures complexes). Le compilateur actuel ne gère pas bien les structures imbriquées.

---

## 📋 PLAN D'ACTION DÉTAILLÉ

### PHASE 1 : IMPLÉMENTER LES FONCTIONS DE LISTE MANQUANTES

#### Étape 1.1 : Implémenter LENGTH
**Priorité** : 🔴 HAUTE  
**Temps estimé** : 30-45 minutes

**Tâches** :
1. Créer `compile-length` dans `compiler.lisp` (après `compile-null`, ~ligne 2090)
2. Algorithme :
   ```lisp
   (defun compile-length (list-expr env)
     "Compile (length list) → compte les éléments"
     (let ((code '())
           (loop-label (gen-label env "LENGTH_LOOP"))
           (end-label (gen-label env "LENGTH_END")))
       ;; Compiler l'expression liste → $V0
       (setf code (append code (compile-expr list-expr env)))
       (setf code (append code 
         `((:MOVE ,*reg-v0* ,*reg-t0*)    ; Liste dans $T0
           (:LI 0 ,*reg-v0*)               ; Compteur = 0
           (:LABEL ,loop-label)
           ;; Si liste vide, fin
           (:BEQ ,*reg-t0* ,*reg-zero* ,end-label)
           ;; Incrémenter compteur
           (:ADDI ,*reg-v0* 1 ,*reg-v0*)
           ;; Avancer dans liste (CDR)
           (:ADDI ,*reg-t0* 8 ,*reg-t0*)  ; Passer au CDR (+8 octets)
           (:LW ,*reg-t0* ,*reg-t0* 0)    ; Charger CDR
           (:J ,loop-label)
           (:LABEL ,end-label))))
       code))
   ```

3. Ajouter le case dans `compile-expr` (~ligne 2367) :
   ```lisp
   (:length
    (compile-length (second parsed) env))
   ```

4. Ajouter `:length` au parser dans `parse-lisp` (~ligne 2200)

**Tests** :
```lisp
(compile-lisp '(length '(1 2 3)))  ; Devrait retourner 3
(compile-lisp '(length nil))       ; Devrait retourner 0
```

---

#### Étape 1.2 : Implémenter NTH
**Priorité** : 🔴 HAUTE  
**Temps estimé** : 45-60 minutes

**Tâches** :
1. Créer `compile-nth` dans `compiler.lisp` (après `compile-length`)
2. Algorithme :
   ```lisp
   (defun compile-nth (index-expr list-expr env)
     "Compile (nth n list) → retourne le n-ième élément"
     (let ((code '())
           (loop-label (gen-label env "NTH_LOOP"))
           (end-label (gen-label env "NTH_END")))
       ;; Compiler index → $V0
       (setf code (append code (compile-expr index-expr env)))
       (setf code (append code `((:MOVE ,*reg-v0* ,*reg-t1*))))  ; Index dans $T1
       
       ;; Compiler liste → $V0
       (setf code (append code (compile-expr list-expr env)))
       (setf code (append code `((:MOVE ,*reg-v0* ,*reg-t0*))))  ; Liste dans $T0
       
       ;; Boucle pour avancer de N éléments
       (setf code (append code
         `((:LABEL ,loop-label)
           ;; Si index = 0, on a trouvé
           (:BEQ ,*reg-t1* ,*reg-zero* ,end-label)
           ;; Si liste vide, erreur (retourner NIL = 0)
           (:BEQ ,*reg-t0* ,*reg-zero* ,end-label)
           ;; Décrémenter index
           (:ADDI ,*reg-t1* -1 ,*reg-t1*)
           ;; Avancer dans liste (CDR)
           (:ADDI ,*reg-t0* 8 ,*reg-t0*)
           (:LW ,*reg-t0* ,*reg-t0* 0)
           (:J ,loop-label)
           (:LABEL ,end-label)
           ;; Récupérer CAR de la cellule courante
           (:LW ,*reg-v0* ,*reg-t0* 0))))  ; CAR = premier mot
       code))
   ```

3. Ajouter au parser et au dispatch

**Tests** :
```lisp
(compile-lisp '(nth 0 '(10 20 30)))  ; Devrait retourner 10
(compile-lisp '(nth 2 '(10 20 30)))  ; Devrait retourner 30
```

---

### PHASE 2 : GÉRER LES APPELS AUX FONCTIONS VM

#### Étape 2.1 : Stratégie pour MEM-WRITE
**Priorité** : 🔴 CRITIQUE  
**Temps estimé** : 2-3 heures

**Problème** : `mem-write` est une fonction de la VM hôte (CLISP) qui écrit dans la mémoire de la VM. Quand on compile en MIPS, il faut que le code MIPS puisse écrire dans **sa propre** mémoire VM.

**Solutions possibles** :

##### Option A : Remplacer par SW (Store Word)
```lisp
;; Au lieu de :
(mem-write addr instr)

;; Utiliser :
(sw instr addr 0)  ; Store Word : mémoire[addr] ← instr
```

**Tâches Option A** :
1. Créer `compile-mem-write` qui génère des instructions SW
2. **Problème** : `instr` est une liste complexe `(ADD $T0 $T1 $T2)`, pas un entier
3. **Solution** : Il faut un encodeur d'instructions

##### Option B : Créer des primitives VM
```lisp
(defun compile-vm-call (func-name args env)
  "Compile un appel à une primitive VM"
  (case func-name
    (mem-write
     (compile-mem-write-primitive args env))
    (set-register
     (compile-set-register-primitive args env))
    ...))
```

**Tâches Option B** :
1. Définir liste de primitives VM dans `compiler.lisp`
2. Ajouter détection dans `parse-lisp` pour distinguer primitives VM
3. Implémenter handlers spéciaux pour chaque primitive

##### Option C : Syscalls / Trap Instructions
```lisp
;; Définir des syscalls MIPS
(:SYSCALL 100)  ; Syscall #100 = mem-write
```

**Recommandation** : **Option B** (primitives VM) est la plus flexible

**Plan détaillé Option B** :

1. **Créer table des primitives** (~ligne 120) :
   ```lisp
   (defparameter *vm-primitives* 
     '(mem-write mem-read set-register get-register get-reg)
     "Liste des fonctions primitives de la VM")
   
   (defun vm-primitive-p (symbol)
     "Vérifie si un symbole est une primitive VM"
     (member symbol *vm-primitives*))
   ```

2. **Modifier parse-lisp** (~ligne 2150) pour détecter primitives :
   ```lisp
   ((vm-primitive-p (car expr))
    (list :vm-primitive (car expr) (cdr expr)))
   ```

3. **Créer compile-vm-primitive** :
   ```lisp
   (defun compile-vm-primitive (name args env)
     "Compile un appel à une primitive VM"
     (case name
       (mem-write (compile-mem-write-prim args env))
       (set-register (compile-set-register-prim args env))
       (get-reg (compile-get-reg-prim args env))
       (t (error "Primitive VM inconnue: ~A" name))))
   ```

4. **Implémenter chaque primitive** :

   **mem-write** :
   ```lisp
   (defun compile-mem-write-prim (args env)
     "Compile (mem-write addr value) → SW"
     (let ((addr-expr (first args))
           (value-expr (second args))
           (code '()))
       ;; Compiler adresse → $V0
       (setf code (append code (compile-expr addr-expr env)))
       (setf code (append code `((:MOVE ,*reg-v0* ,*reg-t0*))))
       
       ;; Compiler valeur → $V0
       (setf code (append code (compile-expr value-expr env)))
       
       ;; SW $V0, 0($T0) : mémoire[$T0] ← $V0
       (setf code (append code `((:SW ,*reg-v0* ,*reg-t0* 0))))
       code))
   ```

   **set-register** :
   ```lisp
   (defun compile-set-register-prim (args env)
     "Compile (set-register reg-id value)"
     ;; Besoin d'une instruction spéciale ou mappage registre→adresse
     ;; Pour l'instant, simplification : ignorer ou erreur
     (error "set-register pas encore supporté en compilation"))
   ```

   **get-reg** :
   ```lisp
   (defun compile-get-reg-prim (args env)
     "Compile (get-reg :pc) → retourne numéro de registre"
     (let ((reg-keyword (first args)))
       ;; Retourner la constante numérique du registre
       (list (list :LI (reg-keyword-to-number reg-keyword) *reg-v0*))))
   ```

---

#### Étape 2.2 : Simplifier le chargeur pour éviter SET-REGISTER
**Priorité** : 🟡 MOYENNE  
**Temps estimé** : 1 heure

**Objectif** : Modifier `simple-load-code` pour éviter `set-register`

**Code modifié** :
```lisp
(defun simple-load-code (code-list start-addr)
  "Chargeur simplifié sans set-register"
  (let ((addr start-addr)
        (i 0))
    (while (< i (length code-list))
      (let ((instr (nth i code-list)))
        (mem-write addr instr)
        (setq addr (+ addr 1))
        (setq i (+ i 1))))
    ;; Retourner l'adresse de début au lieu de modifier $PC
    start-addr))
```

**Avantage** : Élimine `set-register` et `get-reg` complètement

---

### PHASE 3 : GÉRER LES STRUCTURES COMPLEXES (LISTES D'INSTRUCTIONS)

#### Étape 3.1 : Représentation des instructions en mémoire
**Priorité** : 🟠 HAUTE  
**Temps estimé** : 3-4 heures

**Problème** : Le chargeur manipule une **liste d'instructions** où chaque instruction est une liste : `((ADD $T0 $T1 $T2) (LW $T0 $SP 0) ...)`

**Solution** : Encoder les instructions comme des structures en mémoire

**Format proposé** :
```
Instruction = [opcode, arg1, arg2, arg3]
Liste = cellule CONS : [CAR=instruction, CDR=reste]
```

**Exemple** :
```
(ADD $T0 $T1 $T2) → [1, 8, 9, 10]  ; 1=ADD, 8=T0, 9=T1, 10=T2
```

**Tâches** :

1. **Créer encodeur d'instructions** dans `compiler.lisp` :
   ```lisp
   (defparameter *opcode-table*
     '((ADD . 1) (SUB . 2) (LW . 3) (SW . 4) (BEQ . 5) ...))
   
   (defparameter *register-table*
     '((:$V0 . 2) (:$T0 . 8) (:$T1 . 9) ...))
   
   (defun encode-instruction (instr)
     "Encode une instruction symbolique en entiers"
     (let ((opcode (cdr (assoc (car instr) *opcode-table*)))
           (args (mapcar #'encode-operand (cdr instr))))
       (cons opcode args)))
   
   (defun encode-operand (operand)
     "Encode un opérande (registre, immédiat, label)"
     (cond
       ((keywordp operand) 
        (cdr (assoc operand *register-table*)))
       ((numberp operand) operand)
       ((symbolp operand) 
        ;; Label : retourner adresse ou référence
        (gethash operand *label-table*))
       (t (error "Opérande invalide: ~A" operand))))
   ```

2. **Modifier compile-mem-write** pour accepter instructions encodées

3. **Créer fonction de sérialisation** :
   ```lisp
   (defun compile-instruction-list (instrs env)
     "Compile une liste d'instructions en structure de données"
     ;; Créer une liste chaînée en mémoire
     ...)
   ```

**Alternative plus simple** : **Utiliser des tableaux**

```lisp
;; Au lieu de liste d'instructions, utiliser un tableau
(defun simple-load-code (code-array start-addr count)
  (let ((i 0))
    (while (< i count)
      (let ((instr (aref code-array i)))
        (mem-write (+ start-addr i) instr)
        (setq i (+ i 1))))
    start-addr))
```

Le compilateur supporte déjà `aref` et `make-array` !

---

### PHASE 4 : TESTS ET VALIDATION

#### Étape 4.1 : Tests unitaires pour LENGTH et NTH
**Fichier** : `tests/unit/test-compiler-lists.lisp`

```lisp
;; Test LENGTH
(defparameter *test-length-code*
  '(progn
     (defun test-length ()
       (let ((list1 '(1 2 3 4 5)))
         (length list1)))))

;; Test NTH
(defparameter *test-nth-code*
  '(progn
     (defun test-nth ()
       (let ((list1 '(10 20 30 40)))
         (nth 2 list1)))))
```

#### Étape 4.2 : Test du chargeur simplifié
**Fichier** : `tests/integration/test-loader-compilation.lisp`

```lisp
(defparameter *simple-loader*
  '(defun simple-load-code (code-array start-addr count)
     (let ((i 0))
       (while (< i count)
         (aref code-array i)  ; Juste lire pour commencer
         (setq i (+ i 1)))
       count)))

;; Compiler
(defparameter *loader-mips* (compile-lisp *simple-loader*))

;; Charger et tester
(load-code *vm* *loader-mips*)
```

#### Étape 4.3 : Test d'intégration complet
**Objectif** : Décommenter les étapes 2-3 de `test-full-compilation-chain.lisp`

---

## 📊 RÉCAPITULATIF DES ÉTAPES

### Ordre d'exécution recommandé

| # | Phase | Étape | Priorité | Temps | Dépendances |
|---|-------|-------|----------|-------|-------------|
| 1 | 1.1 | Implémenter LENGTH | 🔴 | 45min | Aucune |
| 2 | 1.2 | Implémenter NTH | 🔴 | 60min | LENGTH |
| 3 | 4.1 | Tests LENGTH/NTH | 🟢 | 30min | 1, 2 |
| 4 | 2.2 | Simplifier loader | 🟡 | 60min | Aucune |
| 5 | 2.1 | Impl. VM primitives | 🔴 | 3h | LENGTH, NTH |
| 6 | 3.1 | Repr. structures | 🟠 | 4h | 1-5 |
| 7 | 4.2 | Test loader simple | 🟢 | 45min | 1-6 |
| 8 | 4.3 | Test intégration | 🟢 | 30min | 1-7 |

**Temps total estimé** : 10-12 heures de développement

---

## 🎯 JALONS (MILESTONES)

### Milestone 1 : Fonctions de liste (2h)
- ✅ LENGTH compilable
- ✅ NTH compilable
- ✅ Tests passent

### Milestone 2 : Primitives VM basiques (4h)
- ✅ Table des primitives créée
- ✅ mem-write via SW fonctionnel
- ✅ Détection automatique des primitives

### Milestone 3 : Loader simplifié (6h)
- ✅ Version sans set-register
- ✅ Utilise tableaux au lieu de listes
- ✅ Compilation sans erreur

### Milestone 4 : Test complet (10h)
- ✅ Loader compile en MIPS
- ✅ Loader chargé dans VM
- ✅ Étapes 2-3 décommentées dans test-full-compilation-chain.lisp

---

## 🚧 DÉFIS ANTICIPÉS

### Défi #1 : Représentation des instructions
**Problème** : Les instructions MIPS sont des listes symboliques, pas des entiers  
**Solution** : Encoder les instructions ou utiliser un format simplifié

### Défi #2 : Gestion de la mémoire
**Problème** : Le loader doit écrire dans la zone mémoire de la VM  
**Solution** : SW écrit directement en mémoire, mais attention aux adresses

### Défi #3 : Intégration avec le reste du système
**Problème** : Les modifications peuvent casser les tests existants  
**Solution** : Tests incrémentaux, validation continue

---

## 📚 RÉFÉRENCES

### Fichiers à modifier
- `src/compiler.lisp` : Ajout LENGTH, NTH, primitives VM
- `src/loader.lisp` : Simplification de simple-load-code
- `tests/unit/test-compiler-lists.lisp` : Nouveaux tests
- `tests/integration/test-loader-compilation.lisp` : Test loader
- `tests/integration/test-full-compilation-chain.lisp` : Décommenter étapes

### Documentation
- [Instruction MIPS Reference](documentation/Reference_MIPS.txt)
- [Compiler Architecture](documentation/STRUCTURE_ORGANISEE.md)
- [VM Memory Layout](src/vm.lisp lignes 1-100)

---

## ✅ CRITÈRES DE SUCCÈS

Le projet est considéré comme réussi quand :

1. ✅ `(compile-lisp '(length '(1 2 3)))` retourne du code MIPS valide
2. ✅ `(compile-lisp '(nth 2 '(10 20 30)))` retourne du code MIPS valide
3. ✅ `(compile-lisp *simple-loader*)` compile sans erreur
4. ✅ Le loader compilé peut être chargé dans la VM
5. ✅ Les étapes 2-3 de `test-full-compilation-chain.lisp` fonctionnent
6. ✅ Tous les tests existants continuent de passer

---

## 🔄 PROCHAINES ÉTAPES APRÈS SUCCÈS

Une fois le loader compilable :

1. **Compiler le compilateur** (étapes 4-5)
   - Défis similaires mais plus complexes
   - Nécessite DEFUN récursif, tables de hachage
   
2. **Bootstrap complet**
   - VM0 (native) → Loader (MIPS) → Compiler (MIPS) → Code (MIPS)
   - Self-hosting du compilateur

3. **Optimisations**
   - Réduction du nombre d'instructions
   - Gestion mémoire plus efficace

---

**Document créé le** : 26 décembre 2025  
**Auteur** : GitHub Copilot  
**Version** : 1.0
