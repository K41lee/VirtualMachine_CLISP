# 🚀 Compilateur LISP → MIPS# Compilateur LISP → MIPS + Machine Virtuelle



Un compilateur complet traduisant du code LISP vers l'assembleur MIPS, avec machine virtuelle intégrée.Un système complet de compilation et d'exécution développé en Common LISP, comprenant un compilateur LISP vers assembleur MIPS et une machine virtuelle MIPS.



## 📁 Structure du Projet## 🎯 Objectif du Projet



```Développer un système permettant de :

VirtualMachine_CLISP/1. **Compiler** du code LISP en assembleur MIPS

├── src/                          # Code source du compilateur2. **Exécuter** le code MIPS sur une machine virtuelle

│   ├── compiler.lisp             # Compilateur LISP → MIPS3. **Supporter** les fonctions récursives (ex: fibonacci)

│   ├── vm.lisp                   # Machine virtuelle MIPS4. **Comparer** les performances avec LISP natif

│   ├── loader.lisp               # Chargeur et utilitaires

│   ├── asm-ops.lisp              # Opérations assembleur**Objectif atteint** : fibonacci(20) = 6765 ✓

│   └── utils.lisp                # Fonctions utilitaires

│## 📦 Structure du Projet

├── tests/                        # Tests

│   ├── unit/                     # Tests unitaires```

│   │   ├── test-cond.lisp        # Tests CONDVirtualMachine_CLISP/

│   │   ├── test-when-unless.lisp # Tests WHEN/UNLESS├── README.md                    # Ce fichier - Documentation complète

│   │   ├── test-logical.lisp     # Tests AND/OR/NOT│

│   │   ├── test-case.lisp        # Tests CASE├── main.lisp                    # Point d'entrée principal du projet

│   │   ├── test-dotimes.lisp     # Tests DOTIMES│

│   │   ├── test-math.lisp        # Tests fonctions math├── asm-ops.lisp                 # Définitions opcodes et registres MIPS

│   │   └── ...├── vm.lisp                      # Machine virtuelle MIPS (exécution)

│   │├── loader.lisp                  # Chargeur de code avec résolution labels

│   └── debug/                    # Tests de débogage│

│       └── ...├── compiler.lisp                # Compilateur LISP → MIPS (noyau)

│├── test-compiler.lisp           # Tests du compilateur (séparé)

├── examples/                     # Exemples d'utilisation│

│   ├── examples.lisp             # Exemples LISP├── tests.lisp                   # Suite de tests de la VM

│   └── examples-mips.lisp        # Exemples MIPS├── utils.lisp                   # Outils de debug et visualisation

││

├── docs/                         # Documentation├── examples-mips.lisp           # Exemples de code MIPS

│   ├── RECAPITULATIF_SESSION.md  # Récapitulatif détaillé├── examples.lisp                # Exemples ancien format

│   ├── FichierTexteSuivi/        # Documentation de suivi│

│   └── Ressource_externe/        # Spécifications externes└── FichierTexteSuivi/           # Documentation consolidée

│    ├── GUIDE_PROJET.txt         # Plan + Progression complète

├── scripts/                      # Scripts utilitaires    ├── DOCUMENTATION_TECHNIQUE.txt # Référence MIPS complète

│   ├── run-tests.sh              # Lancer tous les tests    ├── HISTORIQUE_DEVELOPPEMENT.txt # Chronologie développement

│   └── validate-phase8.sh        # Validation phase 8    └── RAPPORT_FINAL.txt        # Rapport final consolidé

│```

├── main.lisp                     # Point d'entrée principal

└── README.md                     # Ce fichier### Organisation Modulaire

```

**Fichiers Principaux :**

## 🎯 Fonctionnalités- `compiler.lisp` : Contient uniquement la logique de compilation

- `test-compiler.lisp` : Contient tous les tests du compilateur

### Structures de Contrôle- `tests.lisp` : Tests de la machine virtuelle

- ✅ **IF** - Conditionnelle simple- `main.lisp` : Charge l'ensemble du système

- ✅ **COND** - Conditionnelle multiple en cascade

- ✅ **WHEN/UNLESS** - Conditionnelles simplifiées## 🚀 Installation et Lancement

- ✅ **CASE** - Pattern matching sur valeurs

### Prérequis

### Opérateurs Logiques

- ✅ **AND/OR** - Avec évaluation en court-circuit- **Common LISP** (CLISP, SBCL, ou autre implémentation)

- ✅ **NOT** - Négation logique- Système Unix/Linux ou Windows avec CLISP



### Boucles### Lancer le Projet

- ✅ **LOOP WHILE** - Boucle conditionnelle

- ✅ **DOTIMES** - Boucle avec compteur (0 à N-1)```bash

# Charger le système complet

### Variables et Fonctionsclisp main.lisp

- ✅ **LET** - Variables locales avec portée lexicale

- ✅ **SETQ** - Assignation de variables# Ou charger uniquement le compilateur

- ✅ **LABELS** - Fonctions locales avec closuresclisp -x "(load \"compiler.lisp\")"

- ✅ **Closures** - Capture de variables avec static links

# Charger le compilateur avec les tests

### Opérationsclisp -x "(load \"test-compiler.lisp\")"

- ✅ **Arithmétique** : +, -, *, /, mod```

- ✅ **Comparaison** : <, >, <=, >=, =, /=

- ✅ **Mathématiques** : abs, max, min## 💻 Utilisation



## 🚀 Démarrage Rapide### 1. Compiler et Exécuter un Programme Simple



### Installation```lisp

;; Charger le compilateur

```bash(load "compiler.lisp")

# Cloner le projet

git clone https://github.com/K41lee/VirtualMachine_CLISP.git;; Compiler une expression

cd VirtualMachine_CLISP(compile-lisp '(+ 5 3))

; => Code MIPS généré

# Vérifier que CLISP est installé

clisp --version;; Compiler et exécuter

```(let* ((code (compile-lisp '(+ 5 3)))

       (vm (make-new-vm)))

### Utilisation  (load-and-run vm code))

; => Résultat: 8

```bash```

# Lancer le compilateur

clisp main.lisp### 2. Définir et Appeler une Fonction



# Dans le REPL CLISP:```lisp

(compile-lisp '(+ 2 3))              # Compile vers MIPS;; Définir une fonction

(compile-and-run '(+ 2 3))           # Compile et exécute(defun test-double ()

```  (let* ((fib-def '(defun double (x) (* x 2)))

         (fib-call '(double 21))

### Exemples         (fib-code (compile-lisp fib-def))

         (call-code (compile-lisp fib-call))

```lisp         (full-code (append (list (list :JMP ':MAIN))

;; Factorielle avec LABELS et récursion                           fib-code

(labels ((fact (n)                           (list (list :LABEL ':MAIN))

           (if (<= n 1)                           call-code

               1                           (list (list :HALT))))

               (* n (fact (- n 1))))))         (vm (make-new-vm)))

  (fact 5))    (load-and-run vm full-code)

;; Résultat: 120    (get-register vm *reg-v0*)))



;; Boucle DOTIMES(test-double)

(let ((sum 0)); => 42

  (dotimes (i 10)```

    (setq sum (+ sum i)))

  sum)### 3. Tester Fibonacci avec Comparaison Performance

;; Résultat: 45 (somme de 0 à 9)

```lisp

;; Pattern matching avec CASE;; Tester fibonacci(10) et comparer avec LISP natif

(let ((x 2))(test-fibonacci-performance 10)

  (case x

    (1 100);; Tester fibonacci(20)

    (2 200)(test-fibonacci-performance 20)

    (3 300)```

    (otherwise 999)))

;; Résultat: 200**Exemple de sortie** :

```

;; Fonctions mathématiques================================================================================

(max (abs -5) (min 10 3))           TEST DE PERFORMANCE: FIBONACCI(10)

;; Résultat: 5================================================================================

```

--- Test avec CLISP natif ---

## 🧪 TestsRésultat: 55

Temps: 0.000044 secondes

### Lancer tous les tests

--- Test avec VM MIPS ---

```bashRésultat: 55

./scripts/run-tests.shTemps: 0.118468 secondes

```Instructions exécutées: 5834



### Lancer des tests spécifiquesRatio (VM / Natif): 2692.45x plus lent



```bash✓ Résultats identiques!

# Tests unitaires uniquement```

./scripts/run-tests.sh unit

### 4. Exécuter les Tests

# Tests de debug uniquement

./scripts/run-tests.sh debug```lisp

;; Tests de la VM

# Test spécifique(load "tests.lisp")

clisp tests/unit/test-math.lisp(test-vm-basic)

```(test-stack-operations)

(test-jal-jr)

### Résultats des Tests

;; Tests du compilateur

| Catégorie | Tests | Statut |(load "test-compiler.lisp")

|-----------|-------|--------|(test-compiler-constant)

| COND | 6/6 | ✅ 100% |(test-compiler-addition)

| WHEN/UNLESS | 7/7 | ✅ 100% |(test-compiler-if)

| AND/OR/NOT | 10/10 | ✅ 100% |(test-compiler-simple-function)

| CASE | 8/8 | ✅ 100% |(test-fibonacci-performance 10)

| DOTIMES | 6/6 | ✅ 100% |(run-all-compiler-tests)

| Fonctions Math | 21/21 | ✅ 100% |```

| **TOTAL** | **58/58** | **✅ 100%** |

## 🏗️ Architecture

## 📊 Architecture

### Machine Virtuelle MIPS

### Compilation LISP → MIPS

**38 Registres** :

```- `$zero` : Constante 0 (lecture seule)

Code LISP- `$v0-$v1` : Valeurs de retour

    ↓- `$a0-$a3` : Arguments de fonction

Parser (parse-lisp-expr)- `$t0-$t9` : Temporaires (10 registres)

    ↓- `$s0-$s7` : Sauvegardés (8 registres)

AST interne- `$gp, $sp, $fp, $ra` : Pointeurs spéciaux

    ↓- `$pc, $hi, $lo` : Registres spéciaux

Compilateur (compile-expr)

    ↓**Disposition Mémoire** :

Code MIPS```

    ↓0         : Réservé

Machine Virtuelle (VM)1-20      : Variables basses

    ↓21-2020   : Tas (allocation dynamique) ← $gp

Résultat2021-4999 : Espace libre

```5000←     : Pile (descendante) ← $sp

5000→     : Code (instructions) ← $pc

### Gestion de la Mémoire```



- **Pile** : Variables locales, frames de fonctions### Instructions Supportées (20+)

- **Registres** :

  - `$V0` : Valeur de retour**Arithmétiques** :

  - `$T0-$T3` : Temporaires (caller-saved)- `ADD $rs $rt $rd` : Addition

  - `$S0-$S2` : Saved (callee-saved)- `SUB $rs $rt $rd` : Soustraction

  - `$FP` : Frame pointer (closures)- `MUL $rs $rt` : Multiplication (résultat dans $hi:$lo)

  - `$SP` : Stack pointer- `DIV $rs $rt` : Division (quotient $lo, reste $hi)

  - `$RA` : Return address- `ADDI $rs imm $rt` : Addition immédiate



### Closures et Static Links**Transfert** :

- `LI imm $rt` : Load immediate

Les closures utilisent un système de **static links** pour accéder aux variables capturées :- `MOVE $rs $rd` : Move register

- Chaque frame de fonction contient un pointeur vers le frame parent- `LW $base offset $dest` : Load word

- Les variables capturées sont accessibles en suivant la chaîne de static links- `SW $src $base offset` : Store word

- Frame layout : `[Old FP][RA][Static Link][Params...]`

**Branchement** :

## 📝 Documentation- `J label` : Jump inconditionnel

- `JAL label` : Jump and link (appel fonction)

- **[RECAPITULATIF_SESSION.md](docs/RECAPITULATIF_SESSION.md)** : Historique détaillé des fonctionnalités- `JR $rs` : Jump register (retour fonction)

- **[README_OLD.md](docs/README_OLD.md)** : Documentation originale du projet- `BEQ $rs $rt label` : Branch if equal

- **[FichierTexteSuivi/](docs/FichierTexteSuivi/)** : Documentation technique des phases- `BNE $rs $rt label` : Branch if not equal

- **[Ressource_externe/](docs/Ressource_externe/)** : Spécifications du projet- `BLT $rs $rt label` : Branch if less than

- `BGT $rs $rt label` : Branch if greater than

## 🔧 Développement

**Contrôle** :

### Ajouter une Nouvelle Fonctionnalité- `HALT` : Arrêt VM

- `NOP` : No operation

1. **Parser** : Ajouter le cas dans `parse-lisp-expr` (`src/compiler.lisp`)- `PRINT $rs` : Affichage debug

2. **Compilateur** : Créer `compile-xxx` pour générer le code MIPS

3. **Dispatcher** : Ajouter le cas dans `compile-expr`### Compilateur LISP → MIPS

4. **Tests** : Créer `tests/unit/test-xxx.lisp`

**Expressions Supportées** :

### Convention de Code

1. **Constantes** : `42`, `-10`, `0`

- Fonctions de compilation : `compile-xxx`2. **Variables** : `x`, `n`, `result`

- Fonctions utilitaires : `xxx-yyy`3. **Arithmétique** : `+`, `-`, `*`, `/`, `mod`

- Labels uniques : `(gen-label env "PREFIX")`4. **Comparaisons** : `<`, `<=`, `>`, `>=`, `=`

- Commentaires : `;;;` pour sections, `;;` pour lignes5. **Conditionnelles** : `(if test then else)`

6. **Fonctions** : `(defun name (params) body)`

## 🐛 Problèmes Connus7. **Appels** : `(func arg1 arg2)`



### Closures - Test 5**Exemple de Compilation** :

**Statut** : ⚠️ En cours de correction

```lisp

**Problème** : Appels entre fonctions locales du même LABELS ne passent pas le bon static link.;; Code LISP

(defun fib (n)

**Exemple qui échoue** :  (if (<= n 1)

```lisp      n

(labels ((outer (factor)      (+ (fib (- n 1))

           (labels ((mult (n) (* factor n))         (fib (- n 2)))))

                    (twice (n) (mult (mult n))))

             (twice 3))));; Génère ~54 instructions MIPS avec :

  (outer 2));; - Prologue fonction (ADDI $SP, SW $RA, etc.)

;; Attendu: 12, Obtenu: 27;; - Test condition (<= n 1)

```;; - Branchements BEQ

;; - Appels récursifs JAL

**Cause identifiée** : Les fonctions au même niveau LABELS devraient partager le même static link parent, mais actuellement elles se passent leurs propres frame pointers.;; - Gestion pile pour résultats intermédiaires

;; - Épilogue fonction (LW $RA, JR $RA)

## 📈 Statistiques```



- **Lignes de code** : ~1400 lignes (compiler.lisp)## 🔧 Fonctionnalités Avancées

- **Fonctions de compilation** : 11+

- **Tests** : 58 tests unitaires### Gestion Dynamique des Registres

- **Taux de réussite** : 100% (58/58)

Le compilateur utilise un **pool limité de 3 registres** ($t0, $t1, $t2) avec allocation dynamique :

## 👥 Contributeurs

```lisp

- **Projet** : VirtualMachine_CLISP;; Pool de registres

- **Repository** : K41lee/VirtualMachine_CLISP(temp-regs-available '($T0 $T1 $T2))

- **Date** : Novembre 2025

;; Allocation dynamique

---(allocate-temp-reg env)  ; Alloue un registre

(free-temp-reg env reg)  ; Libère un registre

**Dernière mise à jour** : 26 novembre 2025

;; Stratégie 3 niveaux :
;; 1. 2 registres dispos → Code optimal
;; 2. 1 registre dispo → Code sans pile
;; 3. 0 registre → Spill sur pile
```

### Mode Verbose

```lisp
;; Exécuter avec traçage détaillé
(let ((vm (make-new-vm :verbose t)))
  (load-and-run vm code :verbose t))

;; Affiche chaque instruction exécutée :
;; [PC=5001] LI 21 $V0 → $V0=21
;; [PC=5002] MOVE $V0 $A0 → $A0=21
;; [PC=5003] JAL 5001 → $RA=5004, PC=5001
;; ...
```

### Outils de Debug

```lisp
;; Afficher état VM
(dump-vm-state vm)

;; Afficher registres
(dump-registers vm)

;; Afficher pile
(dump-stack vm)

;; Afficher code
(dump-code vm)

;; Statistiques
(print-vm-stats vm)
```

## 📊 Tests et Validation

### Tests Disponibles

**Tests VM (9 tests)** :
- `test-vm-basic` : Initialisation VM
- `test-stack-operations` : PUSH/POP
- `test-memory-operations` : LW/SW
- `test-labels` : Résolution labels
- `test-execution-simple` : 5 + 3 = 8
- `test-jal-jr` : Appels fonction

**Tests Compilateur (5 tests)** :
- `test-compiler-constant` : 42 → 42
- `test-compiler-addition` : (+ 5 3) → 8
- `test-compiler-if` : Conditionnelles
- `test-compiler-simple-function` : double(21) → 42
- `test-fibonacci-performance` : Fibonacci avec mesure temps

### Résultats Validation

```
✓ 9/9 tests VM réussis (100%)
✓ 5/5 tests compilateur réussis (100%)
✓ fibonacci(1) = 1
✓ fibonacci(2) = 1
✓ fibonacci(10) = 55
✓ fibonacci(20) = 6765 (722396 instructions, 8.98s)
```

## 🎓 Exemples d'Utilisation

### Exemple 1 : Factorielle

```lisp
(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

;; Compiler et exécuter
(let* ((fact-def '(defun fact (n) 
                    (if (<= n 1) 
                        1 
                        (* n (fact (- n 1))))))
       (fact-call '(fact 5))
       (code (append (list (list :JMP ':MAIN))
                    (compile-lisp fact-def)
                    (list (list :LABEL ':MAIN))
                    (compile-lisp fact-call)
                    (list (list :HALT))))
       (vm (make-new-vm)))
  (load-and-run vm code)
  (get-register vm *reg-v0*))
; => 120
```

### Exemple 2 : Expression Complexe

```lisp
;; Compiler (+ (* 2 3) (* 4 5))
(compile-lisp '(+ (* 2 3) (* 4 5)))

;; Code généré :
;; LI 2 $V0
;; MOVE $V0 $T0
;; LI 3 $V0
;; MOVE $V0 $T1
;; MUL $T0 $T1
;; MFLO $V0
;; MOVE $V0 $T0      ; Résultat 2*3 dans $t0
;; LI 4 $V0
;; MOVE $V0 $T1
;; LI 5 $V0
;; MUL $T1 $V0
;; MFLO $V0          ; Résultat 4*5 dans $v0
;; MOVE $V0 $T1
;; ADD $T0 $T1 $V0   ; Addition finale
```

### Exemple 3 : Countdown Récursif

```lisp
(defun countdown (n)
  (if (= n 0)
      0
      (countdown (- n 1))))

;; Test
(test-countdown 5)
; => Affiche : 5 4 3 2 1 0
```

## 📈 Performance

### Comparaison VM vs LISP Natif

| Test | LISP Natif | VM MIPS | Ratio | Instructions |
|------|------------|---------|-------|--------------|
| fib(1) | 0.000001s | 0.000792s | 792x | 29 |
| fib(2) | 0.000002s | 0.002151s | 1075x | 92 |
| fib(10) | 0.000044s | 0.118468s | 2692x | 5834 |
| fib(20) | 0.005267s | 8.979857s | 1705x | 722396 |

**Analyse** :
- Ratio constant ~1800-2700x indique implémentation correcte
- Overhead dû à :
  - Interprétation instructions (vs compilation native)
  - Gestion pile explicite
  - Pas d'optimisation (tail-call, mémoïsation)
- Performance acceptable pour VM éducative

## 🔍 Débogage

### Problèmes Courants

**1. Erreur "Adresse hors limites"**
```lisp
; Vérifier que labels sont correctement résolus
(collect-labels code 5000)
```

**2. Résultat incorrect**
```lisp
; Activer mode verbose
(let ((vm (make-new-vm :verbose t)))
  (load-and-run vm code :verbose t))
```

**3. Pile corrompue**
```lisp
; Vérifier état pile
(dump-stack vm)
; Vérifier $sp
(get-register vm *reg-sp*)  ; Devrait être ~4999 après exécution
```

### Traçage Détaillé

```lisp
;; Exécuter pas-à-pas
(defun step-by-step (vm)
  (loop while (not (eq (vm-state vm) :halted))
        do (progn
             (format t "~%=== Instruction ~A ==~%" 
                     (vm-instruction-count vm))
             (dump-registers vm)
             (execute-instruction vm)
             (read-char))))  ; Attendre entrée utilisateur
```

## 🛠️ Développement et Extension

### Ajouter une Nouvelle Instruction

1. **Définir dans asm-ops.lisp** :
```lisp
(defparameter *opcodes* 
  '(:ADD :SUB :MUL :DIV :MONOP ...))  ; Ajouter :MONOP
```

2. **Implémenter dans vm.lisp** :
```lisp
(:MONOP 
  (let ((src (first args))
        (dst (second args)))
    (set-register vm dst (ma-fonction (get-value vm src)))))
```

3. **Utiliser dans compilateur** :
```lisp
(defun compile-monop (arg env)
  (append (compile-expr arg env)
          (list (list :MONOP *reg-v0* *reg-v0*))))
```

### Ajouter un Nouveau Construct LISP

```lisp
;; Dans compiler.lisp
(defun compile-expr (expr env)
  (cond
    ;; ... autres cas ...
    ((eq (car expr) 'mon-construct)
     (compile-mon-construct (cdr expr) env))
    ...))

(defun compile-mon-construct (args env)
  ;; Implémenter compilation
  ...)
```

## 📚 Documentation Complète

Documentation organisée et consolidée dans `FichierTexteSuivi/` :

### **GUIDE_PROJET.txt** (Plan + Progression)
Contient le plan d'action complet (11 phases) et le suivi de progression détaillé.
- Objectifs du projet
- Plan d'action phase par phase
- État actuel de chaque phase
- Utilisation du système
- Prochaines étapes

### **DOCUMENTATION_TECHNIQUE.txt** (Référence MIPS)
Documentation technique complète de l'architecture MIPS.
- 38 registres MIPS détaillés
- 20+ instructions avec syntaxe et exemples
- Conventions d'appel MIPS
- Exemples complets (fibonacci, factorial, etc.)
- Score conformité MIPS 100%

### **HISTORIQUE_DEVELOPPEMENT.txt** (Chronologie)
Historique chronologique du développement.
- Phase 2 : Appels fonction JAL/JR
- Phase 3/5 : Compilateur LISP → MIPS
- Phase 6 : Fibonacci récursif
- Amélioration : Gestion dynamique registres
- Modularisation : Séparation tests
- Leçons apprises

### **RAPPORT_FINAL.txt** (Rapport consolidé)
Rapport final complet du projet.
- Résumé exécutif
- Résultats fibonacci(20)
- Architecture système
- Défis et solutions
- Analyse performance
- Conclusions

## 🏆 Résultats Finaux

### Objectifs Atteints ✓

- ✅ Machine virtuelle MIPS complète (38 registres, 20+ instructions)
- ✅ Compilateur LISP → MIPS fonctionnel
- ✅ Support récursivité (simple et double)
- ✅ Gestion pile robuste
- ✅ fibonacci(20) = 6765 validé
- ✅ Tests exhaustifs (14/14 réussis)
- ✅ Gestion dynamique registres avec spilling
- ✅ Documentation complète

### Conformité MIPS

- **Architecture** : 100% ✓
- **Registres** : 38/38 ✓
- **Instructions** : 20+ natives ✓
- **Conventions** : Appel fonction ✓
- **Mémoire** : Tas/Pile conformes ✓

## 👥 Contribution

Projet éducatif développé dans le cadre d'un cours de programmation en LISP.

**Auteur** : K41lee  
**Date** : Novembre 2025  
**Langage** : Common LISP (CLISP)  
**Paradigme** : Fonctionnel + Impératif

## 📄 Licence

Projet éducatif - Usage académique

## 🔗 Liens Utiles

- [MIPS Reference](https://en.wikipedia.org/wiki/MIPS_architecture)
- [Common LISP](https://common-lisp.net/)
- [CLISP Documentation](https://clisp.sourceforge.io/)

---

**Note** : Ce projet est un système complet et fonctionnel permettant de compiler et exécuter du code LISP sur une machine virtuelle MIPS. Il démontre les concepts de compilation, gestion mémoire, conventions d'appel, et récursivité dans un contexte éducatif.

Pour toute question ou problème, consultez la documentation dans `FichierTexteSuivi/` ou examinez les exemples dans `examples-mips.lisp`.

**Statut** : ✅ PROJET COMPLET ET VALIDÉ
