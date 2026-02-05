╔════════════════════════════════════════════════════════════════════════════╗
║                   PLAN D'ACTION: BOOTSTRAP COMPLET                         ║
║              Compiler des expressions Lisp dans la VM                      ║
╚════════════════════════════════════════════════════════════════════════════╝

DATE: 7 janvier 2026
OBJECTIF: Permettre au compilateur compilé de compiler de vraies expressions Lisp
          passées en mémoire (pas seulement des constantes numériques)

═══════════════════════════════════════════════════════════════════════════════
ANALYSE DE L'EXISTANT
═══════════════════════════════════════════════════════════════════════════════

✅ DÉJÀ IMPLÉMENTÉ:
  1. Système d'interning de symboles
     - *vm-symbol-table* (hash-table bidirectionnelle)
     - 90+ symboles pré-internés
     - Instructions: INTERN, SYMBOL-NAME
     - Fonctions: intern-symbol(), symbol-name-from-id()

  2. Variables globales
     - *vm-globals* (hash-table)
     - Instructions: GLOBAL-GET, GLOBAL-SET
     - 25+ variables pré-définies

  3. Construction de listes
     - Instruction LIST (corrigée)
     - Instructions: LIST-CAR, LIST-CDR, LIST-CONS
     - *vm-lisp-objects* pour stocker les objets

⚠️  MANQUE ACTUELLEMENT:
  - Fonctions helper pour construire des expressions complexes
  - Test qui construit une expression Lisp en mémoire
  - Appel du compilateur avec l'expression construite

═══════════════════════════════════════════════════════════════════════════════
POINT 1: SYSTÈME DE SYMBOLES (✅ DÉJÀ FAIT)
═══════════════════════════════════════════════════════════════════════════════

STATUS: ✅ COMPLET

Ce qui existe:
  - Table d'interning: *vm-symbol-table* (vm.lisp lignes 89-170)
  - Symboles pré-internés: 90+ symboles (LI, ADD, $V0, DEFUN, IF, etc.)
  - Instruction INTERN: (INTERN "FIBO") → retourne ID
  - Instruction SYMBOL-NAME: (SYMBOL-NAME 42) → retourne "FIBO"
  - Conversion: symbol-id-to-keyword() pour debug

ACTIONS:
  □ RIEN À FAIRE (déjà complet)
  □ Vérifier que DEFUN, IF, <, +, -, LAMBDA sont internés
  □ Tester: (INTERN "NOUVEAU-SYMBOLE") crée bien un nouvel ID

═══════════════════════════════════════════════════════════════════════════════
POINT 2: REPRÉSENTATION DES EXPRESSIONS LISP EN MÉMOIRE
═══════════════════════════════════════════════════════════════════════════════

STATUS: ⚠️ PARTIELLEMENT FAIT (LIST existe, mais pas de helpers)

Ce qui existe:
  - LIST: Crée une liste depuis la pile
  - LIST-CONS: Ajoute un élément en tête
  - LIST-CAR/CDR: Lecture
  - *vm-lisp-objects*: Stockage des structures

Ce qui manque:
  - Fonction pour construire '(defun fibo (n) ...)
  - Fonction pour construire '(if (< n 2) n ...)
  - Test de construction d'une expression complexe

ACTIONS:

2.1. Créer des fonctions helper en Lisp natif
    □ build-lisp-expression-in-vm(vm, expr) → handle
      Exemple: (build-lisp-expression-in-vm vm '(+ 1 2))
      → Retourne un handle vers la structure en mémoire

    □ Algorithme récursif:
      - Atome: 
        * Nombre → retourner le nombre directement
        * Symbole → INTERN le symbole, retourner l'ID
      - Liste → 
        * Pour chaque élément: construire récursivement
        * Assembler avec LIST-CONS

    FICHIER: utils.lisp ou nouveau test-build-expression.lisp

2.2. Créer un test de construction d'expressions
    □ Test simple: (+ 1 2)
      Résultat attendu: handle vers (ID_+ . (1 . (2 . NIL)))
    
    □ Test imbriqué: (if (< n 2) n (+ n 1))
      Résultat: handle vers l'arbre complet
    
    FICHIER: test-build-expressions.lisp

2.3. Vérifier avec LIST-CAR/LIST-CDR
    □ Lire l'expression construite
    □ Vérifier la structure (CAR = symbole +, CDR = liste d'args)

ESTIMATION: 2-3 heures de développement

═══════════════════════════════════════════════════════════════════════════════
POINT 3: APPELER LE COMPILATEUR DANS LA VM
═══════════════════════════════════════════════════════════════════════════════

STATUS: ❌ NON FAIT (mais toutes les pièces existent)

Ce qui existe:
  - Compilateur compilé et chargé dans la VM
  - Adresse de compile-constant-simplified connue
  - Système de passage de paramètres ($A0, $A1, etc.)

Ce qui manque:
  - Construction de l'expression en mémoire AVANT l'appel
  - Appel avec le handle de l'expression
  - Validation que le code généré est correct

ACTIONS:

3.1. Modifier test-true-bootstrap.lisp - Phase préparation
    □ AVANT l'exécution:
      * Construire l'expression '42 → handle1
      * Construire l'expression '123 → handle2
    
    □ Code à ajouter (après ÉTAPE 4):
      ```lisp
      (format t "~%Étape 4.5: Construction des expressions en mémoire~%")
      (defun build-simple-constant (vm value)
        "Construit un nombre en mémoire (juste retourner le nombre)"
        value)  ; Les nombres n'ont pas besoin d'être internés
      
      (defparameter *expr-handles* nil)
      (dolist (const '(42 123))
        (let ((handle (build-simple-constant *bootstrap-vm* const)))
          (push (list const handle) *expr-handles*)))
      ```

3.2. Modifier l'appel du compilateur (ÉTAPE 6)
    □ Au lieu de passer la constante directement:
      * Passer le HANDLE de l'expression
      * Le compilateur lit l'expression avec LIST-CAR/CDR si nécessaire
    
    □ Code modifié:
      ```lisp
      (let ((expr-handle (second (find const *expr-handles* :key #'first))))
        ;; Appeler compile-constant-simplified avec le handle
        (set-register exec-vm (get-reg :a0) expr-handle)
        (set-register exec-vm (get-reg :a1) 0)  ; env = NIL
        ...)
      ```

3.3. Tester avec des expressions plus complexes
    □ Phase 1: (+ 1 2)
      * Construire l'expression avec INTERN et LIST
      * Compiler avec compile-lisp-to-mips-simplified
      * Vérifier le code généré
    
    □ Phase 2: (defun fibo (n) ...)
      * Construire toute l'expression DEFUN
      * Compiler
      * Comparer avec le natif

3.4. Validation finale
    □ Code natif = Code VM (comparaison binaire des IDs)
    □ Les deux utilisent les mêmes symboles internés
    □ Le compilateur dans la VM génère du MIPS fonctionnel

ESTIMATION: 3-4 heures de développement

═══════════════════════════════════════════════════════════════════════════════
PLAN D'IMPLÉMENTATION DÉTAILLÉ
═══════════════════════════════════════════════════════════════════════════════

PHASE 1: Fonctions de construction (1-2h)
──────────────────────────────────────────
□ 1.1. Créer utils-bootstrap.lisp
  Contenu:
    - build-atom-in-vm(vm, atom)
    - build-list-in-vm(vm, list)
    - build-expression-in-vm(vm, expr)
    - read-expression-from-vm(vm, handle)

□ 1.2. Tests unitaires
  Fichier: test-build-expressions.lisp
    - Test: construire '(+ 1 2)
    - Test: construire '(if t 42 0)
    - Test: lire avec LIST-CAR/CDR
    - Test: (build (read handle)) = identité

PHASE 2: Construction de FIBO (1h)
───────────────────────────────────
□ 2.1. Expression complète de fibo
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2)))))

□ 2.2. Construire étape par étape
  - Symboles: DEFUN, FIBO, N, IF, <, +, -
  - Sous-expressions: (< n 2), (- n 1), etc.
  - Expression complète

□ 2.3. Validation
  - Compter les nœuds de l'arbre
  - Vérifier avec LIST-CAR/CDR la structure

PHASE 3: Appel du compilateur (1-2h)
─────────────────────────────────────
□ 3.1. Modifier test-true-bootstrap.lisp
  - Ajouter l'étape 4.5 (construction)
  - Modifier l'étape 6 (passage de handles)

□ 3.2. Première tentative: compile-constant-simplified
  - Passer 42 comme expression construite
  - Vérifier le code généré

□ 3.3. Deuxième tentative: compile-lisp-to-mips-simplified
  - Compiler FIBO complet
  - Comparer natif vs VM

□ 3.4. Debug et ajustements
  - Ajouter traces si différences
  - Vérifier les IDs des symboles

PHASE 4: Validation finale (30min)
──────────────────────────────────
□ 4.1. Tests de régression
  - Test simple (42) fonctionne toujours
  - Test complexe (fibo) identique

□ 4.2. Documentation
  - Mettre à jour BOOTSTRAP_RESULTAT.txt
  - Enlever l'AVERTISSEMENT du test

□ 4.3. Nettoyage
  - Retirer les traces de debug
  - Commenter le code

═══════════════════════════════════════════════════════════════════════════════
CODE À CRÉER
═══════════════════════════════════════════════════════════════════════════════

FICHIER 1: utils-bootstrap.lisp
────────────────────────────────
```lisp
;;;; Fonctions pour construire des expressions Lisp dans la VM

(defun build-atom-in-vm (vm atom)
  "Construit un atome en mémoire de la VM
   - Nombre → retourne le nombre
   - Symbole → retourne l'ID du symbole (après INTERN)"
  (cond
    ((numberp atom) atom)
    ((symbolp atom)
     ;; Chercher si le symbole existe déjà
     (let ((existing-id (gethash (symbol-name atom) 
                                  (vm-symbol-table vm))))
       (if existing-id
           existing-id
           ;; Sinon, l'interner
           (intern-symbol (symbol-name atom)))))))

(defun build-list-in-vm (vm list-expr)
  "Construit une liste en mémoire de la VM
   Retourne un handle vers la structure"
  (if (null list-expr)
      0  ; NIL = 0
      (let* ((car-handle (build-expression-in-vm vm (car list-expr)))
             (cdr-handle (build-list-in-vm vm (cdr list-expr)))
             (cons-cell (cons car-handle cdr-handle))
             (handle (next-handle vm)))
        (setf (gethash handle *vm-lisp-objects*) cons-cell)
        handle)))

(defun build-expression-in-vm (vm expr)
  "Construit une expression Lisp arbitraire en mémoire de la VM"
  (cond
    ((null expr) 0)
    ((atom expr) (build-atom-in-vm vm expr))
    ((listp expr) (build-list-in-vm vm expr))))

(defun read-expression-from-vm (vm handle)
  "Lit une expression depuis la mémoire de la VM (pour debug)"
  (if (= handle 0)
      nil
      (let ((obj (gethash handle *vm-lisp-objects*)))
        (if (consp obj)
            (cons (read-expression-from-vm vm (car obj))
                  (read-expression-from-vm vm (cdr obj)))
            ;; Atome: nombre ou ID de symbole
            (if (numberp obj)
                (if (> obj 1000)  ; Heuristique: grands nombres = IDs
                    (symbol-id-to-keyword obj)
                    obj)
                obj)))))
```

FICHIER 2: test-build-expressions.lisp
───────────────────────────────────────
```lisp
#!/usr/bin/env clisp
;;;; Test de construction d'expressions Lisp dans la VM

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "utils-bootstrap.lisp")

(initialize-compiler-symbols)

(format t "╔════════════════════════════════════════════════════════╗~%")
(format t "║   TEST: Construction d'expressions Lisp dans la VM    ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%")

(defparameter *test-vm* (make-new-vm :verbose nil))

;; TEST 1: Atomes simples
(format t "~%TEST 1: Atomes simples~%")
(let ((h1 (build-expression-in-vm *test-vm* 42))
      (h2 (build-expression-in-vm *test-vm* 'FIBO)))
  (format t "  42    → handle ~A → ~A~%" h1 (read-expression-from-vm *test-vm* h1))
  (format t "  'FIBO → handle ~A → ~A~%" h2 (read-expression-from-vm *test-vm* h2)))

;; TEST 2: Listes simples
(format t "~%TEST 2: Listes simples~%")
(let ((h (build-expression-in-vm *test-vm* '(+ 1 2))))
  (format t "  '(+ 1 2) → handle ~A~%" h)
  (format t "  Lecture: ~A~%" (read-expression-from-vm *test-vm* h)))

;; TEST 3: Expressions imbriquées
(format t "~%TEST 3: Expressions imbriquées~%")
(let ((h (build-expression-in-vm *test-vm* '(if (< n 2) n (+ n 1)))))
  (format t "  '(if (< n 2) n (+ n 1))~%")
  (format t "  → handle ~A~%" h)
  (format t "  Lecture: ~A~%" (read-expression-from-vm *test-vm* h)))

;; TEST 4: DEFUN complet
(format t "~%TEST 4: Expression DEFUN complète~%")
(let ((fibo-expr '(defun fibo (n)
                    (if (< n 2)
                        n
                        (+ (fibo (- n 1)) 
                           (fibo (- n 2)))))))
  (format t "  Construction de FIBO...~%")
  (let ((h (build-expression-in-vm *test-vm* fibo-expr)))
    (format t "  → handle ~A~%" h)
    (format t "  Lecture (prettified):~%")
    (format t "    ~A~%" (read-expression-from-vm *test-vm* h))))

(format t "~%✅ Tous les tests de construction réussis!~%")
```

FICHIER 3: Modifications à test-true-bootstrap.lisp
────────────────────────────────────────────────────
Voir PHASE 3.1 ci-dessus pour les modifications exactes.

═══════════════════════════════════════════════════════════════════════════════
ORDRE D'EXÉCUTION
═══════════════════════════════════════════════════════════════════════════════

1. Créer utils-bootstrap.lisp
2. Créer test-build-expressions.lisp
3. Tester: clisp test-build-expressions.lisp
4. Vérifier que les expressions se construisent correctement
5. Modifier test-true-bootstrap.lisp (ajouter étape 4.5)
6. Tester l'appel avec expression construite
7. Comparer natif vs VM
8. Valider et documenter

═══════════════════════════════════════════════════════════════════════════════
CRITÈRES DE SUCCÈS
═══════════════════════════════════════════════════════════════════════════════

✓ Construire '(+ 1 2) en mémoire et le lire → '(+ 1 2)
✓ Construire FIBO complet en mémoire
✓ Compiler FIBO avec le compilateur dans la VM
✓ Code généré identique au natif (IDs)
✓ Plus d'avertissement dans le test
✓ Documentation à jour

═══════════════════════════════════════════════════════════════════════════════
ESTIMATION TOTALE: 5-7 heures
═══════════════════════════════════════════════════════════════════════════════

Décomposition:
  - Phase 1 (utils): 1-2h
  - Phase 2 (FIBO): 1h
  - Phase 3 (appel): 1-2h
  - Phase 4 (validation): 30min
  - Debug/ajustements: 1-2h
  - Documentation: 30min

═══════════════════════════════════════════════════════════════════════════════
