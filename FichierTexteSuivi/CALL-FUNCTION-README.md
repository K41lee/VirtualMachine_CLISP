# Appel Direct de Fonctions dans la VM

## Vue d'ensemble

La VM supporte maintenant les **appels directs de fonctions** via la fonction `call-function`, qui simplifie grandement l'exécution de code compilé.

## Utilisation

### Ancienne méthode (manuelle)

```lisp
;; 1. Localiser la fonction
(defparameter *func-addr* (find-function-address vm 'FIBO))

;; 2. Configurer les registres
(set-register vm (get-reg :a0) 20)
(set-register vm (get-reg :pc) *func-addr*)
(set-register vm (get-reg :ra) 0)

;; 3. Exécuter
(run-vm vm)

;; 4. Récupérer le résultat
(defparameter *result* (get-register vm (get-reg :v0)))
```

### Nouvelle méthode (automatique) ✨

```lisp
;; Tout en une seule ligne!
(defparameter *result* (call-function vm 'FIBO 20))
```

## API

### `call-function`

```lisp
(call-function vm function-name &rest args)
```

**Paramètres:**
- `vm` : Instance de la machine virtuelle
- `function-name` : Nom de la fonction (symbole, ex: `'FIBO`)
- `args` : Arguments de la fonction (0 à 4 maximum)

**Retourne:** Le résultat de la fonction (valeur dans `$V0`)

**Exemple:**
```lisp
;; Fonction à 1 argument
(call-function vm 'FIBO 20)  ; → 6765

;; Fonction à 2 arguments
(call-function vm 'ACK 3 4)  ; → 125

;; Fonction sans argument
(call-function vm 'MAIN)     ; → résultat
```

### `find-function-address`

```lisp
(find-function-address vm function-name)
```

Localise l'adresse d'une fonction dans le code chargé.

**Paramètres:**
- `vm` : Instance de la machine virtuelle
- `function-name` : Nom de la fonction (symbole)

**Retourne:** L'adresse absolue de la fonction

## Workflow complet

```lisp
;; 1. Créer la VM
(defparameter *vm* (make-new-vm))

;; 2. Compiler le code
(defparameter *code*
  (compile-lisp-to-mips-simplified
    '(defun fibo (n)
       (if (< n 2) n
           (+ (fibo (- n 1)) (fibo (- n 2)))))))

;; 3. Charger le code
(load-code *vm* *code*)

;; 4. Appeler la fonction (appels multiples possibles)
(call-function *vm* 'FIBO 5)   ; → 5
(setf (vm-state *vm*) :ready)  ; Réinitialiser l'état

(call-function *vm* 'FIBO 10)  ; → 55
(setf (vm-state *vm*) :ready)

(call-function *vm* 'FIBO 20)  ; → 6765
```

## Limitations

- **Maximum 4 arguments** (registres MIPS `$a0` à `$a3`)
- La VM doit être en état `:ready` avant chaque appel
- Une seule fonction peut être exécutée à la fois (pas de parallélisme)

## Gestion des erreurs

`call-function` gère automatiquement:
- L'erreur "Adresse mémoire hors limites: 0" (retour à RA=0 - normal)
- La validation de l'état de la VM
- La vérification du nombre d'arguments

Les autres erreurs sont propagées normalement.

## Exemples de fichiers

### Fichiers de démonstration

- **`exec-code.lisp`** : Exemple simple avec fibonacci
- **`demo-call-function.lisp`** : Démonstration complète avec 3 fonctions
  - Fibonacci (1 argument)
  - Factorielle (1 argument)
  - Ackermann (2 arguments)

### Exécution

```bash
# Exemple simple
clisp exec-code.lisp

# Démonstration complète
clisp demo-call-function.lisp
```

## Avantages

✅ **Simplicité** : Une seule ligne au lieu de 7-8  
✅ **Lisibilité** : Code plus clair et expressif  
✅ **Fiabilité** : Gestion automatique des erreurs  
✅ **Flexibilité** : Supporte 0 à 4 arguments  
✅ **Performance** : Pas de surcoût (même code assembleur exécuté)

## Implémentation

Les fonctions sont implémentées dans:
- **`src/vm.lisp`** : `call-function`, `find-function-address`
- **`src/loader.lisp`** : Stockage du code chargé pour localisation

La table `*vm-loaded-code*` maintient un mapping VM → code assembleur pour permettre la localisation des fonctions.
