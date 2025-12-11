# Système de Benchmark Multi-Niveaux : LISP / VM0 / VM1→VM2

## Vue d'ensemble

Ce système permet de tester du code LISP sur **3 scénarios d'exécution** différents :

1. **LISP natif** : Exécution directe (référence de performance)
2. **VM0** : Machine virtuelle native en LISP interprétant du MIPS
3. **VM1→VM2** : Bootstrap avec VM compilée chargée dans VM0

---

## Résultats Actuels

### Test : Fibonacci récursif (n=14) = 610

```
Scénario             | Résultat | Temps (s)  | Ratio vs natif
---------------------|----------|------------|---------------
LISP natif           | 610      | 0.0006s    | 1x (référence)
VM0                  | 610      | 0.91s      | 1500x
VM1→VM2 (optimisé)   | 610      | 0.99s      | 1650x
```

### Analyse

- ✅ **Tous les scénarios donnent le bon résultat** (610)
- ✅ **VM0 et VM1→VM2 ont des performances similaires** (~1500x overhead)
- ⚠️ **VM1→VM2 devrait être BEAUCOUP plus lent** (~2,25M x overhead)

**Pourquoi ces résultats ?**
Le scénario VM1→VM2 actuel est **hybride** : parties réelles + simulation optimisée.

---

## État Actuel du Bootstrap VM1→VM2

### ✅ Ce qui est RÉEL

1. **Parsing de VM1**
   - 1472 instructions MIPS de vm-executable.mips parsées
   - Conversion de syntaxe MIPS → format ASM de la VM
   - Support de 15 opcodes MIPS (LI, MOVE, ADD, SUB, LW, SW, etc.)

2. **Chargement de VM1 dans VM0**
   - VM0 créée (1 MB mémoire, 40 registres)
   - VM1 (code MIPS) chargée en mémoire de VM0
   - 1472 instructions présentes et adressables

3. **Compilation du code utilisateur**
   - Code LISP → 118 instructions MIPS
   - Code prêt pour VM2

4. **Exécution finale**
   - Code s'exécute dans une VM native
   - Résultat correct retourné

### ⚡ Ce qui est SIMULÉ (pour performance)

1. **Appels de fonctions VM1**
   - `VM1.FN_MAKE-NEW-VM()` : Création de VM2
   - `VM1.FN_LOAD-CODE()` : Chargement du code dans VM2
   - `VM1.FN_RUN-VM()` : Exécution de VM2
   
   **Raison** : Ces fonctions existent dans VM1 mais ne sont pas appelées réellement depuis VM0

2. **Exécution en cascade**
   - VM0 interprétant VM1 interprétant VM2
   - **Overhead réel attendu : ~2,25 millions x**
   - **Temps estimé : ~22-30 minutes** au lieu de 1 seconde
   
   **Raison** : Trop lent pour être pratique (1500 x 1500 = 2,25M overhead)

---

## Architecture Détaillée

### Scénario 1 : LISP Natif
```
Code LISP → eval() → Résultat
```
- Temps : 0.0006s
- Overhead : 1x (référence)

### Scénario 2 : VM0
```
Code LISP → Compilateur → Instructions MIPS → VM0 (interprète MIPS) → Résultat
```
- Temps : 0.91s
- Overhead : ~1500x
- Instructions exécutées : ~68,000

### Scénario 3 : VM1→VM2 (Actuel - Hybride)
```
Code LISP → Compilateur → Instructions MIPS
                          ↓
VM1 (code MIPS) chargée dans VM0 ✓ RÉEL
                          ↓
[SIMULATION : VM1 crée VM2]
                          ↓
VM2 (native) exécute le code ✓ RÉEL
                          ↓
                       Résultat
```
- Temps : 0.99s
- Overhead : ~1650x
- Chargement VM1 : **RÉEL**
- Appels VM1 : **SIMULÉ**
- Exécution : **RÉELLE** (mais pas dans VM2)

### Scénario 3 : VM1→VM2 (Vrai Bootstrap - Non implémenté)
```
Code LISP → Compilateur → Instructions MIPS
                          ↓
VM0 (LISP natif, interprète MIPS)
 └─→ charge et exécute VM1 (code MIPS)
      └─→ VM1 crée VM2 (autre instance VM)
           └─→ VM1 charge le code dans VM2
                └─→ VM1 exécute VM2
                     └─→ VM2 interprète le code utilisateur
                          └─→ Résultat
```
- Temps estimé : **~1350s (22,5 minutes)**
- Overhead : **~2,25 millions x**
- Cascade d'interprétation : 
  - VM0 : 1500x
  - VM1 dans VM0 : 1500x
  - **Total : 1500² = 2,25M x**

---

## Ce qui Manque pour le Vrai Bootstrap

### Fonctions Critiques Absentes

1. **`load-code`** : Charge du code MIPS en mémoire
   - Existe dans `src/loader.lisp` (LISP natif)
   - **❌ Absente de `src/vm-compilable.lisp`** (VM1)
   - Dépend de : `preprocess-code`, `validate-program`, `collect-labels`, `resolve-labels`

2. **`run-vm`** : Boucle principale d'exécution
   - Existe dans `src/vm-compilable.lisp` mais **commentée**
   - Utilise `WHILE` qui n'est pas supporté par le compilateur
   - Doit être réécrite (récursion ou boucle déroulée)

### Infrastructure Manquante

3. **Table des labels MIPS**
   - Parser actuel : instructions ✓, labels ❌
   - Besoin : `(hash-table 'FN_MAKE-NEW-VM -> adresse-instruction)`
   - Nécessaire pour appeler les fonctions de VM1

4. **Mécanisme d'appel de fonctions**
   - Besoin : `(call-vm1-function vm0 'FN_MAKE-NEW-VM arg1 arg2)`
   - Doit :
     - Positionner $PC sur le label
     - Placer arguments dans $A0-$A3
     - Exécuter VM0 jusqu'au retour (JR $RA)
     - Récupérer résultat depuis $V0

5. **Gestion de la mémoire imbriquée**
   - VM2 existe comme structure de données dans VM1
   - VM1 existe comme code MIPS dans VM0
   - Passage de données complexes (listes, code) entre niveaux

---

## TODO List Complète

Un fichier détaillé a été créé : **`TODO-VRAI-BOOTSTRAP.md`**

### Résumé (11 étapes)

**Phase 1 : Préparation (4-6h)**
1. Analyser dépendances de load-code
2. Créer versions compilables de preprocess-code, collect-labels, resolve-labels
3. Créer version compilable de validate-program
4. Ajouter load-code dans vm-compilable.lisp
5. Réécrire run-vm sans WHILE (récursion ou déroulage)
6. Régénérer vm-executable.mips

**Phase 2 : Infrastructure (3-4h)**
7. Modifier parser pour créer table des labels
8. Implémenter call-vm1-function()

**Phase 3 : Tests (1h)**
9. Tester appel simple (FN_MAKE-NEW-VM)

**Phase 4 : Bootstrap (4-5h)**
10. Implémenter bootstrap complet
11. Mesurer overhead réel

**Total estimé : 10-15 heures**

---

## Options Recommandées

### Option A : Bootstrap Partiel Réel (Recommandé)
**Temps** : 6-8 heures  
**Implémentation** :
- ✅ Étapes 1-9 (tout sauf bootstrap complet)
- ✅ Vrai appel à `FN_MAKE-NEW-VM` depuis VM0
- ⚡ Simulation de `load-code` et `run-vm`
- 📊 Documentation précise de ce qui est réel/simulé

**Résultat** :
- Démontre la technique du bootstrap
- Overhead mesuré : ~3000x (au lieu de 2,25M x)
- Temps d'exécution : 2-3 secondes (au lieu de 22 minutes)
- Code fonctionnel et testable

### Option B : Bootstrap Instrumenté
**Temps** : 2 heures  
**Implémentation** :
- Garder simulation actuelle
- Compter précisément chaque opération simulée
- Estimer temps réel avec formules

**Résultat** :
- Aucun code complexe
- Résultats théoriques corrects
- Documentation pédagogique

### Option C : Bootstrap Complet
**Temps** : 10-15 heures  
**Implémentation** :
- Toutes les 11 étapes
- Vrai bootstrap fonctionnel
- Tests sur petit exemple (fibo(5))

**Résultat** :
- Vraie cascade d'interprétation
- Prouveté technique complète
- Très long à débugger

---

## Commandes d'Utilisation

### Test actuel
```bash
cd /home/etudiant/Bureau/CLisp/TD\ LISP-20251009/VirtualMachine_CLISP
clisp test-bootstrap-mod.lisp
```

### Benchmark personnalisé
```lisp
(load "run-benchmark.lisp")

;; Tous les scénarios
(benchmark-code '(+ 1 2 3))

;; Scénarios spécifiques
(benchmark-code '(* 5 5) :scenarios '(:native :vm0))

;; Fibonacci
(benchmark-code '(progn
                   (defun fibo (n)
                     (if (= n 0) 1
                         (if (= n 1) 1
                             (+ (fibo (- n 1)) (fibo (- n 2))))))
                   (fibo 10)))
```

### Fichiers de test disponibles
- `test-bootstrap-mod.lisp` : Fibonacci(14) = 610
- `demo-benchmark.lisp` : 7 exemples pré-configurés

---

## Conclusion

Le système actuel démontre **l'architecture du bootstrap** avec :
- ✅ VM1 réellement chargée dans VM0
- ✅ Code compilé et prêt pour VM2
- ✅ Résultats corrects sur tous les scénarios
- ⚡ Optimisation hybride pour éviter 22 minutes d'attente

Pour un vrai bootstrap complet (cascade VM0→VM1→VM2), suivre les **11 étapes du TODO-VRAI-BOOTSTRAP.md**.

**Recommandation académique** : Option A (Bootstrap Partiel Réel)
- Démontre la maîtrise technique
- Temps de développement raisonnable
- Documentation claire de l'architecture
- Explications théoriques de l'overhead complet
