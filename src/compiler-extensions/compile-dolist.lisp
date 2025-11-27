;;;; compile-dolist.lisp
;;;; Extension du compilateur pour supporter DOLIST
;;;; Phase 11 - Compilation de la VM

;;; ============================================================================
;;; STRATÉGIE D'IMPLÉMENTATION DOLIST
;;; ============================================================================

;; DOLIST en LISP itère sur les éléments d'une liste:
;; (dolist (var list [result]) body...)
;;
;; Exemple:
;;   (dolist (x '(1 2 3))
;;     (print x))
;;   
;;   Équivalent à:
;;   (let ((x nil)
;;         (temp-list '(1 2 3)))
;;     (while temp-list
;;       (progn
;;         (setq x (car temp-list))
;;         (setq temp-list (cdr temp-list))
;;         body...)))

;;; ============================================================================
;;; SYNTAXE
;;; ============================================================================

;; Forme générale:
;;   (dolist (var list-expr [result-form]) body...)
;;
;; Arguments:
;;   - var: symbole, variable d'itération (prend chaque élément)
;;   - list-expr: expression qui retourne une liste
;;   - result-form: optionnel, valeur de retour après la boucle
;;   - body: expressions à exécuter pour chaque élément

;; Exemples:
;;   (dolist (x '(1 2 3)) (print x))           ; itère sur 1, 2, 3
;;   (dolist (x my-list) (+ x 1))              ; itère sur my-list
;;   (dolist (x numbers 42) (setq sum (+ sum x))) ; retourne 42 à la fin

;;; ============================================================================
;;; TRANSFORMATION
;;; ============================================================================

;; DOLIST se transforme en:
;;
;; (dolist (var list-expr result-form) body...)
;; 
;; →
;;
;; (let ((var nil)
;;       (#:list-temp list-expr))
;;   (while #:list-temp
;;     (progn
;;       (setq var (car #:list-temp))
;;       (setq #:list-temp (cdr #:list-temp))
;;       body...))
;;   result-form)  ; ou nil si absent

;;; ============================================================================
;;; IMPLÉMENTATION
;;; ============================================================================

(defun compile-dolist (var-spec body env)
  "Compile (dolist (var list [result]) body...)
   
   Syntaxe: (dolist (x '(1 2 3)) (print x))
   
   Transformation:
   1. Créer variable temporaire pour la liste
   2. Créer variable d'itération
   3. Boucle while: tant que liste non vide
      - Assigner (car liste) à var
      - Avancer liste avec (cdr liste)
      - Exécuter body
   4. Retourner result-form (ou nil)
   
   Arguments:
   - var-spec: (var list-expr [result-form])
   - body: expressions à exécuter
   - env: environnement de compilation
   
   Note: Pour la Phase 11, on simplifie en supposant que:
   - Les listes sont des QUOTÉES '(1 2 3) ou des variables
   - Pas de listes imbriquées complexes
   - CAR et CDR sont des opérations primitives"
  
  (let* ((var (first var-spec))
         (list-expr (second var-spec))
         (result-form (third var-spec))  ; Optionnel, peut être nil
         (list-temp-var (gensym "DOLIST-TEMP"))
         (code '()))
    
    ;; NOTE: Pour vm.lisp, les cas d'usage sont simples:
    ;; - (dolist (reg *register-names*) ...) où *register-names* est une constante
    ;; - La liste est déjà en mémoire (pas besoin de construction)
    
    ;; PROBLÈME: Listes ne sont PAS encore supportées par le compilateur !
    ;; Pour Phase 11, on doit d'abord implémenter:
    ;; 1. Support des listes quotées '(1 2 3)
    ;; 2. Opérations CAR et CDR
    ;; 3. Opération NULL (test liste vide)
    
    ;; Pour l'instant, on documente l'approche et on marquera DOLIST
    ;; comme "À SIMPLIFIER" dans vm-compilable.lisp
    
    (error "DOLIST pas encore implémenté - nécessite support LISTES (CAR/CDR/NULL)")))

;;; ============================================================================
;;; ALTERNATIVE: TRANSFORMER DOLIST EN BOUCLES INDEXÉES
;;; ============================================================================

;; Pour vm.lisp, les 2 usages de DOLIST sont:
;;
;; 1. (dolist (reg *register-names*) ...)
;;    → Peut être transformé en:
;;    (let ((i 0))
;;      (while (< i (length *register-names*))
;;        (let ((reg (nth i *register-names*)))
;;          body...
;;          (setq i (+ i 1)))))
;;
;; 2. Même pattern pour dump-registers
;;
;; Cette approche nécessite:
;; - LENGTH (compte éléments d'une liste) 
;; - NTH (accès élément par index)
;; - Mais évite CAR/CDR/NULL !

;;; ============================================================================
;;; DÉCISION POUR PHASE 11
;;; ============================================================================

;; OPTION A: Implémenter LISTES complètes (CAR/CDR/NULL/CONS)
;;   Avantages: Support complet DOLIST, élégant
;;   Inconvénients: Complexe (3-4h), lists = structures avec pointeurs
;;   
;; OPTION B: Transformer DOLIST en boucles indexées dans vm-compilable.lisp
;;   Avantages: Simple, réutilise code existant (WHILE, LET, <, +)
;;   Inconvénients: Nécessite LENGTH et NTH
;;
;; OPTION C: Dérouler manuellement les DOLIST dans vm-compilable.lisp
;;   Avantages: Très simple, pas de nouveau code
;;   Inconvénients: Code répétitif, perd l'abstraction
;;
;; RECOMMANDATION: OPTION C pour Phase 11
;;   - Les 2 dolist dans vm.lisp peuvent être déroulés manuellement
;;   - *register-names* a 42 éléments → 42 appels explicites
;;   - Ou mieux: remplacer par une boucle numérique (0 à 41)

;;; ============================================================================
;;; EXEMPLE DE TRANSFORMATION (OPTION C)
;;; ============================================================================

;; AVANT (dans vm.lisp):
;;   (dolist (reg *register-names*)
;;     (setf (gethash reg (vm-registers vm)) 0))
;;
;; APRÈS (dans vm-compilable.lisp):
;;   (let ((i 0))
;;     (while (< i 42)
;;       (progn
;;         (let ((reg (aref *register-names-array* i)))
;;           (setf (aref vm-registers i) 0))
;;         (setq i (+ i 1)))))
;;
;; Note: Remplace aussi hash-table par array pour simplifier !

;;; ============================================================================
;;; CONCLUSION
;;; ============================================================================

;; DOLIST n'est PAS implémenté dans le compilateur pour Phase 11.
;;
;; À la place, dans vm-compilable.lisp:
;; 1. Remplacer *register-names* (liste) par *register-names-array* (array)
;; 2. Remplacer (vm-registers vm) (hash-table) par array
;; 3. Transformer DOLIST en boucles WHILE indexées
;;
;; Cette approche:
;; - Évite d'implémenter listes/CAR/CDR/NULL (gain 3-4h)
;; - Réutilise ARRAYS déjà implémenté
;; - Simplifie vm-compilable.lisp (structures → arrays)
;; - Compatible avec notre compilateur actuel

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

;; Note: compile-dolist n'est pas exporté car non implémenté
;; (export '())
