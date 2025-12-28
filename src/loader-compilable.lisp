;;;; loader-compilable.lisp
;;;; Version compilable du loader utilisant uniquement les primitives VM

;;; ============================================================================
;;; COLLECT-LABELS - VERSION COMPILABLE
;;; ============================================================================

(defun collect-labels (asm-code code-start)
  "Collecte tous les labels et leurs positions ABSOLUES dans le code.
   Version compilable utilisant les primitives VM."
  (let ((labels (vm-make-hash-table :test 'equal))
        (position 0)
        (first-elem nil))
    (dolist (instr asm-code)
      (if (vm-consp instr)
          (progn
            (setq first-elem (vm-car instr))
            ;; Stocker chaque premier élément avec sa position
            (vm-hash-set labels first-elem (+ code-start position))
            (setq position (+ position 1)))
          (setq position (+ position 1))))
    labels))

;;; ============================================================================
;;; RESOLVE-LABELS - VERSION COMPILABLE
;;; ============================================================================

(defun resolve-labels (asm-code labels-hash)
  "Remplace les références symboliques par des adresses.
   Version compilable utilisant construction impérative de listes."
  (let ((resolved-code nil)
        (instr nil)
        (first-elem nil)
        (resolved-instr nil))
    (dolist (instruction asm-code)
      (setq instr instruction)
      ;; Vérifier si c'est une instruction cons
      (if (vm-consp instr)
          (progn
            (setq first-elem (vm-car instr))
            ;; Ne pas inclure les :LABEL dans le code final
            ;; On stocke tous les opcodes, mais on pourrait filtrer :LABEL
            (setq resolved-instr (resolve-instruction instr labels-hash))
            (setq resolved-code (vm-cons resolved-instr resolved-code)))
          nil))
    ;; Inverser la liste (construite à l'envers par cons)
    (reverse-list resolved-code)))

(defun resolve-instruction (instr labels-hash)
  "Résout une instruction en remplaçant les symboles par des adresses.
   Reconstruit l'instruction élément par élément."
  (let ((result nil)
        (elem nil)
        (addr nil))
    (dolist (element instr)
      (setq elem element)
      ;; Si c'est un symbole, chercher dans labels-hash
      (if (vm-symbolp elem)
          (progn
            (setq addr (vm-gethash labels-hash elem))
            ;; Si trouvé, utiliser l'adresse, sinon garder le symbole
            (if addr
                (setq elem addr)
                nil))
          nil)
      ;; Ajouter l'élément (résolu ou original) au résultat
      (setq result (vm-cons elem result)))
    ;; Inverser car cons construit à l'envers
    (reverse-list result)))

(defun reverse-list (lst)
  "Inverse une liste de manière itérative.
   Utilise vm-cons pour compatibilité compilation."
  (let ((result nil))
    (dolist (elem lst)
      (setq result (vm-cons elem result)))
    result))

;;; ============================================================================
;;; NORMALISATION - VERSION COMPILABLE
;;; ============================================================================

(defun normalize-instruction (instr)
  "Convertit les keywords en symboles dans une instruction.
   Version simplifiée pour compilation."
  ;; Pour la compilation, on suppose que les instructions sont déjà normalisées
  ;; ou on les traite telles quelles
  instr)

(defun normalize-code (asm-code)
  "Normalise tout le code assembleur.
   Version compilable - itère avec dolist."
  (let ((result nil))
    (dolist (instr asm-code)
      (setq result (vm-cons (normalize-instruction instr) result)))
    (reverse-list result)))

;;; ============================================================================
;;; PRÉTRAITEMENT - VERSION COMPILABLE
;;; ============================================================================

(defun preprocess-code (asm-code code-start)
  "Prétraite le code assembleur: résolution des labels.
   Version compilable retournant deux valeurs via liste."
  (let ((labels-hash (collect-labels asm-code code-start))
        (resolved nil))
    ;; D'abord collecter les labels
    (setq labels-hash (collect-labels asm-code code-start))
    ;; Ensuite résoudre avec la hash-table
    (setq resolved (resolve-labels asm-code labels-hash))
    ;; Retourner une liste (resolved-code . labels-hash)
    ;; Car multiple-value-bind n'est pas facilement compilable
    (vm-cons resolved labels-hash)))

;;; ============================================================================
;;; LOADER PRINCIPAL - VERSION COMPILABLE
;;; ============================================================================

(defun load-code-compilable (asm-code code-start)
  "Charge du code assembleur dans la VM après résolution des labels.
   Version entièrement compilable.
   Note: mem-write écrit dans la VM courante (primitive VM)."
  (let ((preprocessed (preprocess-code asm-code code-start))
        (resolved-code nil)
        (addr code-start))
    ;; Extraire le code résolu du résultat de preprocess-code
    (setq resolved-code (vm-car preprocessed))
    ;; Charger chaque instruction en mémoire
    (dolist (instr resolved-code)
      (mem-write addr instr)
      (setq addr (+ addr 1)))
    code-start))
