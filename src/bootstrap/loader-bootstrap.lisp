;;;; loader-bootstrap.lisp
;;;; Version bootstrap du chargeur de code assembleur (Phase 10)
;;;; Adapté pour compilation MIPS - sans dépendances natives

;;; Chargement des primitives
(load "src/primitives.lisp")

;;; ============================================================================
;;; RÉSOLUTION DES LABELS (Version Bootstrap)
;;; ============================================================================

(defun collect-labels-bootstrap (asm-code code-start)
  "Collecte tous les labels et leurs positions ABSOLUES dans le code.
   Retourne une liste d'association ((label . adresse) ...) au lieu d'une hash-table."
  (labels ((collect-iter (code pos labels)
             (cond
               ((null code) labels)
               ((and (listp (car code)) 
                     (eq (caar code) :LABEL))
                ;; C'est un label, l'ajouter à la liste
                (collect-iter (cdr code) 
                             pos 
                             (my-acons (cadar code) 
                                      (+ code-start pos) 
                                      labels)))
               (t 
                ;; Sinon, incrémenter la position
                (collect-iter (cdr code) (+ pos 1) labels)))))
    (collect-iter asm-code 0 '())))

(defun resolve-labels-bootstrap (asm-code labels)
  "Remplace les références symboliques par des adresses.
   Utilise une liste d'association au lieu d'une hash-table."
  (labels ((resolve-element (element)
             (if (and (symbolp element)
                      (my-assoc element labels))
                 ;; Remplacer le symbole par son adresse
                 (cdr (my-assoc element labels))
                 ;; Sinon garder l'élément tel quel
                 element))
           
           (resolve-instruction (instr)
             (my-mapcar #'resolve-element instr))
           
           (resolve-iter (code result)
             (cond
               ((null code) (my-nreverse result))
               ;; Ne pas inclure les labels dans le code final
               ((and (listp (car code)) 
                     (eq (caar code) :LABEL))
                (resolve-iter (cdr code) result))
               (t 
                (resolve-iter (cdr code) 
                             (cons (resolve-instruction (car code)) result))))))
    
    (resolve-iter asm-code '())))

;;; ============================================================================
;;; PARSING ET VALIDATION (Version Bootstrap)
;;; ============================================================================

(defun parse-asm-bootstrap (code)
  "Parse et valide le code assembleur.
   Version simplifiée sans gestion d'erreur."
  (cond
    ;; Si c'est déjà une liste d'instructions
    ((and (listp code) (my-every #'listp code))
     code)
    ;; Si c'est une seule instruction
    ((listp code)
     (list code))
    ;; Sinon retourner NIL (au lieu d'error)
    (t nil)))

(defun preprocess-code-bootstrap (asm-code code-start)
  "Prétraite le code assembleur (résolution des labels, etc.).
   Retourne (resolved-code . labels) au lieu de multiple values."
  (let* ((parsed (parse-asm-bootstrap asm-code))
         (labels (collect-labels-bootstrap parsed code-start))
         (resolved (resolve-labels-bootstrap parsed labels)))
    (cons resolved labels)))

;;; ============================================================================
;;; CHARGEMENT EN MÉMOIRE (Version Bootstrap Simplifiée)
;;; ============================================================================

(defun calculate-code-start-bootstrap (vm)
  "Calcule l'adresse de début de la zone code.
   Version identique à l'original - pure arithmétique."
  (- *maxmem* *code-size*))

(defun load-code-bootstrap (vm asm-code)
  "Charge le code assembleur dans la mémoire de la VM.
   Version simplifiée sans verbose ni messages debug.
   Ajoute automatiquement HALT à la fin pour éviter l'erreur 'Instruction nulle'."
  (let* ((code-start (calculate-code-start-bootstrap vm))
         ;; Ajouter HALT à la fin du code pour arrêter proprement (avec :HALT)
         (asm-code-with-halt (my-append asm-code '((:HALT))))
         (result (preprocess-code-bootstrap asm-code-with-halt code-start))
         (resolved-code (car result))
         (labels (cdr result)))
    
    ;; Charger dans la mémoire
    (labels ((load-iter (code addr)
               (cond
                 ((null code) addr)
                 (t
                  (mem-write vm addr (car code))
                  (load-iter (cdr code) (+ addr 1))))))
      
      (load-iter resolved-code code-start))
    
    ;; Initialiser $pc au début du code
    (set-register vm (get-reg :pc) code-start)
    
    ;; Retourner (resolved-code . labels)
    (cons resolved-code labels)))

;;; ============================================================================
;;; FONCTION LOAD-AND-RUN SIMPLIFIÉE
;;; ============================================================================

(defun load-and-run-bootstrap (vm asm-code)
  "Charge et exécute du code assembleur (version bootstrap simplifiée)."
  (load-code-bootstrap vm asm-code)
  (run-vm vm)
  vm)

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(load-code-bootstrap 
          load-and-run-bootstrap
          parse-asm-bootstrap 
          preprocess-code-bootstrap
          collect-labels-bootstrap 
          resolve-labels-bootstrap
          calculate-code-start-bootstrap))

;;; ============================================================================
;;; MESSAGE DE CHARGEMENT
;;; ============================================================================

(format t "~%✅ Loader Bootstrap chargé (Phase 10 Étape 2).~%")
(format t "   Fonctions disponibles:~%")
(format t "   - (load-code-bootstrap vm asm-code)~%")
(format t "   - (load-and-run-bootstrap vm asm-code)~%")
(format t "   - (calculate-code-start-bootstrap vm)~%")
(format t "   - (collect-labels-bootstrap code start)~%")
(format t "   - (resolve-labels-bootstrap code labels)~%~%")
