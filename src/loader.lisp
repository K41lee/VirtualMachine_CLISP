;;;; loader.lisp
;;;; Chargeur de code assembleur dans la VM

;; Note: vm.lisp est chargé par main.lisp, pas ici

;;; ============================================================================
;;; RÉSOLUTION DES LABELS
;;; ============================================================================

(defun collect-labels (asm-code code-start)
  "Collecte tous les labels et leurs positions ABSOLUES dans le code"
  (let ((labels (make-hash-table :test 'equal))
        (position 0))
    (dolist (instr asm-code)
      (if (and (listp instr) (eq (first instr) :LABEL))
          ;; C'est un label, l'enregistrer avec l'adresse absolue
          (setf (gethash (second instr) labels) (+ code-start position))
          ;; Sinon, incrémenter la position
          (incf position)))
    labels))

(defun resolve-labels (asm-code labels)
  "Remplace les références symboliques par des adresses"
  (let ((resolved-code '()))
    (dolist (instr asm-code)
      (when (not (and (listp instr) (eq (first instr) :LABEL)))
        ;; Ne pas inclure les labels dans le code final
        (let ((resolved-instr 
               (mapcar (lambda (element)
                         (if (and (symbolp element)
                                  (gethash element labels))
                             ;; Remplacer le symbole par son adresse
                             (gethash element labels)
                             ;; Sinon garder l'élément tel quel
                             element))
                       instr)))
          (push resolved-instr resolved-code))))
    (nreverse resolved-code)))

;;; ============================================================================
;;; PARSING ET VALIDATION
;;; ============================================================================

(defun parse-asm (code)
  "Parse et valide le code assembleur"
  (cond
    ;; Si c'est déjà une liste d'instructions
    ((and (listp code) (every #'listp code))
     code)
    ;; Si c'est une seule instruction
    ((listp code)
     (list code))
    ;; Sinon erreur
    (t (error "Format de code invalide: ~A" code))))

(defun preprocess-code (asm-code code-start)
  "Prétraite le code assembleur (résolution des labels, etc.)"
  (let* ((parsed (parse-asm asm-code))
         (labels (collect-labels parsed code-start))
         (resolved (resolve-labels parsed labels)))
    (values resolved labels)))

;;; ============================================================================
;;; CHARGEMENT EN MÉMOIRE
;;; ============================================================================

(defun calculate-code-start (vm)
  "Calcule l'adresse de début de la zone code"
  (- *maxmem* *code-size*))

(defun load-code (vm asm-code &key (verbose nil))
  "Charge le code assembleur dans la mémoire de la VM"
  (let ((code-start (calculate-code-start vm)))
    (multiple-value-bind (resolved-code labels)
        (preprocess-code asm-code code-start)
    
    (when verbose
      (format t "~%=== CHARGEMENT DU CODE ===~%")
      (format t "Nombre d'instructions: ~A~%" (length resolved-code))
      (format t "Labels trouvés: ~A~%" (hash-table-count labels))
      (when (> (hash-table-count labels) 0)
        (format t "Labels:~%")
        (maphash (lambda (name addr)
                   (format t "  ~A -> ~A~%" name addr))
                 labels)))
    
      ;; Valider le code
      (validate-program resolved-code)
      
      ;; Charger dans la mémoire
      (let ((addr 0))
        (dolist (instr resolved-code)
          (mem-write vm (+ code-start addr) instr)
          (when verbose
            (format t "  [~A] ~A~%" addr (format-instruction instr)))
          (incf addr))
        
        ;; Initialiser $pc au début du code
        (set-register vm (get-reg :pc) code-start)
        
        (when verbose
          (format t "Code chargé à partir de l'adresse ~A~%" code-start)
          (format t "$pc initialisé à ~A~%" code-start))
        
        (values resolved-code labels)))))

;;; ============================================================================
;;; CHARGEMENT INCRÉMENTAL
;;; ============================================================================

(defun append-code (vm asm-code &key (verbose nil))
  "Ajoute du code à la suite du code existant"
  (multiple-value-bind (resolved-code labels)
      (preprocess-code asm-code)
    
    (validate-program resolved-code)
    
    ;; Trouver la fin du code existant
    (let* ((code-start (calculate-code-start vm))
           (current-addr code-start))
      ;; Chercher la première cellule vide
      (loop while (and (< current-addr *maxmem*)
                       (not (zerop (mem-read vm current-addr))))
            do (incf current-addr))
      
      ;; Charger le nouveau code
      (let ((offset (- current-addr code-start)))
        (dolist (instr resolved-code)
          (mem-write vm current-addr instr)
          (when verbose
            (format t "  [~A] ~A~%" current-addr (format-instruction instr)))
          (incf current-addr))
        
        (when verbose
          (format t "Code ajouté à partir de l'adresse ~A (offset ~A)~%" 
                  (- current-addr (length resolved-code)) offset))
        
        (values resolved-code labels offset)))))

;;; ============================================================================
;;; UTILITAIRES
;;; ============================================================================

(defun dump-code (vm &optional (max-instructions 50))
  "Désassemble et affiche le code chargé"
  (format t "~%=== CODE ASSEMBLEUR ===~%")
  (let ((code-start (calculate-code-start vm))
        (count 0))
    (loop for addr from code-start below *maxmem*
          while (< count max-instructions)
          for instr = (mem-read vm addr)
          unless (zerop instr)
          do (format t "[~4A] ~A~%" addr (format-instruction instr))
             (incf count))))

(defun load-and-run (vm asm-code &key (verbose nil))
  "Charge et exécute du code assembleur"
  (load-code vm asm-code :verbose verbose)
  (when verbose
    (dump-registers vm)
    (format t "~%=== DÉBUT DE L'EXÉCUTION ===~%"))
  (run-vm vm)
  (when verbose
    (format t "~%=== FIN DE L'EXÉCUTION ===~%")
    (dump-registers vm)
    (dump-stack vm)))

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(load-code append-code dump-code load-and-run
          parse-asm preprocess-code
          collect-labels resolve-labels))
