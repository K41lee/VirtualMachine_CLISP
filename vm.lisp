;;;; vm.lisp
;;;; Machine virtuelle pour l'exécution de code assembleur

(load "asm-ops.lisp")

;;; ============================================================================
;;; STRUCTURE DE LA VM
;;; ============================================================================

(defstruct vm
  "Structure représentant la machine virtuelle"
  (memory (make-array *maxmem* :initial-element 0)
          :type (simple-array t (*)))
  (registers (make-hash-table :test 'eq)
             :type hash-table)
  (state :ready
         :type keyword)  ; :ready, :running, :halted, :error
  (instruction-count 0
                     :type integer)
  (verbose nil
           :type boolean))

;;; ============================================================================
;;; INITIALISATION
;;; ============================================================================

(defun make-new-vm (&key (verbose nil))
  "Crée et initialise une nouvelle VM"
  (let ((vm (make-vm :verbose verbose)))
    (init-registers vm)
    (init-memory-layout vm)
    vm))

(defun init-registers (vm)
  "Initialise tous les registres de la VM (style MIPS)"
  (dolist (reg *register-names*)
    (setf (gethash reg (vm-registers vm)) 0))
  ;; Initialisation spécifique MIPS
  (set-register vm :$zero 0)                         ; $zero est toujours 0
  (set-register vm :$sp (- *maxmem* *code-size* 1))  ; Stack pointer
  (set-register vm :$fp (get-register vm :$sp))      ; Frame pointer
  (set-register vm :$gp +heap-start+)                ; Global pointer (pour le tas)
  (set-register vm :$pc 0)                           ; Program counter
  (set-register vm :$ra 0))                          ; Return address

(defun init-memory-layout (vm)
  "Initialise la disposition de la mémoire"
  ;; La mémoire est déjà initialisée à 0 par make-array
  ;; On pourrait ajouter des marqueurs de zones ici si nécessaire
  (when (vm-verbose vm)
    (format t "Mémoire initialisée (architecture MIPS):~%")
    (format t "  Registres: ~A à ~A~%" +registers-start+ 
            (+ +registers-start+ +registers-size+ -1))
    (format t "  Tas: ~A à ~A~%" +heap-start+ 
            (+ +heap-start+ *heap-size* -1))
    (format t "  Pile ($sp): ~A (descend)~%" (get-register vm :$sp))
    (format t "  Code: ~A à ~A~%" (- *maxmem* *code-size*) 
            (- *maxmem* 1))))

(defun reset-vm (vm)
  "Réinitialise la VM à son état initial"
  (setf (vm-memory vm) (make-array *maxmem* :initial-element 0))
  (setf (vm-state vm) :ready)
  (setf (vm-instruction-count vm) 0)
  (init-registers vm)
  (init-memory-layout vm))

;;; ============================================================================
;;; GESTION DES REGISTRES
;;; ============================================================================

(defun get-register (vm reg)
  "Lit la valeur d'un registre"
  (unless (register-p reg)
    (error "Registre invalide: ~A" reg))
  (gethash reg (vm-registers vm)))

(defun set-register (vm reg value)
  "Écrit une valeur dans un registre"
  (unless (register-p reg)
    (error "Registre invalide: ~A" reg))
  (setf (gethash reg (vm-registers vm)) value))

(defun dump-registers (vm)
  "Affiche tous les registres"
  (format t "~%=== REGISTRES ===~%")
  (dolist (reg *register-names*)
    (format t "~6A: ~A~%" reg (get-register vm reg))))

;;; ============================================================================
;;; GESTION DE LA MÉMOIRE
;;; ============================================================================

(defun check-memory-bounds (vm addr)
  "Vérifie que l'adresse est dans les limites de la mémoire"
  (when (or (< addr 1) (>= addr *maxmem*))
    (error "Adresse mémoire hors limites: ~A (0 < addr < ~A)" addr *maxmem*))
  t)

(defun mem-read (vm addr)
  "Lit une valeur à l'adresse mémoire"
  (check-memory-bounds vm addr)
  (aref (vm-memory vm) addr))

(defun mem-write (vm addr value)
  "Écrit une valeur à l'adresse mémoire"
  (check-memory-bounds vm addr)
  (setf (aref (vm-memory vm) addr) value))

(defun alloc-memory (vm size)
  "Alloue de la mémoire sur le tas et retourne l'adresse"
  (let ((addr (get-register vm :$gp)))
    (when (>= (+ addr size) (get-register vm :$sp))
      (error "Dépassement de mémoire: tas et pile se rencontrent"))
    (set-register vm :$gp (+ addr size))
    addr))

(defun dump-memory (vm start end)
  "Affiche une zone de mémoire"
  (format t "~%=== MÉMOIRE [~A-~A] ===~%" start end)
  (loop for addr from start to (min end (1- *maxmem*))
        for value = (aref (vm-memory vm) addr)
        unless (zerop value)
        do (format t "~6A: ~A~%" addr value)))

;;; ============================================================================
;;; UTILITAIRES CODE
;;; ============================================================================

(defun calculate-code-start (vm)
  "Calcule l'adresse de début de la zone code"
  (declare (ignore vm))
  (- *maxmem* *code-size*))

;;; ============================================================================
;;; GESTION DE LA PILE
;;; ============================================================================

(defun push-stack (vm value)
  "Empile une valeur sur la pile"
  (let ((sp (get-register vm :$sp)))
    (when (<= sp (+ +heap-start+ *heap-size*))
      (error "Débordement de pile"))
    (set-register vm :$sp (1- sp))
    (mem-write vm sp value)
    (when (vm-verbose vm)
      (format t "  PUSH: ~A à $sp=~A~%" value sp))))

(defun pop-stack (vm)
  "Dépile et retourne une valeur de la pile"
  (let* ((sp (get-register vm :$sp))
         (value (mem-read vm (1+ sp))))
    (set-register vm :$sp (1+ sp))
    (when (vm-verbose vm)
      (format t "  POP: ~A de $sp=~A~%" value (1+ sp)))
    value))

(defun peek-stack (vm &optional (offset 0))
  "Lit une valeur de la pile sans dépiler"
  (let ((sp (get-register vm :$sp)))
    (mem-read vm (+ sp offset 1))))

(defun dump-stack (vm &optional (depth 10))
  "Affiche le sommet de la pile"
  (format t "~%=== PILE (sommet ~A éléments) ===~%" depth)
  (let ((sp (get-register vm :$sp)))
    (loop for i from 1 to depth
          for addr = (+ sp i)
          while (< addr *maxmem*)
          for value = (aref (vm-memory vm) addr)
          do (format t "$sp+~A (~6A): ~A~%" i addr value))))

;;; ============================================================================
;;; EXÉCUTION D'INSTRUCTIONS
;;; ============================================================================

(defun fetch-instruction (vm)
  "Récupère l'instruction à l'adresse $pc"
  (let ((pc (get-register vm :$pc)))
    (mem-read vm pc)))

(defun get-value (vm operand)
  "Récupère la valeur d'un opérande (registre ou valeur immédiate)"
  (cond
    ;; Si c'est un registre, récupérer sa valeur
    ((register-p operand)
     (let ((val (get-register vm operand)))
       ;; $zero est toujours 0 (convention MIPS)
       (if (eq operand :$zero) 0 val)))
    ;; Si c'est un nombre, le retourner directement
    ((numberp operand)
     operand)
    ;; Sinon erreur
    (t (error "Opérande invalide: ~A (doit être un registre ou un nombre)" operand))))

(defun set-value (vm operand value)
  "Définit la valeur d'un opérande (doit être un registre)"
  (unless (register-p operand)
    (error "La destination doit être un registre: ~A" operand))
  ;; $zero ne peut pas être modifié (convention MIPS)
  (unless (eq operand :$zero)
    (set-register vm operand value)))

(defun execute-instruction (vm instr)
  "Exécute une instruction"
  (when (vm-verbose vm)
    (format t "~%[~A] Exécution: ~A~%" 
            (vm-instruction-count vm) 
            (format-instruction instr)))
  
  (let ((opcode (first instr))
        (args (rest instr)))
    (case opcode
      ;; Instructions arithmétiques style MIPS
      ;; Format: (ADD src1 src2 dest) -> dest = src1 + src2
      (:ADD (let* ((src1 (first args))
                   (src2 (second args))
                   (dest (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (set-value vm dest (+ val1 val2))))
      
      ;; Format: (ADDI src imm dest) -> dest = src + imm
      (:ADDI (let* ((src (first args))
                    (imm (second args))
                    (dest (third args))
                    (val (get-value vm src)))
               (set-value vm dest (+ val imm))))
      
      (:SUB (let* ((src1 (first args))
                   (src2 (second args))
                   (dest (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (set-value vm dest (- val1 val2))))
      
      ;; MUL style MIPS: résultat dans $hi:$lo
      (:MUL (let* ((src1 (first args))
                   (src2 (second args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2))
                   (result (* val1 val2)))
              ;; Pour simplifier, on met tout dans $lo
              (set-value vm :$lo result)
              (set-value vm :$hi 0)))
      
      ;; DIV style MIPS: quotient dans $lo, reste dans $hi
      (:DIV (let* ((src1 (first args))
                   (src2 (second args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (zerop val2)
                (error "Division par zéro"))
              (set-value vm :$lo (truncate val1 val2))
              (set-value vm :$hi (mod val1 val2))))
      
      ;; MFLO: Move From LO
      (:MFLO (let ((dest (first args)))
               (set-value vm dest (get-value vm :$lo))))
      
      ;; MFHI: Move From HI
      (:MFHI (let ((dest (first args)))
               (set-value vm dest (get-value vm :$hi))))
      
      ;; Instructions de transfert
      (:MOVE (let* ((src (first args))
                    (dest (second args))
                    (val (get-value vm src)))
               (set-value vm dest val)))
      
      ;; LI: Load Immediate (style MIPS)
      (:LI (let* ((imm (first args))
                  (dest (second args)))
             (set-value vm dest imm)))
      
      ;; Compatibilité avec ancien format
      (:LOADI (let* ((imm (first args))
                     (dest (second args)))
                (set-value vm dest imm)))
      
      ;; Instructions mémoire style MIPS
      ;; LW: Load Word - (LW base offset dest) -> dest = MEM[base + offset]
      (:LW (let* ((base (first args))
                  (offset (second args))
                  (dest (third args))
                  (address (+ (get-value vm base) offset))
                  (val (mem-read vm address)))
             (set-value vm dest val)))
      
      ;; SW: Store Word - (SW src base offset) -> MEM[base + offset] = src
      (:SW (let* ((src (first args))
                  (base (second args))
                  (offset (third args))
                  (val (get-value vm src))
                  (address (+ (get-value vm base) offset)))
             (mem-write vm address val)))
      
      ;; Compatibilité avec ancien format
      (:LOAD (let* ((addr (first args))
                    (dest (second args))
                    (address (get-value vm addr))
                    (val (mem-read vm address)))
               (set-value vm dest val)))
      
      (:STORE (let* ((src (first args))
                     (addr (second args))
                     (val (get-value vm src))
                     (address (get-value vm addr)))
                (mem-write vm address val)))
      
      ;; Instructions pile
      (:PUSH (let* ((src (first args))
                    (val (get-value vm src)))
               (push-stack vm val)))
      
      (:POP (let ((dest (first args)))
              (set-value vm dest (pop-stack vm))))
      
      ;; Instructions de saut style MIPS
      ;; J: Jump (saut inconditionnel)
      (:J (let* ((label (first args))
                 (code-start (calculate-code-start vm)))
            (set-register vm :$pc (+ code-start label))
            (return-from execute-instruction)))
      
      ;; BEQ: Branch if Equal
      (:BEQ (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (= val1 val2)
                (let ((code-start (calculate-code-start vm)))
                  (set-register vm :$pc (+ code-start label))
                  (return-from execute-instruction)))))
      
      ;; BNE: Branch if Not Equal
      (:BNE (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (/= val1 val2)
                (let ((code-start (calculate-code-start vm)))
                  (set-register vm :$pc (+ code-start label))
                  (return-from execute-instruction)))))
      
      ;; BLT: Branch if Less Than
      (:BLT (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (< val1 val2)
                (let ((code-start (calculate-code-start vm)))
                  (set-register vm :$pc (+ code-start label))
                  (return-from execute-instruction)))))
      
      ;; BGT: Branch if Greater Than
      (:BGT (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (> val1 val2)
                (let ((code-start (calculate-code-start vm)))
                  (set-register vm :$pc (+ code-start label))
                  (return-from execute-instruction)))))
      
      ;; Compatibilité avec ancien format
      (:JMP (let* ((label (first args))
                   (code-start (calculate-code-start vm)))
              (set-register vm :$pc (+ code-start label))
              (return-from execute-instruction)))
      
      (:JZ (when (= 1 (get-register vm :$eq))
             (let* ((label (first args))
                    (code-start (calculate-code-start vm)))
               (set-register vm :$pc (+ code-start label))
               (return-from execute-instruction))))
      
      (:JNZ (when (= 0 (get-register vm :$eq))
              (let* ((label (first args))
                     (code-start (calculate-code-start vm)))
                (set-register vm :$pc (+ code-start label))
                (return-from execute-instruction))))
      
      (:JGT (when (= 1 (get-register vm :$gt))
              (let* ((label (first args))
                     (code-start (calculate-code-start vm)))
                (set-register vm :$pc (+ code-start label))
                (return-from execute-instruction))))
      
      (:JLT (when (= 1 (get-register vm :$lt))
              (let* ((label (first args))
                     (code-start (calculate-code-start vm)))
                (set-register vm :$pc (+ code-start label))
                (return-from execute-instruction))))
      
      ;; Comparaison (pour compatibilité)
      (:CMP (let* ((src1 (first args))
                   (src2 (second args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (set-register vm :$eq (if (= val1 val2) 1 0))
              (set-register vm :$lt (if (< val1 val2) 1 0))
              (set-register vm :$gt (if (> val1 val2) 1 0))))
      
      ;; SLT: Set on Less Than (style MIPS)
      (:SLT (let* ((src1 (first args))
                   (src2 (second args))
                   (dest (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (set-value vm dest (if (< val1 val2) 1 0))))
      
      ;; Contrôle
      (:HALT (setf (vm-state vm) :halted)
             (when (vm-verbose vm)
               (format t "~%VM HALTED après ~A instructions~%" 
                       (vm-instruction-count vm)))
             (return-from execute-instruction))
      
      (:NOP ) ; Ne rien faire
      
      (:PRINT (let* ((src (first args))
                     (val (get-value vm src)))
                (format t ">>> ~A~%" val)))
      
      (t (error "Opcode non implémenté: ~A" opcode))))
  
  ;; Incrémenter le pointeur d'instruction ($pc)
  (set-register vm :$pc (1+ (get-register vm :$pc))))

;;; ============================================================================
;;; BOUCLE PRINCIPALE
;;; ============================================================================

(defun run-vm (vm &key (max-instructions 100000))
  "Exécute la VM jusqu'à HALT ou erreur"
  (setf (vm-state vm) :running)
  (setf (vm-instruction-count vm) 0)
  
  (handler-case
      (loop while (eq (vm-state vm) :running)
            do (when (>= (vm-instruction-count vm) max-instructions)
                 (error "Limite d'instructions atteinte: ~A" max-instructions))
               (let ((instr (fetch-instruction vm)))
                 (when (or (not instr) (and (numberp instr) (zerop instr)))
                   (error "Instruction nulle à $pc=~A" (get-register vm :$pc)))
                 (execute-instruction vm instr)
                 (incf (vm-instruction-count vm))))
    (error (e)
      (setf (vm-state vm) :error)
      (format t "~%ERREUR: ~A~%" e)
      (dump-registers vm)
      (dump-stack vm)
      (return-from run-vm nil)))
  
  (when (vm-verbose vm)
    (format t "~%Exécution terminée: ~A instructions~%" 
            (vm-instruction-count vm)))
  t)

;;; ============================================================================
;;; EXPORT
;;; ============================================================================

(export '(make-new-vm reset-vm run-vm
          get-register set-register dump-registers
          mem-read mem-write dump-memory
          push-stack pop-stack peek-stack dump-stack
          vm-state vm-instruction-count vm-verbose
          get-value set-value calculate-code-start))
