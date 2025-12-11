;;;; vm.lisp
;;;; Machine virtuelle pour exécuter du code assembleur MIPS

(load "src/asm-ops.lisp")  ; Charger les opérations depuis src/

;;; ============================================================================
;;; GESTION DU TAS DYNAMIQUE (PHASE 9 - CLOSURES)
;;; ============================================================================

(defparameter *heap-pointer* +heap-start+
  "Pointeur courant du tas pour l'allocation dynamique")

(defconstant +heap-limit+ (+ +heap-start+ *heap-size*)
  "Limite supérieure du tas (première adresse invalide)")

(defun reset-heap ()
  "Réinitialise le pointeur du tas"
  (setf *heap-pointer* +heap-start+))

(defun vm-malloc (vm size)
  "Alloue SIZE mots sur le tas et retourne l'adresse.
   Utilise un allocateur simple (bump allocator).
   Pas de garbage collection."
  (when (> (+ *heap-pointer* size) +heap-limit+)
    (error "Heap overflow: tentative d'allocation de ~A mots, espace disponible: ~A"
           size (- +heap-limit+ *heap-pointer*)))
  (let ((addr *heap-pointer*))
    (incf *heap-pointer* size)
    (when (vm-verbose vm)
      (format t "  MALLOC: Allocation de ~A mots à l'adresse ~A~%" size addr))
    addr))

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
  (set-register vm (get-reg :zero) 0)                         ; $zero est toujours 0
  (set-register vm (get-reg :sp) (- *maxmem* *code-size* 1))  ; Stack pointer
  (set-register vm (get-reg :fp) (get-register vm (get-reg :sp)))      ; Frame pointer
  (set-register vm (nth 28 *register-names*) +heap-start+)                ; Global pointer (pour le tas)
  (set-register vm (get-reg :pc) 0)                           ; Program counter
  (set-register vm (get-reg :ra) 0))                          ; Return address

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
  (reset-heap)
  (init-registers vm)
  (init-memory-layout vm))

;;; ============================================================================
;;; GESTION DES REGISTRES
;;; ============================================================================

(defun map-old-register (reg)
  "Convertit les anciens noms de registres vers les nouveaux (compatibilité)"
  (case reg
    (:R0 (get-reg :t0))
    (:R1 (get-reg :t1))
    (:R2 (get-reg :t2))
    (:MEM (get-reg :t3))
    (:GT (nth 36 *register-names*))  ; :$GT
    (:LT (nth 37 *register-names*))  ; :$LT
    (:EQ (nth 38 *register-names*))  ; :$EQ
    (:PL (get-reg :pc))
    (:FP (get-reg :fp))
    (:SP (get-reg :sp))
    (:HP (nth 28 *register-names*))  ; :$GP
    (t reg)))  ; Si pas de mapping, retourner tel quel

(defun get-register (vm reg)
  "Lit la valeur d'un registre"
  (let ((mapped-reg (map-old-register reg)))
    (unless (register-p mapped-reg)
      (error "Registre invalide: ~A" reg))
    (gethash mapped-reg (vm-registers vm))))

(defun set-register (vm reg value)
  "Écrit une valeur dans un registre"
  (let ((mapped-reg (map-old-register reg)))
    (unless (register-p mapped-reg)
      (error "Registre invalide: ~A" reg))
    (setf (gethash mapped-reg (vm-registers vm)) value)))

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
  (let ((pc (get-register vm (get-reg :pc))))
    (mem-read vm pc)))

(defun get-value (vm operand)
  "Récupère la valeur d'un opérande (registre ou valeur immédiate)"
  (cond
    ;; Si c'est un registre, récupérer sa valeur
    ((register-p operand)
     (let ((val (get-register vm operand))
           (zero-reg (get-reg :zero)))
       ;; $zero est toujours 0 (convention MIPS)
       (if (eq operand zero-reg) 0 val)))
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
  (let ((zero-reg (get-reg :zero)))
    (unless (eq operand zero-reg)
      (set-register vm operand value))))

(defun execute-instruction (vm instr)
  "Exécute une instruction"
  (when (vm-verbose vm)
    (format t "~%[~A] Exécution: ~A~%" 
            (vm-instruction-count vm) 
            (format-instruction instr)))
  
  (let ((opcode (first instr))
        (args (rest instr)))
    ;; PHASE 9 FIX: Ignorer les LABEL (utilisés pour les sauts, pas pour l'exécution)
    (when (eq opcode :LABEL)
      (return-from execute-instruction))
    
    (case opcode
      ;; Instructions arithmétiques style MIPS
      ;; Format MIPS: (ADD src1 src2 dest) -> dest = src1 + src2
      ;; Format ancien (compatibilité): (ADD src dest) -> dest = dest + src
      (:ADD (if (= (length args) 3)
                ;; Format MIPS (3 opérandes)
                (let* ((src1 (first args))
                       (src2 (second args))
                       (dest (third args))
                       (val1 (get-value vm src1))
                       (val2 (get-value vm src2)))
                  (set-value vm dest (+ val1 val2)))
                ;; Format ancien (2 opérandes) - compatibilité
                (let* ((src (first args))
                       (dest (second args))
                       (val-src (get-value vm src))
                       (val-dest (get-value vm dest)))
                  (set-value vm dest (+ val-dest val-src)))))
      
      ;; Format: (ADDI src imm dest) -> dest = src + imm
      (:ADDI (let* ((src (first args))
                    (imm (second args))
                    (dest (third args))
                    (val (get-value vm src)))
               (set-value vm dest (+ val imm))))
      
      (:SUB (if (= (length args) 3)
                ;; Format MIPS (3 opérandes)
                (let* ((src1 (first args))
                       (src2 (second args))
                       (dest (third args))
                       (val1 (get-value vm src1))
                       (val2 (get-value vm src2)))
                  (set-value vm dest (- val1 val2)))
                ;; Format ancien (2 opérandes) - compatibilité
                (let* ((src (first args))
                       (dest (second args))
                       (val-src (get-value vm src))
                       (val-dest (get-value vm dest)))
                  (set-value vm dest (- val-dest val-src)))))
      
      ;; MUL style MIPS: résultat dans $hi:$lo
      ;; Format MIPS: (MUL src1 src2) -> $hi:$lo = src1 * src2
      ;; Format ancien: (MUL src dest) -> dest = dest * src
      (:MUL (if (= (length args) 2)
                ;; Format MIPS (2 opérandes) -> résultat dans $hi:$lo
                (let* ((src1 (first args))
                       (src2 (second args))
                       (val1 (get-value vm src1))
                       (val2 (get-value vm src2))
                       (result (* val1 val2)))
                  ;; Pour simplifier, on met tout dans $lo
                  (set-value vm :$lo result)
                  (set-value vm :$hi 0))
                ;; Sinon traiter comme multiplication directe (ancien)
                (let* ((src (first args))
                       (dest (second args))
                       (val-src (get-value vm src))
                       (val-dest (get-value vm dest)))
                  (set-value vm dest (* val-dest val-src)))))
      
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
      ;; LW: Load Word - (LW dest base offset) -> dest = MEM[base + offset]
      ;; PHASE 9 FIX: L'ordre correct généré par le compilateur est (LW dest base offset)
      (:LW (let* ((dest (first args))
                  (base (second args))
                  (offset (third args))
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
                 (code-start (calculate-code-start vm))
                 (pc-reg (get-reg :pc))
                 (target-addr (if (>= label code-start)
                                  label
                                  (+ code-start label))))
            (set-register vm pc-reg target-addr)
            (return-from execute-instruction)))
      
      ;; BEQ: Branch if Equal
      (:BEQ (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (= val1 val2)
                (let* ((code-start (calculate-code-start vm))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register vm pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BNE: Branch if Not Equal
      (:BNE (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (/= val1 val2)
                (let* ((code-start (calculate-code-start vm))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register vm pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BLT: Branch if Less Than
      (:BLT (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (< val1 val2)
                (let* ((code-start (calculate-code-start vm))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register vm pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BGT: Branch if Greater Than
      (:BGT (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (> val1 val2)
                (let* ((code-start (calculate-code-start vm))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register vm pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; JAL: Jump And Link (appel de fonction MIPS)
      ;; Format: (JAL label) où label peut être une adresse absolue ou relative
      ;; Effet: $ra = $pc + 1; $pc = label (si absolu) ou code-start + label (si relatif)
      (:JAL (let* ((label (first args))
                   (code-start (calculate-code-start vm))
                   (pc-reg (get-reg :pc))
                   (ra-reg (get-reg :ra))
                   (return-addr (1+ (get-register vm pc-reg)))
                   ;; Si label >= code-start, c'est déjà une adresse absolue
                   (target-addr (if (>= label code-start) label (+ code-start label))))
              (when (vm-verbose vm)
                (format t "  JAL: Sauvegarde $ra=~A, saut vers ~A~%" 
                        return-addr target-addr))
              ;; Sauvegarder l'adresse de retour dans $ra
              (set-register vm ra-reg return-addr)
              ;; Sauter au label
              (set-register vm pc-reg target-addr)
              (return-from execute-instruction)))
      
      ;; JR: Jump Register (retour de fonction MIPS)
      ;; Format: (JR $rs)
      ;; Effet: $pc = $rs
      (:JR (let* ((reg (first args))
                  (target-addr (get-value vm reg))
                  (pc-reg (get-reg :pc))
                  (current-pc (get-register vm pc-reg)))
              (when (vm-verbose vm)
                (format t "~%*** JR DEBUG ***~%")
                (format t "  Current PC: ~A~%" current-pc)
                (format t "  Register ~A contains: ~A~%" reg target-addr)
                (format t "  Jumping to: ~A~%" target-addr)
                (format t "******************~%"))
              ;; Sauter à l'adresse contenue dans le registre
              (set-register vm pc-reg target-addr)
              (return-from execute-instruction)))
      
      ;; JALR: Jump And Link Register (PHASE 9 - appel de closure)
      ;; Format: (JALR $rs)
      ;; Effet: $ra = $pc + 1, $pc = $rs
      (:JALR (let* ((reg (first args))
                    (target-addr (get-value vm reg))
                    (pc-reg (get-reg :pc))
                    (ra-reg (get-reg :ra))
                    (current-pc (get-register vm pc-reg))
                    (return-addr (1+ current-pc)))
               (when (vm-verbose vm)
                 (format t "~%*** JALR DEBUG ***~%")
                 (format t "  Current PC: ~A~%" current-pc)
                 (format t "  Target register: ~A -> address ~A~%" reg target-addr)
                 (format t "  Return address ($ra): ~A~%" return-addr)
                 (format t "******************~%"))
               ;; Sauvegarder l'adresse de retour dans $ra
               (set-register vm ra-reg return-addr)
               ;; Sauter à l'adresse dans le registre
               (set-register vm pc-reg target-addr)
               (return-from execute-instruction)))
      
      ;; Compatibilité avec ancien format
      (:JMP (let* ((label (first args))
                   (code-start (calculate-code-start vm))
                   (pc-reg (get-reg :pc))
                   (target-addr (if (>= label code-start) label (+ code-start label))))
              (set-register vm pc-reg target-addr)
              (return-from execute-instruction)))
      
      (:JZ (when (= 1 (get-register vm (get-reg :eq)))
             (let* ((label (first args))
                    (code-start (calculate-code-start vm))
                    (pc-reg (get-reg :pc)))
               (set-register vm pc-reg (+ code-start label))
               (return-from execute-instruction))))
      
      (:JNZ (when (= 0 (get-register vm (get-reg :eq)))
              (let* ((label (first args))
                     (code-start (calculate-code-start vm))
                     (pc-reg (get-reg :pc)))
                (set-register vm pc-reg (+ code-start label))
                (return-from execute-instruction))))
      
      (:JGT (when (= 1 (get-register vm (get-reg :gt)))
              (let* ((label (first args))
                     (code-start (calculate-code-start vm))
                     (pc-reg (get-reg :pc)))
                (set-register vm pc-reg (+ code-start label))
                (return-from execute-instruction))))
      
      (:JLT (when (= 1 (get-register vm (get-reg :lt)))
              (let* ((label (first args))
                     (code-start (calculate-code-start vm))
                     (pc-reg (get-reg :pc)))
                (set-register vm pc-reg (+ code-start label))
                (return-from execute-instruction))))
      
      ;; Comparaison (pour compatibilité)
      (:CMP (let* ((src1 (first args))
                   (src2 (second args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2))
                   (eq-reg (get-reg :eq))
                   (lt-reg (get-reg :lt))
                   (gt-reg (get-reg :gt)))
              (set-register vm eq-reg (if (= val1 val2) 1 0))
              (set-register vm lt-reg (if (< val1 val2) 1 0))
              (set-register vm gt-reg (if (> val1 val2) 1 0))))
      
      ;; SLT: Set on Less Than (style MIPS)
      (:SLT (let* ((src1 (first args))
                   (src2 (second args))
                   (dest (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (set-value vm dest (if (< val1 val2) 1 0))))
      
      ;; ======================================================================
      ;; INSTRUCTIONS TAS DYNAMIQUE (PHASE 9 - CLOSURES)
      ;; ======================================================================
      
      ;; MALLOC: Alloue de la mémoire sur le tas
      ;; Format: (MALLOC size result-reg)
      ;; Effet: result-reg = adresse allouée sur le tas
      (:MALLOC (let* ((size (first args))
                      (result-reg (second args))
                      (size-val (get-value vm size))
                      (addr (vm-malloc vm size-val)))
                 (set-value vm result-reg addr)))
      
      ;; LOAD-HEAP: Charge une valeur depuis le tas
      ;; Format: (LOAD-HEAP addr-reg offset result-reg)
      ;; Effet: result-reg = MEM[addr-reg + offset]
      (:LOAD-HEAP (let* ((addr-reg (first args))
                         (offset (second args))
                         (result-reg (third args))
                         (base-addr (get-value vm addr-reg))
                         (offset-val (get-value vm offset))
                         (address (+ base-addr offset-val))
                         (val (mem-read vm address)))
                    (when (vm-verbose vm)
                      (format t "  LOAD-HEAP: Lecture à l'adresse ~A (base=~A + offset=~A) -> ~A~%"
                              address base-addr offset-val val))
                    (set-value vm result-reg val)))
      
      ;; STORE-HEAP: Stocke une valeur dans le tas
      ;; Format: (STORE-HEAP value-reg addr-reg offset)
      ;; Effet: MEM[addr-reg + offset] = value-reg
      (:STORE-HEAP (let* ((value-reg (first args))
                          (addr-reg (second args))
                          (offset (third args))
                          (val (get-value vm value-reg))
                          (base-addr (get-value vm addr-reg))
                          (offset-val (get-value vm offset))
                          (address (+ base-addr offset-val)))
                     (when (vm-verbose vm)
                       (format t "  STORE-HEAP: Écriture de ~A à l'adresse ~A (base=~A + offset=~A)~%"
                               val address base-addr offset-val))
                     (mem-write vm address val)))
      
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
  (let ((pc-reg (get-reg :pc)))
    (set-register vm pc-reg (1+ (get-register vm pc-reg)))))

;;; ============================================================================
;;; BOUCLE PRINCIPALE
;;; ============================================================================

(defun run-vm (vm &key (max-instructions 100000000))
  "Exécute la VM jusqu'à HALT ou erreur"
  (setf (vm-state vm) :running)
  (setf (vm-instruction-count vm) 0)
  
  (handler-case
      (loop while (eq (vm-state vm) :running)
            do (when (>= (vm-instruction-count vm) max-instructions)
                 (error "Limite d'instructions atteinte: ~A" max-instructions))
               (let ((instr (fetch-instruction vm)))
                 (when (or (not instr) (and (numberp instr) (zerop instr)))
                   (error "Instruction nulle à $pc=~A" (get-register vm (get-reg :pc))))
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
          get-value set-value calculate-code-start
          ;; Heap management (Phase 9)
          reset-heap vm-malloc *heap-pointer* +heap-limit+))
