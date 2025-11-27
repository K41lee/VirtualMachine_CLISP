;;;; vm-compilable.lisp
;;;; Machine virtuelle SIMPLIFIÉE pour compilation vers MIPS
;;;; 
;;;; Ce fichier est une version simplifiée de vm.lisp qui utilise uniquement
;;;; des constructions supportées par notre compilateur:
;;;;   - Pas de DEFSTRUCT (remplacé par variables globales)
;;;;   - Pas de HASH-TABLE (remplacé par arrays avec indices)
;;;;   - Pas de DOLIST/LOOP (remplacé par WHILE)
;;;;   - Pas de FORMAT (supprimé ou commenté)
;;;;   - Keywords remplacés par constantes numériques
;;;;
;;;; Cette version est conçue pour être compilée en MIPS et exécutée
;;;; dans la VM originale (bootstrap: VM₁ s'exécute dans VM₀).

(load "src/asm-ops.lisp")  ; Charger les opérations depuis src/

;;; ============================================================================
;;; CONSTANTES D'ÉTAT DE LA VM
;;; ============================================================================

(defconstant +state-ready+ 0
  "État initial - VM prête à charger un programme")

(defconstant +state-running+ 1
  "État actif - VM en cours d'exécution")

(defconstant +state-halted+ 2
  "État terminé - Exécution normale terminée")

(defconstant +state-error+ 3
  "État d'erreur - Erreur d'exécution")

;;; ============================================================================
;;; VARIABLES GLOBALES DE LA VM (remplace DEFSTRUCT)
;;; ============================================================================

(defvar *vm-memory* (make-array *maxmem* :initial-element 0)
  "Mémoire principale de la VM (pile + tas + données + code)")

(defvar *vm-registers* (make-array 42 :initial-element 0)
  "Registres MIPS (32 généraux + 10 spéciaux). Remplace le hash-table.")

(defvar *vm-state* +state-ready+
  "État courant de la VM")

(defvar *vm-instruction-count* 0
  "Compteur d'instructions exécutées")

(defvar *vm-verbose* nil
  "Mode verbeux pour le débogage")

;;; ============================================================================
;;; GESTION DU TAS DYNAMIQUE (PHASE 9 - CLOSURES)
;;; ============================================================================

(defparameter *heap-pointer* +heap-start+
  "Pointeur courant du tas pour l'allocation dynamique")

(defconstant +heap-limit+ (+ +heap-start+ *heap-size*)
  "Limite supérieure du tas (première adresse invalide)")

(defun reset-heap ()
  "Réinitialise le pointeur du tas"
  (setq *heap-pointer* +heap-start+))

(defun vm-malloc (size)
  "Alloue SIZE mots sur le tas et retourne l'adresse.
   Utilise un allocateur simple (bump allocator).
   Pas de garbage collection."
  (when (> (+ *heap-pointer* size) +heap-limit+)
    (error "Heap overflow"))
  (let ((addr *heap-pointer*))
    (setq *heap-pointer* (+ *heap-pointer* size))
    addr))

;;; ============================================================================
;;; GESTION DES REGISTRES (remplace HASH-TABLE par ARRAY)
;;; ============================================================================

(defun reg-index (reg-symbol)
  "Convertit un symbole de registre en indice dans le tableau *vm-registers*.
   Les 32 premiers indices sont pour les registres généraux MIPS,
   les suivants pour les registres spéciaux (pc, hi, lo, etc.)"
  (case reg-symbol
    ;; Registres généraux MIPS ($0-$31)
    (:$zero 0)  (:$at 1)   (:$v0 2)   (:$v1 3)
    (:$a0 4)    (:$a1 5)   (:$a2 6)   (:$a3 7)
    (:$t0 8)    (:$t1 9)   (:$t2 10)  (:$t3 11)
    (:$t4 12)   (:$t5 13)  (:$t6 14)  (:$t7 15)
    (:$s0 16)   (:$s1 17)  (:$s2 18)  (:$s3 19)
    (:$s4 20)   (:$s5 21)  (:$s6 22)  (:$s7 23)
    (:$t8 24)   (:$t9 25)  (:$k0 26)  (:$k1 27)
    (:$gp 28)   (:$sp 29)  (:$fp 30)  (:$ra 31)
    ;; Registres spéciaux (indices 32+)
    (:pc 32)    (:hi 33)   (:lo 34)
    (:zero 35)  (:at 36)   (:v0 37)   (:v1 38)
    (:sp 39)    (:fp 40)   (:ra 41)
    (t (error "Registre inconnu: ~A" reg-symbol))))

;;; ============================================================================
;;; INITIALISATION
;;; ============================================================================

(defun make-new-vm (&key (verbose nil))
  "Crée et initialise une nouvelle VM (utilise les variables globales)"
  (setq *vm-verbose* verbose)
  (setq *vm-state* +state-ready+)
  (setq *vm-instruction-count* 0)
  (setq *vm-memory* (make-array *maxmem* :initial-element 0))
  (setq *vm-registers* (make-array 42 :initial-element 0))
  (init-registers)
  (init-memory-layout)
  t)

(defun init-registers ()
  "Initialise tous les registres de la VM (style MIPS)"
  ;; Initialise tous les registres à 0 avec WHILE
  (let ((i 0))
    (while (< i 42)
      (setq (aref *vm-registers* i) 0)
      (setq i (+ i 1))))
  ;; Initialisation spécifique MIPS
  (set-register (get-reg :zero) 0)                         ; $zero est toujours 0
  (set-register (get-reg :sp) (- *maxmem* *code-size* 1))  ; Stack pointer
  (set-register (get-reg :fp) (get-register (get-reg :sp)))      ; Frame pointer
  (set-register (nth 28 *register-names*) +heap-start+)                ; Global pointer (pour le tas)
  (set-register (get-reg :pc) 0)                           ; Program counter
  (set-register (get-reg :ra) 0))                          ; Return address

(defun init-memory-layout ()
  "Initialise la disposition de la mémoire"
  ;; La mémoire est déjà initialisée à 0 par make-array
  ;; FORMAT supprimé pour la version compilable
  t)

(defun reset-vm ()
  "Réinitialise la VM à son état initial"
  (setq *vm-memory* (make-array *maxmem* :initial-element 0))
  (setq *vm-state* +state-ready+)
  (setq *vm-instruction-count* 0)
  (reset-heap)
  (init-registers)
  (init-memory-layout))

;;; ============================================================================
;;; ACCÈS AUX REGISTRES (utilise le tableau *vm-registers*)
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

(defun get-register (reg)
  "Lit la valeur d'un registre"
  (let ((mapped-reg (map-old-register reg)))
    (unless (register-p mapped-reg)
      (error "Registre invalide: ~A" reg))
    (aref *vm-registers* (reg-index mapped-reg))))

(defun set-register (reg value)
  "Écrit une valeur dans un registre"
  (let ((mapped-reg (map-old-register reg)))
    (unless (register-p mapped-reg)
      (error "Registre invalide: ~A" reg))
    (setq (aref *vm-registers* (reg-index mapped-reg)) value)))

(defun dump-registers ()
  "Affiche tous les registres (VERSION COMMENTÉE - pas de FORMAT)"
  ;; Cette fonction est désactivée dans la version compilable
  ;; car elle utilise FORMAT et DOLIST
  t)

;;; ============================================================================
;;; ACCÈS À LA MÉMOIRE (utilise le tableau *vm-memory*)
;;; ============================================================================

(defun check-memory-bounds (addr)
  "Vérifie que l'adresse est dans les limites de la mémoire"
  (when (or (< addr 1) (>= addr *maxmem*))
    (error "Adresse mémoire hors limites"))
  t)

(defun mem-read (addr)
  "Lit une valeur à l'adresse mémoire"
  (check-memory-bounds addr)
  (aref *vm-memory* addr))

(defun mem-write (addr value)
  "Écrit une valeur à l'adresse mémoire"
  (check-memory-bounds addr)
  (setq (aref *vm-memory* addr) value))

(defun alloc-memory (size)
  "Alloue de la mémoire sur le tas et retourne l'adresse"
  (let ((addr (get-register :$gp)))
    (when (>= (+ addr size) (get-register :$sp))
      (error "Dépassement de mémoire: tas et pile se rencontrent"))
    (set-register :$gp (+ addr size))
    addr))

(defun dump-memory (start end)
  "Affiche une zone de mémoire (VERSION COMMENTÉE - pas de LOOP ni FORMAT)"
  ;; Cette fonction est désactivée dans la version compilable
  t)

;;; ============================================================================
;;; UTILITAIRES CODE
;;; ============================================================================

(defun calculate-code-start ()
  "Calcule l'adresse de début de la zone code"
  (- *maxmem* *code-size*))

;;; ============================================================================
;;; GESTION DE LA PILE
;;; ============================================================================

(defun push-stack (value)
  "Empile une valeur sur la pile"
  (let ((sp (get-register :$sp)))
    (when (<= sp (+ +heap-start+ *heap-size*))
      (error "Débordement de pile"))
    (set-register :$sp (- sp 1))
    (mem-write sp value)))

(defun pop-stack ()
  "Dépile et retourne une valeur de la pile"
  (let* ((sp (get-register :$sp))
         (value (mem-read (+ sp 1))))
    (set-register :$sp (+ sp 1))
    value))

(defun peek-stack (offset)
  "Lit une valeur de la pile sans dépiler"
  (let ((sp (get-register :$sp)))
    (mem-read (+ sp offset 1))))

(defun dump-stack (depth)
  "Affiche le sommet de la pile (VERSION COMMENTÉE - pas de LOOP ni FORMAT)"
  ;; Cette fonction est désactivée dans la version compilable
  t)

;;; ============================================================================
;;; EXÉCUTION D'INSTRUCTIONS
;;; ============================================================================

(defun fetch-instruction ()
  "Récupère l'instruction à l'adresse $pc"
  (let ((pc (get-register (get-reg :pc))))
    (mem-read pc)))

(defun get-value (operand)
  "Récupère la valeur d'un opérande (registre ou valeur immédiate)"
  (cond
    ;; Si c'est un registre, récupérer sa valeur
    ((register-p operand)
     (let ((val (get-register operand))
           (zero-reg (get-reg :zero)))
       ;; $zero est toujours 0 (convention MIPS)
       (if (eq operand zero-reg) 0 val)))
    ;; Si c'est un nombre, le retourner directement
    ((numberp operand)
     operand)
    ;; Sinon erreur
    (t (error "Opérande invalide"))))

(defun set-value (operand value)
  "Définit la valeur d'un opérande (doit être un registre)"
  (unless (register-p operand)
    (error "La destination doit être un registre"))
  ;; $zero ne peut pas être modifié (convention MIPS)
  (let ((zero-reg (get-reg :zero)))
    (unless (eq operand zero-reg)
      (set-register operand value))))

(defun execute-instruction (instr)
  "Exécute une instruction"
  ;; FORMAT supprimé - version silencieuse)
  
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
                       (val1 (get-value src1))
                       (val2 (get-value src2)))
                  (set-value dest (+ val1 val2)))
                ;; Format ancien (2 opérandes) - compatibilité
                (let* ((src (first args))
                       (dest (second args))
                       (val-src (get-value src))
                       (val-dest (get-value dest)))
                  (set-value dest (+ val-dest val-src)))))
      
      ;; Format: (ADDI src imm dest) -> dest = src + imm
      (:ADDI (let* ((src (first args))
                    (imm (second args))
                    (dest (third args))
                    (val (get-value src)))
               (set-value dest (+ val imm))))
      
      (:SUB (if (= (length args) 3)
                ;; Format MIPS (3 opérandes)
                (let* ((src1 (first args))
                       (src2 (second args))
                       (dest (third args))
                       (val1 (get-value src1))
                       (val2 (get-value src2)))
                  (set-value dest (- val1 val2)))
                ;; Format ancien (2 opérandes) - compatibilité
                (let* ((src (first args))
                       (dest (second args))
                       (val-src (get-value src))
                       (val-dest (get-value dest)))
                  (set-value dest (- val-dest val-src)))))
      
      ;; MUL style MIPS: résultat dans $hi:$lo
      ;; Format MIPS: (MUL src1 src2) -> $hi:$lo = src1 * src2
      ;; Format ancien: (MUL src dest) -> dest = dest * src
      (:MUL (if (= (length args) 2)
                ;; Format MIPS (2 opérandes) -> résultat dans $hi:$lo
                (let* ((src1 (first args))
                       (src2 (second args))
                       (val1 (get-value src1))
                       (val2 (get-value src2))
                       (result (* val1 val2)))
                  ;; Pour simplifier, on met tout dans $lo
                  (set-value :$lo result)
                  (set-value :$hi 0))
                ;; Sinon traiter comme multiplication directe (ancien)
                (let* ((src (first args))
                       (dest (second args))
                       (val-src (get-value src))
                       (val-dest (get-value dest)))
                  (set-value dest (* val-dest val-src)))))
      
      ;; DIV style MIPS: quotient dans $lo, reste dans $hi
      (:DIV (let* ((src1 (first args))
                   (src2 (second args))
                   (val1 (get-value src1))
                   (val2 (get-value src2)))
              (when (zerop val2)
                (error "Division par zéro"))
              (set-value :$lo (truncate val1 val2))
              (set-value :$hi (mod val1 val2))))
      
      ;; MFLO: Move From LO
      (:MFLO (let ((dest (first args)))
               (set-value dest (get-value :$lo))))
      
      ;; MFHI: Move From HI
      (:MFHI (let ((dest (first args)))
               (set-value dest (get-value :$hi))))
      
      ;; Instructions de transfert
      (:MOVE (let* ((src (first args))
                    (dest (second args))
                    (val (get-value src)))
               (set-value dest val)))
      
      ;; LI: Load Immediate (style MIPS)
      (:LI (let* ((imm (first args))
                  (dest (second args)))
             (set-value dest imm)))
      
      ;; Compatibilité avec ancien format
      (:LOADI (let* ((imm (first args))
                     (dest (second args)))
                (set-value dest imm)))
      
      ;; Instructions mémoire style MIPS
      ;; LW: Load Word - (LW dest base offset) -> dest = MEM[base + offset]
      ;; PHASE 9 FIX: L'ordre correct généré par le compilateur est (LW dest base offset)
      (:LW (let* ((dest (first args))
                  (base (second args))
                  (offset (third args))
                  (address (+ (get-value base) offset))
                  (val (mem-read address)))
             (set-value dest val)))
      
      ;; SW: Store Word - (SW src base offset) -> MEM[base + offset] = src
      (:SW (let* ((src (first args))
                  (base (second args))
                  (offset (third args))
                  (val (get-value src))
                  (address (+ (get-value base) offset)))
             (mem-write address val)))
      
      ;; Compatibilité avec ancien format
      (:LOAD (let* ((addr (first args))
                    (dest (second args))
                    (address (get-value addr))
                    (val (mem-read address)))
               (set-value dest val)))
      
      (:STORE (let* ((src (first args))
                     (addr (second args))
                     (val (get-value src))
                     (address (get-value addr)))
                (mem-write address val)))
      
      ;; Instructions pile
      (:PUSH (let* ((src (first args))
                    (val (get-value src)))
               (push-stack val)))
      
      (:POP (let ((dest (first args)))
              (set-value dest (pop-stack))))
      
      ;; Instructions de saut style MIPS
      ;; J: Jump (saut inconditionnel)
      (:J (let* ((label (first args))
                 (code-start (calculate-code-start))
                 (pc-reg (get-reg :pc))
                 (target-addr (if (>= label code-start)
                                  label
                                  (+ code-start label))))
            (set-register pc-reg target-addr)
            (return-from execute-instruction)))
      
      ;; BEQ: Branch if Equal
      (:BEQ (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value src1))
                   (val2 (get-value src2)))
              (when (= val1 val2)
                (let* ((code-start (calculate-code-start))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BNE: Branch if Not Equal
      (:BNE (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value src1))
                   (val2 (get-value src2)))
              (when (/= val1 val2)
                (let* ((code-start (calculate-code-start))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BLT: Branch if Less Than
      (:BLT (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value src1))
                   (val2 (get-value src2)))
              (when (< val1 val2)
                (let* ((code-start (calculate-code-start))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BGT: Branch if Greater Than
      (:BGT (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value src1))
                   (val2 (get-value src2)))
              (when (> val1 val2)
                (let* ((code-start (calculate-code-start))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; JAL: Jump And Link (appel de fonction MIPS)
      ;; Format: (JAL label) où label peut être une adresse absolue ou relative
      ;; Effet: $ra = $pc + 1; $pc = label (si absolu) ou code-start + label (si relatif)
      (:JAL (let* ((label (first args))
                   (code-start (calculate-code-start))
                   (pc-reg (get-reg :pc))
                   (ra-reg (get-reg :ra))
                   (return-addr (1+ (get-register pc-reg)))
                   ;; Si label >= code-start, c'est déjà une adresse absolue
                   (target-addr (if (>= label code-start) label (+ code-start label))))
              ;; Sauvegarder l'adresse de retour dans $ra
              (set-register ra-reg return-addr)
              ;; Sauter au label
              (set-register pc-reg target-addr)
              (return-from execute-instruction)))
      
      ;; JR: Jump Register (retour de fonction MIPS)
      ;; Format: (JR $rs)
      ;; Effet: $pc = $rs
      (:JR (let* ((reg (first args))
                  (target-addr (get-value reg))
                  (pc-reg (get-reg :pc))
                  (current-pc (get-register pc-reg)))
              ;; Sauter à l'adresse contenue dans le registre
              (set-register pc-reg target-addr)
              (return-from execute-instruction)))
      
      ;; JALR: Jump And Link Register (PHASE 9 - appel de closure)
      ;; Format: (JALR $rs)
      ;; Effet: $ra = $pc + 1, $pc = $rs
      (:JALR (let* ((reg (first args))
                    (target-addr (get-value reg))
                    (pc-reg (get-reg :pc))
                    (ra-reg (get-reg :ra))
                    (current-pc (get-register pc-reg))
                    (return-addr (1+ current-pc)))
               ;; Sauvegarder l'adresse de retour dans $ra
               (set-register ra-reg return-addr)
               ;; Sauter à l'adresse dans le registre
               (set-register pc-reg target-addr)
               (return-from execute-instruction)))
      
      ;; Compatibilité avec ancien format
      (:JMP (let* ((label (first args))
                   (code-start (calculate-code-start))
                   (pc-reg (get-reg :pc))
                   (target-addr (if (>= label code-start) label (+ code-start label))))
              (set-register pc-reg target-addr)
              (return-from execute-instruction)))
      
      (:JZ (when (= 1 (get-register (get-reg :eq)))
             (let* ((label (first args))
                    (code-start (calculate-code-start))
                    (pc-reg (get-reg :pc)))
               (set-register pc-reg (+ code-start label))
               (return-from execute-instruction))))
      
      (:JNZ (when (= 0 (get-register (get-reg :eq)))
              (let* ((label (first args))
                     (code-start (calculate-code-start))
                     (pc-reg (get-reg :pc)))
                (set-register pc-reg (+ code-start label))
                (return-from execute-instruction))))
      
      (:JGT (when (= 1 (get-register (get-reg :gt)))
              (let* ((label (first args))
                     (code-start (calculate-code-start))
                     (pc-reg (get-reg :pc)))
                (set-register pc-reg (+ code-start label))
                (return-from execute-instruction))))
      
      (:JLT (when (= 1 (get-register (get-reg :lt)))
              (let* ((label (first args))
                     (code-start (calculate-code-start))
                     (pc-reg (get-reg :pc)))
                (set-register pc-reg (+ code-start label))
                (return-from execute-instruction))))
      
      ;; Comparaison (pour compatibilité)
      (:CMP (let* ((src1 (first args))
                   (src2 (second args))
                   (val1 (get-value src1))
                   (val2 (get-value src2))
                   (eq-reg (get-reg :eq))
                   (lt-reg (get-reg :lt))
                   (gt-reg (get-reg :gt)))
              (set-register eq-reg (if (= val1 val2) 1 0))
              (set-register lt-reg (if (< val1 val2) 1 0))
              (set-register gt-reg (if (> val1 val2) 1 0))))
      
      ;; SLT: Set on Less Than (style MIPS)
      (:SLT (let* ((src1 (first args))
                   (src2 (second args))
                   (dest (third args))
                   (val1 (get-value src1))
                   (val2 (get-value src2)))
              (set-value dest (if (< val1 val2) 1 0))))
      
      ;; ======================================================================
      ;; INSTRUCTIONS TAS DYNAMIQUE (PHASE 9 - CLOSURES)
      ;; ======================================================================
      
      ;; MALLOC: Alloue de la mémoire sur le tas
      ;; Format: (MALLOC size result-reg)
      ;; Effet: result-reg = adresse allouée sur le tas
      (:MALLOC (let* ((size (first args))
                      (result-reg (second args))
                      (size-val (get-value size))
                      (addr (vm-malloc size-val)))
                 (set-value result-reg addr)))
      
      ;; LOAD-HEAP: Charge une valeur depuis le tas
      ;; Format: (LOAD-HEAP addr-reg offset result-reg)
      ;; Effet: result-reg = MEM[addr-reg + offset]
      (:LOAD-HEAP (let* ((addr-reg (first args))
                         (offset (second args))
                         (result-reg (third args))
                         (base-addr (get-value addr-reg))
                         (offset-val (get-value offset))
                         (address (+ base-addr offset-val))
                         (val (mem-read address)))
                    (set-value result-reg val)))
      
      ;; STORE-HEAP: Stocke une valeur dans le tas
      ;; Format: (STORE-HEAP value-reg addr-reg offset)
      ;; Effet: MEM[addr-reg + offset] = value-reg
      (:STORE-HEAP (let* ((value-reg (first args))
                          (addr-reg (second args))
                          (offset (third args))
                          (val (get-value value-reg))
                          (base-addr (get-value addr-reg))
                          (offset-val (get-value offset))
                          (address (+ base-addr offset-val)))
                     (mem-write address val)))
      
      ;; Contrôle
      (:HALT (progn
               (setq *vm-state* +state-halted+)
               (return-from execute-instruction)))
      
      (:NOP ) ; Ne rien faire
      
      (:PRINT (let* ((src (first args))
                     (val (get-value src)))
                ;; FORMAT supprimé dans version compilable
                val))
      
      (t (error "Opcode non implémenté: ~A" opcode))))
  
  ;; Incrémenter le pointeur d'instruction ($pc)
  (let ((pc-reg (get-reg :pc)))
    (set-register pc-reg (1+ (get-register pc-reg)))))

;;; ============================================================================
;;; BOUCLE PRINCIPALE
;;; ============================================================================

(defun run-vm (max-instructions)
  "Exécute la VM jusqu'à HALT ou erreur"
  (setq *vm-state* +state-running+)
  (setq *vm-instruction-count* 0)
  
  ;; Boucle principale remplacée par WHILE (pas de LOOP ni HANDLER-CASE)
  (while (= *vm-state* +state-running+)
    (when (>= *vm-instruction-count* max-instructions)
      (progn
        (setq *vm-state* +state-error+)
        (return-from run-vm nil)))
    (let ((instr (fetch-instruction)))
      (when (or (not instr) (and (numberp instr) (= instr 0)))
        (progn
          (setq *vm-state* +state-error+)
          (return-from run-vm nil)))
      (execute-instruction instr)
      (setq *vm-instruction-count* (+ *vm-instruction-count* 1))))
  
  t)

;;; ============================================================================
;;; EXPORT (Version compilable avec variables globales)
;;; ============================================================================

(export '(;; Initialisation
          make-new-vm reset-vm run-vm
          ;; Registres
          get-register set-register dump-registers reg-index
          ;; Mémoire
          mem-read mem-write dump-memory
          ;; Pile
          push-stack pop-stack peek-stack dump-stack
          ;; Variables globales d'état
          *vm-state* *vm-instruction-count* *vm-verbose*
          *vm-memory* *vm-registers*
          ;; Constantes d'état
          +state-ready+ +state-running+ +state-halted+ +state-error+
          ;; Autres fonctions
          get-value set-value calculate-code-start
          fetch-instruction execute-instruction
          ;; Heap management (Phase 9)
          reset-heap vm-malloc *heap-pointer* +heap-limit+))
