;;;; vm.lisp
;;;; Machine virtuelle pour exécuter du code assembleur MIPS

(load "src/asm-ops.lisp")  ; Charger les opérations depuis src/

;;; ============================================================================
;;; GESTION DES HASH-TABLES VM (PHASE LOADER)
;;; ============================================================================

(defparameter *vm-hash-tables* (make-hash-table)
  "Mapping: handle (adresse heap fictive) → hash-table Lisp native.
   Permet au code compilé d'utiliser des hash-tables sans les implémenter en MIPS.")

(defparameter *vm-hash-handle-counter* 1000
  "Compteur pour générer des handles uniques pour les hash-tables")

(defparameter *vm-lisp-objects* (make-hash-table)
  "Mapping: handle → objet Lisp (listes, symboles, etc.)
   Permet au code compilé de manipuler des structures Lisp")

(defparameter *vm-lisp-handle-counter* 5000
  "Compteur pour générer des handles uniques pour les objets Lisp")

(defparameter *vm-arrays* (make-hash-table)
  "Mapping: handle → array Lisp natif
   Permet au code compilé d'utiliser des tableaux sans gestion manuelle de la mémoire")

(defparameter *vm-array-handle-counter* 10000
  "Compteur pour générer des handles uniques pour les tableaux")

;;; ============================================================================
;;; SYSTÈME DE DÉLÉGATION FFI (Foreign Function Interface)
;;; ============================================================================

(defparameter *vm-delegate-to-lisp* t
  "Si T, la VM délègue à CLISP les fonctions inconnues")

(defparameter *vm-delegated-functions* (make-hash-table)
  "Cache des fonctions déléguées: symbole → fonction CLISP")

(defparameter *vm-delegation-stats* (make-hash-table :test 'equal)
  "Statistiques des délégations: nom-fonction → nombre d'appels")

(defun vm-register-delegate (symbol-name function)
  "Enregistre une fonction CLISP pour délégation"
  (setf (gethash (intern (string-upcase symbol-name)) *vm-delegated-functions*)
        function))

(defun vm-can-delegate (function-name)
  "Vérifie si une fonction peut être déléguée à CLISP"
  (and *vm-delegate-to-lisp*
       (or (gethash function-name *vm-delegated-functions*)
           (fboundp function-name))))

(defun vm-delegate-call (vm function-name args)
  "Délègue un appel de fonction à CLISP et retourne le résultat"
  (when (vm-verbose vm)
    (format t "  ⚡ DÉLÉGATION à CLISP: ~A(~{~A~^, ~})~%" 
            function-name args))
  
  ;; Statistiques
  (let ((count (gethash function-name *vm-delegation-stats* 0)))
    (setf (gethash function-name *vm-delegation-stats*) (1+ count)))
  
  ;; Appel CLISP
  (let* ((func (or (gethash function-name *vm-delegated-functions*)
                   (symbol-function function-name)))
         (result (apply func args)))
    
    (when (vm-verbose vm)
      (format t "  ⚡ RÉSULTAT CLISP: ~A~%" result))
    
    result))

(defun vm-reset-delegation-stats ()
  "Réinitialise les statistiques de délégation"
  (clrhash *vm-delegation-stats*))

(defun vm-show-delegation-stats ()
  "Affiche les statistiques de délégation"
  (format t "~%Statistiques de délégation CLISP:~%")
  (format t "──────────────────────────────────~%")
  (let ((total 0))
    (maphash (lambda (func count)
               (format t "  ~A: ~A appels~%" func count)
               (incf total count))
             *vm-delegation-stats*)
    (format t "──────────────────────────────────~%")
    (format t "Total: ~A appels délégués~%" total)))

(defun reset-vm-hash-tables ()
  "Réinitialise les tables de hash-tables, objets Lisp et tableaux"
  (clrhash *vm-hash-tables*)
  (clrhash *vm-lisp-objects*)
  (clrhash *vm-arrays*)
  (setf *vm-hash-handle-counter* 1000)
  (setf *vm-lisp-handle-counter* 5000)
  (setf *vm-array-handle-counter* 10000)
  (setf *heap-pointer* +heap-start+)
  (vm-reset-delegation-stats))

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
;;; GESTION DES TABLEAUX (ARRAYS)
;;; ============================================================================

(defun vm-store-array (vm array)
  "Stocke un tableau Lisp natif et retourne son handle unique"
  (let ((handle (incf *vm-array-handle-counter*)))
    (setf (gethash handle *vm-arrays*) array)
    (when (vm-verbose vm)
      (format t "  STORE-ARRAY: Tableau de ~A éléments, handle ~A~%" 
              (length array) handle))
    handle))

(defun vm-get-array (vm handle)
  "Récupère un tableau Lisp natif depuis son handle"
  (let ((array (gethash handle *vm-arrays*)))
    (unless array
      (error "ARRAY: Handle invalide ~A" handle))
    array))

(defun vm-get-lisp-object (handle)
  "Récupère un objet Lisp depuis son handle
   Utilisé pour récupérer des résultats complexes (listes) depuis la VM"
  (gethash handle *vm-lisp-objects*))

(defun vm-store-lisp-object (object)
  "Stocke un objet Lisp et retourne son handle
   Utilisé pour passer des structures complexes à la VM"
  (let ((handle (incf *vm-lisp-handle-counter*)))
    (setf (gethash handle *vm-lisp-objects*) object)
    handle))

;;; ============================================================================
;;; SYSTÈME D'INTERNING DE SYMBOLES
;;; ============================================================================

(defvar *vm-symbol-to-id* (make-hash-table :test 'equal)
  "Table: nom de symbole (string) → ID numérique")

(defvar *vm-id-to-symbol* (make-hash-table :test 'eql)
  "Table: ID numérique → nom de symbole (string)")

(defvar *vm-next-symbol-id* 1
  "Prochain ID disponible pour un symbole")

(defvar *vm-globals* (make-hash-table :test 'eq)
  "Table des variables globales: symbole → valeur")

(defun intern-symbol (symbol-name)
  "Retourne l'ID d'un symbole, le crée si nécessaire"
  (let ((existing-id (gethash symbol-name *vm-symbol-to-id*)))
    (if existing-id
        existing-id
        ;; Créer nouveau symbole
        (let ((new-id *vm-next-symbol-id*))
          (setf (gethash symbol-name *vm-symbol-to-id*) new-id)
          (setf (gethash new-id *vm-id-to-symbol*) symbol-name)
          (incf *vm-next-symbol-id*)
          new-id))))

(defun symbol-name-from-id (symbol-id)
  "Retourne le nom d'un symbole depuis son ID"
  (gethash symbol-id *vm-id-to-symbol*))

(defun symbol-id-to-keyword (symbol-id)
  "Convertit un ID de symbole en keyword (pour les registres)"
  (let ((name (symbol-name-from-id symbol-id)))
    (when name
      (intern name :keyword))))

(defun initialize-compiler-symbols ()
  "Pré-intern les symboles utilisés par le compilateur"
  ;; Instructions MIPS
  (intern-symbol "LI")
  (intern-symbol "LW")
  (intern-symbol "SW")
  (intern-symbol "ADD")
  (intern-symbol "SUB")
  (intern-symbol "ADDI")
  (intern-symbol "LIST")
  (intern-symbol "MUL")
  (intern-symbol "DIV")
  (intern-symbol "BEQ")
  (intern-symbol "BNE")
  (intern-symbol "BLT")
  (intern-symbol "BGT")
  (intern-symbol "J")
  (intern-symbol "JAL")
  (intern-symbol "JR")
  (intern-symbol "JALR")
  (intern-symbol "MOVE")
  (intern-symbol "PUSH")
  (intern-symbol "POP")
  (intern-symbol "HALT")
  (intern-symbol "LABEL")
  (intern-symbol "MFLO")
  (intern-symbol "MFHI")
  (intern-symbol "GLOBAL-GET")
  (intern-symbol "GLOBAL-SET")
  ;; Registres
  (intern-symbol "$V0")
  (intern-symbol "$V1")
  (intern-symbol "$A0")
  (intern-symbol "$A1")
  (intern-symbol "$A2")
  (intern-symbol "$A3")
  (intern-symbol "$T0")
  (intern-symbol "$T1")
  (intern-symbol "$T2")
  (intern-symbol "$T3")
  (intern-symbol "$S0")
  (intern-symbol "$S1")
  (intern-symbol "$SP")
  (intern-symbol "$FP")
  (intern-symbol "$RA")
  (intern-symbol "$ZERO")
  ;; Keywords Lisp
  (intern-symbol "DEFUN")
  (intern-symbol "IF")
  (intern-symbol "LET")
  (intern-symbol "QUOTE")
  (intern-symbol "LAMBDA")
  (intern-symbol "NIL")
  (intern-symbol "T")
  (intern-symbol "COND")
  (intern-symbol "AND")
  (intern-symbol "OR")
  (intern-symbol "NOT"))

(defun initialize-global-variables ()
  "Initialise les variables globales utilisées par le compilateur"
  ;; Registres
  (setf (gethash '*reg-v0* *vm-globals*) (intern-symbol "$V0"))
  (setf (gethash '*reg-v1* *vm-globals*) (intern-symbol "$V1"))
  (setf (gethash '*reg-a0* *vm-globals*) (intern-symbol "$A0"))
  (setf (gethash '*reg-a1* *vm-globals*) (intern-symbol "$A1"))
  (setf (gethash '*reg-a2* *vm-globals*) (intern-symbol "$A2"))
  (setf (gethash '*reg-a3* *vm-globals*) (intern-symbol "$A3"))
  (setf (gethash '*reg-t0* *vm-globals*) (intern-symbol "$T0"))
  (setf (gethash '*reg-t1* *vm-globals*) (intern-symbol "$T1"))
  (setf (gethash '*reg-t2* *vm-globals*) (intern-symbol "$T2"))
  (setf (gethash '*reg-t3* *vm-globals*) (intern-symbol "$T3"))
  (setf (gethash '*reg-sp* *vm-globals*) (intern-symbol "$SP"))
  (setf (gethash '*reg-fp* *vm-globals*) (intern-symbol "$FP"))
  (setf (gethash '*reg-ra* *vm-globals*) (intern-symbol "$RA"))
  (setf (gethash '*reg-s0* *vm-globals*) (intern-symbol "$S0"))
  (setf (gethash '*reg-s1* *vm-globals*) (intern-symbol "$S1"))
  ;; Instructions
  (setf (gethash '*instr-li* *vm-globals*) (intern-symbol "LI"))
  (setf (gethash '*instr-lw* *vm-globals*) (intern-symbol "LW"))
  (setf (gethash '*instr-sw* *vm-globals*) (intern-symbol "SW"))
  (setf (gethash '*instr-add* *vm-globals*) (intern-symbol "ADD"))
  (setf (gethash '*instr-sub* *vm-globals*) (intern-symbol "SUB"))
  (setf (gethash '*instr-addi* *vm-globals*) (intern-symbol "ADDI"))
  (setf (gethash '*instr-list* *vm-globals*) (intern-symbol "LIST")))

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
  (reset-vm-hash-tables)  ; Réinitialiser les tables globales
  (initialize-compiler-symbols)  ; Pré-intern les symboles du compilateur
  (initialize-global-variables)  ; Initialiser les variables globales
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
  (set-register vm (get-reg :gp) +heap-start+)                ; Global pointer (pour le tas)
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
  (reset-vm-hash-tables)
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
    ;; Si c'est un ID de symbole de registre, le convertir
    ((and (numberp operand) (fboundp 'symbol-id-to-keyword))
     (let ((keyword-reg (symbol-id-to-keyword operand)))
       (if (and keyword-reg (register-p keyword-reg))
           (get-value vm keyword-reg)
           operand)))  ; Sinon c'est un nombre normal
    ;; Si c'est un registre keyword, récupérer sa valeur
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
  ;; Convertir ID en keyword si nécessaire
  (let ((reg (if (and (numberp operand) (fboundp 'symbol-id-to-keyword))
                 (or (symbol-id-to-keyword operand) operand)
                 operand)))
    (unless (register-p reg)
      (error "La destination doit être un registre: ~A (ID: ~A)" reg operand))
    ;; $zero ne peut pas être modifié (convention MIPS)
    (let ((zero-reg (get-reg :zero)))
      (unless (eq reg zero-reg)
        (set-register vm reg value)))))

(defun execute-instruction (vm instr)
  "Exécute une instruction"
  (when (vm-verbose vm)
    (format t "~%[~A] Exécution: ~A~%" 
            (vm-instruction-count vm) 
            (format-instruction instr)))
  
  (let ((opcode (first instr))
        (args (rest instr)))
    ;; Convertir ID numérique en keyword si nécessaire
    (when (numberp opcode)
      (setf opcode (symbol-id-to-keyword opcode)))
    
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
      
      ;; BLE: Branch if Less than or Equal
      (:BLE (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (<= val1 val2)
                (let* ((code-start (calculate-code-start vm))
                       (pc-reg (get-reg :pc))
                       (target-addr (if (>= label code-start)
                                        label
                                        (+ code-start label))))
                  (set-register vm pc-reg target-addr)
                  (return-from execute-instruction)))))
      
      ;; BGE: Branch if Greater than or Equal
      (:BGE (let* ((src1 (first args))
                   (src2 (second args))
                   (label (third args))
                   (val1 (get-value vm src1))
                   (val2 (get-value vm src2)))
              (when (>= val1 val2)
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
      ;; NOUVEAU: Si le label est un symbole et pas une adresse, tenter délégation CLISP
      (:JAL (let* ((label (first args))
                   (code-start (calculate-code-start vm))
                   (pc-reg (get-reg :pc))
                   (ra-reg (get-reg :ra))
                   (return-addr (1+ (get-register vm pc-reg))))
              
              ;; Cas 1: Label est un symbole → peut être une fonction CLISP
              (when (and (symbolp label) (vm-can-delegate label))
                (when (vm-verbose vm)
                  (format t "  JAL: Détection fonction déléguée ~A~%" label))
                
                ;; Extraire les arguments depuis les registres $A0-$A3
                (let* ((a0 (get-register vm (get-reg :a0)))
                       (a1 (get-register vm (get-reg :a1)))
                       (a2 (get-register vm (get-reg :a2)))
                       (a3 (get-register vm (get-reg :a3)))
                       ;; Compter les arguments valides (non-nuls ou présents)
                       ;; TODO: améliorer la détection du nombre d'arguments
                       (args-list (list a0 a1 a2 a3))
                       ;; Pour l'instant, on passe tous les registres
                       (result (vm-delegate-call vm label args-list)))
                  
                  ;; Placer le résultat dans $V0
                  (set-register vm (get-reg :v0) result)
                  
                  ;; Simuler le retour de fonction (pas de vrai saut)
                  ;; On continue à l'instruction suivante
                  (set-register vm pc-reg return-addr)
                  (return-from execute-instruction)))
              
              ;; Cas 2: Adresse numérique → saut normal
              (let ((target-addr (if (>= label code-start) label (+ code-start label))))
                (when (vm-verbose vm)
                  (format t "  JAL: Sauvegarde $ra=~A, saut vers ~A~%" 
                          return-addr target-addr))
                ;; Sauvegarder l'adresse de retour dans $ra
                (set-register vm ra-reg return-addr)
                ;; Sauter au label
                (set-register vm pc-reg target-addr)
                (return-from execute-instruction))))
      
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
      
      ;; ======================================================================
      ;; INSTRUCTIONS HASH-TABLE (PHASE LOADER)
      ;; ======================================================================
      
      ;; HASH-MAKE: Crée une hash-table
      ;; Format: (HASH-MAKE test-fn-reg)
      ;; Effet: $V0 = handle de la hash-table créée
      (:HASH-MAKE 
       (let* ((test-fn-reg (first args))
              (test-fn-val (get-value vm test-fn-reg))
              ;; test-fn-val: 0='eq, 1='equal
              (test-fn (if (= test-fn-val 1) 'equal 'eq))
              (ht (make-hash-table :test test-fn))
              (handle (incf *vm-hash-handle-counter*)))
         (setf (gethash handle *vm-hash-tables*) ht)
         (set-value vm :$v0 handle)
         (when (vm-verbose vm)
           (format t "  HASH-MAKE: Créé hash-table avec handle ~A (test=~A)~%" handle test-fn))))
      
      ;; HASH-GET: Récupère une valeur dans une hash-table
      ;; Format: (HASH-GET table-reg key-reg)
      ;; Effet: $V0 = valeur ou 0 si absent
      (:HASH-GET
       (let* ((table-reg (first args))
              (key-reg (second args))
              (handle (get-value vm table-reg))
              (key (get-value vm key-reg))
              (ht (gethash handle *vm-hash-tables*)))
         (unless ht
           (error "HASH-GET: Handle invalide ~A" handle))
         (let ((value (gethash key ht 0)))  ; 0 par défaut si absent
           (set-value vm :$v0 value)
           (when (vm-verbose vm)
             (format t "  HASH-GET: table[~A] -> ~A~%" key value)))))
      
      ;; HASH-SET: Stocke une valeur dans une hash-table
      ;; Format: (HASH-SET table-reg key-reg value-reg)
      (:HASH-SET
       (let* ((table-reg (first args))
              (key-reg (second args))
              (value-reg (third args))
              (handle (get-value vm table-reg))
              (key (get-value vm key-reg))
              (value (get-value vm value-reg))
              (ht (gethash handle *vm-hash-tables*)))
         (unless ht
           (error "HASH-SET: Handle invalide ~A" handle))
         (setf (gethash key ht) value)
         (when (vm-verbose vm)
           (format t "  HASH-SET: table[~A] = ~A~%" key value))))
      
      ;; HASH-COUNT: Nombre d'entrées dans une hash-table
      ;; Format: (HASH-COUNT table-reg)
      ;; Effet: $V0 = nombre d'entrées
      (:HASH-COUNT
       (let* ((table-reg (first args))
              (handle (get-value vm table-reg))
              (ht (gethash handle *vm-hash-tables*)))
         (unless ht
           (error "HASH-COUNT: Handle invalide ~A" handle))
         (let ((count (hash-table-count ht)))
           (set-value vm :$v0 count)
           (when (vm-verbose vm)
             (format t "  HASH-COUNT: ~A entrées~%" count)))))
      
      ;; HASH-HAS-KEY: Vérifie si une clé existe
      ;; Format: (HASH-HAS-KEY table-reg key-reg)
      ;; Effet: $V0 = 1 si existe, 0 sinon
      (:HASH-HAS-KEY
       (let* ((table-reg (first args))
              (key-reg (second args))
              (handle (get-value vm table-reg))
              (key (get-value vm key-reg))
              (ht (gethash handle *vm-hash-tables*)))
         (unless ht
           (error "HASH-HAS-KEY: Handle invalide ~A" handle))
         (let ((exists (if (nth-value 1 (gethash key ht)) 1 0)))
           (set-value vm :$v0 exists)
           (when (vm-verbose vm)
             (format t "  HASH-HAS-KEY: clé ~A existe? ~A~%" key exists)))))
      
      ;; ======================================================================
      ;; INSTRUCTIONS PRÉDICATS DE TYPE (PHASE LOADER)
      ;; ======================================================================
      
      ;; TYPE-CHECK: Vérifie le type d'une valeur
      ;; Format: (TYPE-CHECK predicate value-reg)
      ;; Effet: $V0 = 1 si vrai, 0 sinon
      (:TYPE-CHECK
       (let* ((predicate (first args))
              (value-reg (second args))
              (value-raw (get-value vm value-reg))
              ;; Déréférencer les handles si nécessaire
              (value (gethash value-raw *vm-lisp-objects* value-raw))
              (result (case predicate
                        (listp (if (listp value) 1 0))
                        (symbolp (if (symbolp value) 1 0))
                        (keywordp (if (keywordp value) 1 0))
                        (consp (if (consp value) 1 0))
                        (atom (if (atom value) 1 0))
                        (t (error "TYPE-CHECK: Prédicat inconnu ~A" predicate)))))
         (set-value vm :$v0 result)
         (when (vm-verbose vm)
           (format t "  TYPE-CHECK: (~A ~A) -> ~A~%" predicate value result))))
      
      ;; ======================================================================
      ;; INSTRUCTIONS LISTES (PHASE LOADER)
      ;; ======================================================================
      
      ;; LIST-CAR: Récupère le car d'une liste
      ;; Format: (LIST-CAR list-reg)
      ;; Effet: $V0 = car de la liste
      (:LIST-CAR
       (let* ((list-reg (first args))
              (list-handle (get-value vm list-reg))
              (lst (gethash list-handle *vm-lisp-objects* list-handle)))
         (unless (listp lst)
           (error "LIST-CAR: ~A n'est pas une liste" lst))
         (let* ((result (car lst))
                ;; Si le résultat est une liste/symbole, créer un handle
                (result-value (if (or (listp result) (symbolp result))
                                  (let ((handle (incf *vm-lisp-handle-counter*)))
                                    (setf (gethash handle *vm-lisp-objects*) result)
                                    handle)
                                  result)))
           (set-value vm :$v0 result-value)
           (when (vm-verbose vm)
             (format t "  LIST-CAR: ~A -> ~A~%" lst result)))))
      
      ;; LIST-CDR: Récupère le cdr d'une liste
      ;; Format: (LIST-CDR list-reg)
      ;; Effet: $V0 = cdr de la liste
      (:LIST-CDR
       (let* ((list-reg (first args))
              (list-handle (get-value vm list-reg))
              (lst (gethash list-handle *vm-lisp-objects* list-handle)))
         (unless (listp lst)
           (error "LIST-CDR: ~A n'est pas une liste" lst))
         (let* ((result (cdr lst))
                ;; Créer un handle pour le cdr
                (result-value (if (or (listp result) (symbolp result))
                                  (let ((handle (incf *vm-lisp-handle-counter*)))
                                    (setf (gethash handle *vm-lisp-objects*) result)
                                    handle)
                                  result)))
           (set-value vm :$v0 result-value)
           (when (vm-verbose vm)
             (format t "  LIST-CDR: ~A -> ~A~%" lst result)))))
      
      ;; LIST: Crée une liste à partir d'éléments sur la pile
      ;; Format: (LIST count) ou (:LIST count)
      ;; Lit 'count' valeurs depuis la pile et crée une liste
      ;; Effet: $V0 = handle de la liste créée
      ((LIST :LIST)
       (let* ((count (first args))
              (elements nil))
         ;; Récupérer les éléments de la pile
         ;; Les éléments sont empilés avec SW puis ADDI $SP, -4, donc ils sont à:
         ;; 1er élément: $SP + (count * 4)
         ;; 2ème élément: $SP + ((count-1) * 4)
         ;; etc.
         (dotimes (i count)
           (let* ((offset (* (- count i) 4))  ; Offset depuis $SP: count*4, (count-1)*4, ..., 4
                  (addr (+ (get-value vm :$sp) offset))
                  (val (mem-read vm addr)))
             ;; Résoudre le handle si c'est un objet Lisp
             (push (gethash val *vm-lisp-objects* val) elements)))
         ;; Restaurer la pile (dépiler count éléments * 4 octets)
         (set-value vm :$sp (+ (get-value vm :$sp) (* count 4)))
         ;; Créer la liste et son handle
         (let ((result-list (nreverse elements))  ; Inverser car on a pushé en LIFO
               (handle (incf *vm-lisp-handle-counter*)))
           (setf (gethash handle *vm-lisp-objects*) result-list)
           (set-value vm :$v0 handle)
           (when (vm-verbose vm)
             (format t "  LIST(~A): ~A -> handle ~A~%" count result-list handle)))))
      
      ;; LIST-CONS: Crée une paire cons
      ;; Format: (LIST-CONS car-reg cdr-reg)
      ;; Effet: $V0 = handle de (cons car cdr)
      (:LIST-CONS
       (let* ((car-reg (first args))
              (cdr-reg (second args))
              (car-val (get-value vm car-reg))
              (cdr-val (get-value vm cdr-reg))
              ;; Résoudre les handles si nécessaire
              (car-obj (gethash car-val *vm-lisp-objects* car-val))
              (cdr-obj (gethash cdr-val *vm-lisp-objects* cdr-val))
              (result (cons car-obj cdr-obj))
              (handle (incf *vm-lisp-handle-counter*)))
         (setf (gethash handle *vm-lisp-objects*) result)
         (set-value vm :$v0 handle)
         (when (vm-verbose vm)
           (format t "  LIST-CONS: (~A . ~A) -> handle ~A~%" car-obj cdr-obj handle))))
      
      ;; LIST-CADR: Récupère le second élément d'une liste (car (cdr ...))
      ;; Format: (LIST-CADR list-reg)
      ;; Effet: $V0 = cadr de la liste
      (:LIST-CADR
       (let* ((list-reg (first args))
              (list-handle (get-value vm list-reg))
              (lst (gethash list-handle *vm-lisp-objects* list-handle)))
         (unless (listp lst)
           (error "LIST-CADR: ~A n'est pas une liste" lst))
         (let* ((result (cadr lst))
                ;; Si le résultat est une liste/symbole, créer un handle
                (result-value (if (or (listp result) (symbolp result))
                                  (let ((handle (incf *vm-lisp-handle-counter*)))
                                    (setf (gethash handle *vm-lisp-objects*) result)
                                    handle)
                                  result)))
           (set-value vm :$v0 result-value)
           (when (vm-verbose vm)
             (format t "  LIST-CADR: ~A -> ~A~%" lst result)))))
      
      ;; INTERN: Convertit un string en symbole (retourne son ID)
      ;; Format: (INTERN string-handle-reg)
      ;; string-handle-reg: registre contenant le handle d'un string
      ;; Effet: $V0 = ID du symbole
      (:INTERN
       (let* ((string-handle-reg (first args))
              (string-handle (get-value vm string-handle-reg))
              (string-obj (gethash string-handle *vm-lisp-objects*)))
         (unless (stringp string-obj)
           (error "INTERN attend un handle de string, reçu handle ~A -> ~A" 
                  string-handle string-obj))
         (let ((symbol-id (intern-symbol string-obj)))
           (set-value vm :$v0 symbol-id)
           (when (vm-verbose vm)
             (format t "  INTERN: ~S -> ID ~A~%" string-obj symbol-id)))))
      
      ;; SYMBOL-NAME: Convertit un ID de symbole en string (retourne handle)
      ;; Format: (SYMBOL-NAME symbol-id-reg)
      ;; symbol-id-reg: registre contenant l'ID d'un symbole
      ;; Effet: $V0 = handle du nom du symbole (string)
      (:SYMBOL-NAME
       (let* ((symbol-id-reg (first args))
              (symbol-id (get-value vm symbol-id-reg))
              (symbol-name (symbol-name-from-id symbol-id)))
         (if symbol-name
             (let ((handle (incf *vm-lisp-handle-counter*)))
               (setf (gethash handle *vm-lisp-objects*) symbol-name)
               (set-value vm :$v0 handle)
               (when (vm-verbose vm)
                 (format t "  SYMBOL-NAME: ID ~A -> ~S (handle ~A)~%" 
                         symbol-id symbol-name handle)))
             (progn
               (set-value vm :$v0 0)
               (when (vm-verbose vm)
                 (format t "  SYMBOL-NAME: ID ~A inconnu~%" symbol-id))))))
      
      ;; GLOBAL-GET: Lit une variable globale
      ;; Format: (GLOBAL-GET symbol)
      ;; symbol: symbole de la variable (ex: *reg-v0*)
      ;; Effet: $V0 = valeur de la variable globale
      (:GLOBAL-GET
       (let* ((symbol (first args))
              (value (gethash symbol *vm-globals*)))
         (if value
             (progn
               (set-value vm :$v0 value)
               (when (vm-verbose vm)
                 (format t "  GLOBAL-GET: ~A -> ~A~%" symbol value)))
             (progn
               (set-value vm :$v0 0)
               (when (vm-verbose vm)
                 (format t "  GLOBAL-GET: ~A non trouvé~%" symbol))))))
      
      ;; GLOBAL-SET: Définit une variable globale
      ;; Format: (GLOBAL-SET symbol value-reg)
      ;; symbol: symbole de la variable
      ;; value-reg: registre contenant la valeur
      ;; Effet: définit la variable globale
      (:GLOBAL-SET
       (let* ((symbol (first args))
              (value-reg (second args))
              (value (get-value vm value-reg)))
         (setf (gethash symbol *vm-globals*) value)
         (when (vm-verbose vm)
           (format t "  GLOBAL-SET: ~A = ~A~%" symbol value))))
      
      ;; ======================================================================
      ;; INSTRUCTIONS DE COMPARAISON (PHASE LOADER)
      ;; ======================================================================
      
      ;; EQUAL: Compare deux valeurs (avec déréférencement de handles)
      ;; Format: (EQUAL val1-reg val2-reg)
      ;; Effet: $V0 = 1 si égaux, 0 sinon
      (:EQUAL
       (let* ((val1-reg (first args))
              (val2-reg (second args))
              (val1-raw (get-value vm val1-reg))
              (val2-raw (get-value vm val2-reg))
              ;; Déréférencer les handles si nécessaire
              (val1 (gethash val1-raw *vm-lisp-objects* val1-raw))
              (val2 (gethash val2-raw *vm-lisp-objects* val2-raw))
              ;; Comparer avec equal (fonctionne pour nombres, symboles, keywords, etc.)
              (result (if (equal val1 val2) 1 0)))
         (set-value vm :$v0 result)
         (when (vm-verbose vm)
           (format t "  EQUAL: ~A = ~A -> ~A~%" val1 val2 result))))
      
      ;; ======================================================================
      ;; INSTRUCTIONS POUR TABLEAUX (ARRAYS) - Délégué à Lisp
      ;; ======================================================================
      ;; Les tableaux sont des objets Lisp natifs stockés dans une table
      ;; La VM manipule des IDs/références au lieu de gérer la mémoire
      
      ;; MAKE-ARRAY: Crée un tableau Lisp natif
      ;; Format: (MAKE-ARRAY size-reg)
      ;; size-reg: nombre d'éléments
      ;; Effet: Crée un vrai tableau Lisp, retourne son ID dans $V0
      (:MAKE-ARRAY
       (let* ((size-reg (first args))
              (size (get-value vm size-reg))
              ;; Créer un vrai tableau Lisp
              (array (make-array size :initial-element 0))
              ;; Générer un ID unique et stocker le tableau
              (array-id (vm-store-array vm array)))
         ;; Retourner l'ID dans $V0
         (set-value vm :$v0 array-id)
         (when (vm-verbose vm)
           (format t "  MAKE-ARRAY: Création tableau Lisp de ~A éléments, ID=~A~%" 
                   size array-id))))
      
      ;; AREF: Accède à un élément du tableau
      ;; Format: (AREF array-id-reg index-reg dest-reg)
      ;; array-id-reg: ID du tableau
      ;; index-reg: index de l'élément (0-based)
      ;; dest-reg: registre destination (typiquement $V0)
      ;; Effet: dest-reg = array[index]
      (:AREF
       (let* ((array-id-reg (first args))
              (index-reg (second args))
              (dest-reg (third args))
              (array-id (get-value vm array-id-reg))
              (index (get-value vm index-reg))
              ;; Récupérer le tableau Lisp
              (array (vm-get-array vm array-id)))
         ;; Vérifier les bornes (Lisp le fait aussi, mais pour message clair)
         (when (or (< index 0) (>= index (length array)))
           (error "AREF: Index hors limites: ~A (taille: ~A)" index (length array)))
         ;; Lire et retourner l'élément
         (let ((value (aref array index)))
           (set-value vm dest-reg value)
           (when (vm-verbose vm)
             (format t "  AREF: array[~A] = ~A~%" index value)))))
      
      ;; ASET: Modifie un élément du tableau
      ;; Format: (ASET array-id-reg index-reg value-reg)
      ;; array-id-reg: ID du tableau
      ;; index-reg: index de l'élément (0-based)
      ;; value-reg: valeur à stocker
      ;; Effet: array[index] = value
      (:ASET
       (let* ((array-id-reg (first args))
              (index-reg (second args))
              (value-reg (third args))
              (array-id (get-value vm array-id-reg))
              (index (get-value vm index-reg))
              (value (get-value vm value-reg))
              ;; Récupérer le tableau Lisp
              (array (vm-get-array vm array-id)))
         ;; Vérifier les bornes
         (when (or (< index 0) (>= index (length array)))
           (error "ASET: Index hors limites: ~A (taille: ~A)" index (length array)))
         ;; Écrire la valeur
         (setf (aref array index) value)
         (when (vm-verbose vm)
           (format t "  ASET: array[~A] := ~A~%" index value))))
      
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
;;; APPEL DIRECT DE FONCTIONS
;;; ============================================================================

(defparameter *vm-loaded-code* (make-hash-table)
  "Mapping: VM → code assembleur chargé dans cette VM")

(defun find-function-address (vm function-name)
  "Trouve l'adresse d'une fonction dans le code chargé dans la VM"
  (let ((code (gethash vm *vm-loaded-code*))
        (code-start (calculate-code-start vm))
        (addr 0)
        (func-str (if (symbolp function-name)
                      (symbol-name function-name)
                      function-name)))
    (unless code
      (error "Aucun code chargé dans cette VM. Utilisez load-code d'abord."))
    
    (dolist (instr code)
      (when (and (listp instr)
                 (eq (first instr) :LABEL))
        (let ((label-str (if (symbolp (second instr))
                            (symbol-name (second instr))
                            (second instr))))
          (when (string= label-str func-str)
            (return-from find-function-address (+ code-start addr)))))
      (incf addr))
    
    (error "Fonction ~A introuvable dans le code chargé" function-name)))

(defun call-function (vm function-name &rest args)
  "Appelle une fonction chargée dans la VM avec les arguments donnés.
   Localise automatiquement la fonction, configure les registres et exécute.
   Retourne le résultat dans $V0.
   
   Exemple: (call-function vm 'FIBO 20)
            (call-function vm 'ACK 3 4)"
  
  ;; Vérifier que la VM est prête
  (unless (eq (vm-state vm) :ready)
    (error "La VM n'est pas prête. État: ~A" (vm-state vm)))
  
  ;; Vérifier le nombre d'arguments (max 4 pour MIPS: $a0-$a3)
  (when (> (length args) 4)
    (error "Trop d'arguments (~A). Maximum: 4 (registres $a0-$a3)" (length args)))
  
  ;; Localiser la fonction
  (let ((func-addr (find-function-address vm function-name)))
    
    (when (vm-verbose vm)
      (format t "~%Appel de fonction: ~A(~{~A~^, ~})~%" function-name args)
      (format t "  Adresse: ~A~%" func-addr))
    
    ;; Placer les arguments dans les registres $a0, $a1, $a2, $a3
    (let ((arg-regs '(:a0 :a1 :a2 :a3)))
      (loop for arg in args
            for reg in arg-regs
            do (progn
                 (set-register vm (get-reg reg) arg)
                 (when (vm-verbose vm)
                   (format t "  $~A = ~A~%" 
                           (string-upcase (symbol-name reg)) arg)))))
    
    ;; Configurer PC et RA
    (set-register vm (get-reg :pc) func-addr)
    (set-register vm (get-reg :ra) 0)  ; Retour = HALT
    
    (when (vm-verbose vm)
      (format t "  $PC = ~A~%" func-addr)
      (format t "  $RA = 0 (HALT)~%")
      (format t "~%Exécution...~%"))
    
    ;; Exécuter (capture l'erreur "hors limites: 0" qui est normale)
    (handler-case
        (run-vm vm)
      (error (e)
        (let ((err-msg (format nil "~A" e)))
          (unless (search "Adresse mémoire hors limites: 0" err-msg)
            (error e)))))  ; Propager autres erreurs
    
    ;; Récupérer le résultat
    (let ((result (get-register vm (get-reg :v0))))
      (when (vm-verbose vm)
        (format t "~%Résultat: ~A~%" result))
      result)))

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
          reset-heap vm-malloc *heap-pointer* +heap-limit+
          ;; Function calling (direct call)
          call-function find-function-address
          ;; FFI Delegation
          *vm-delegate-to-lisp* vm-register-delegate vm-can-delegate
          vm-delegate-call vm-reset-delegation-stats vm-show-delegation-stats
          ;; Lisp objects marshalling
          vm-get-lisp-object vm-store-lisp-object *vm-lisp-objects*))
