;;;; asm-ops.lisp
;;;; Définition des opcodes et instructions assembleur pour la VM

;;; ============================================================================
;;; CONSTANTES
;;; ============================================================================

(defparameter *maxmem* 10000
  "Taille maximale de la mémoire de la VM")

;;; CONVENTIONS MIPS
;;; $zero ($0) = toujours 0
;;; $v0-$v1 ($2-$3) = valeurs de retour de fonction
;;; $a0-$a3 ($4-$7) = arguments de fonction (4 premiers)
;;; $t0-$t9 ($8-$15, $24-$25) = temporaires (non sauvegardés)
;;; $s0-$s7 ($16-$23) = sauvegardés (doivent être restaurés)
;;; $sp ($29) = stack pointer
;;; $fp ($30) = frame pointer
;;; $ra ($31) = return address

;;; ============================================================================
;;; OPCODES - Instructions assembleur
;;; ============================================================================

(defparameter *opcodes*
  '(;; Instructions arithmétiques style MIPS
    :ADD :ADDI :SUB :MUL :DIV :MOD
    :MFLO :MFHI :SLT
    ;; Instructions logiques
    :AND :OR :NOT
    ;; Instructions de comparaison (compatibilité)
    :CMP :EQ :NE :GT :LT :GE :LE
    ;; Instructions de saut style MIPS
    :J :BEQ :BNE :BLT :BGT :BLE :BGE
    ;; Instructions de saut (compatibilité)
    :JMP :JEQ :JNE :JGT :JLT :JGE :JLE :JZ :JNZ
    ;; Instructions pile
    :PUSH :POP
    ;; Instructions mémoire et transfert style MIPS
    :LW :SW :LI :MOVE
    ;; Instructions mémoire (compatibilité)
    :LOAD :STORE :LOADI
    ;; Instructions appel de fonction
    :JAL :JR :JALR :CALL :RET
    ;; Instructions tas dynamique (PHASE 9 CLOSURES)
    :MALLOC :LOAD-HEAP :STORE-HEAP
    ;; Autres
    :NOP :HALT :LABEL :PRINT :SYSCALL)
  "Liste des opcodes supportés par la VM")

;;; ============================================================================
;;; FORMAT DES INSTRUCTIONS
;;; ============================================================================

;;; Format des instructions (style MIPS) :
;;; (ADD $t0 $t1 $t2)    - $t2 = $t0 + $t1 (format R)
;;; (ADDI $t0 42 $t1)    - $t1 = $t0 + 42 (format I - immediate)
;;; (SUB $t0 $t1 $t2)    - $t2 = $t0 - $t1
;;; (MUL $t0 $t1)        - $hi:$lo = $t0 * $t1
;;; (DIV $t0 $t1)        - $lo = $t0 / $t1, $hi = $t0 % $t1
;;; (MOVE $t0 $t1)       - $t1 = $t0
;;; (LI val $t0)         - $t0 = val (load immediate)
;;; (LW $t0 offset $t1)  - $t1 = MEM[$t0 + offset] (load word)
;;; (SW $t0 offset $t1)  - MEM[$t1 + offset] = $t0 (store word)
;;; (BEQ $t0 $t1 label)  - Branch if $t0 == $t1
;;; (BNE $t0 $t1 label)  - Branch if $t0 != $t1
;;; (BLT $t0 $t1 label)  - Branch if $t0 < $t1
;;; (BGT $t0 $t1 label)  - Branch if $t0 > $t1
;;; (J label)            - Saut inconditionnel
;;; (JAL label)          - Jump and link (appel de fonction)
;;; (JR $ra)             - Jump register (retour de fonction)
;;; (SYSCALL)            - Appel système (pour PRINT, HALT, etc.)
;;; (NOP)                - Ne fait rien
;;; (LABEL name)         - Définit un label
;;;
;;; Note: Certaines instructions gardent un format simplifié pour la compatibilité

;;; ============================================================================
;;; REGISTRES (Architecture MIPS)
;;; ============================================================================

(defparameter *register-names*
  '(;; Registres MIPS standards
    :$zero :$at           ; $0 = constante 0, $1 = assembleur temporaire
    :$v0 :$v1             ; $2-$3 = valeurs de retour
    :$a0 :$a1 :$a2 :$a3   ; $4-$7 = arguments de fonction
    :$t0 :$t1 :$t2 :$t3 :$t4 :$t5 :$t6 :$t7  ; $8-$15 = temporaires
    :$s0 :$s1 :$s2 :$s3 :$s4 :$s5 :$s6 :$s7  ; $16-$23 = sauvegardés
    :$t8 :$t9             ; $24-$25 = temporaires
    :$k0 :$k1             ; $26-$27 = réservés OS
    :$gp                  ; $28 = pointeur global
    :$sp                  ; $29 = stack pointer
    :$fp                  ; $30 = frame pointer
    :$ra                  ; $31 = adresse de retour
    ;; Registres spéciaux VM
    :$pc                  ; Program Counter (comme PL)
    :$hi :$lo             ; Pour multiplication/division
    ;; Flags de comparaison (extension)
    :$gt :$lt :$eq        ; Flags de comparaison
    ;; COMPATIBILITÉ: Anciens noms de registres
    :R0 :R1 :R2 :MEM :GT :LT :EQ :PL :FP :SP :HP)
  "Liste des noms de registres MIPS")

;;; ============================================================================
;;; HELPER pour obtenir les registres de manière sûre
;;; ============================================================================

(defun get-reg (name)
  "Retourne le symbole de registre depuis la liste (évite problème $ dans CLISP)"
  (case name
    (:sp (nth 29 *register-names*))   ; :$sp
    (:ra (nth 31 *register-names*))   ; :$ra
    (:fp (nth 30 *register-names*))   ; :$fp
    (:pc (nth 32 *register-names*))   ; :$pc
    (:v0 (nth 2 *register-names*))    ; :$v0
    (:a0 (nth 4 *register-names*))    ; :$a0
    (:a1 (nth 5 *register-names*))    ; :$a1
    (:a2 (nth 6 *register-names*))    ; :$a2
    (:a3 (nth 7 *register-names*))    ; :$a3
    (:s0 (nth 16 *register-names*))   ; :$s0
    (:s1 (nth 17 *register-names*))   ; :$s1
    (:s2 (nth 18 *register-names*))   ; :$s2
    (:s3 (nth 19 *register-names*))   ; :$s3
    (:s4 (nth 20 *register-names*))   ; :$s4
    (:s5 (nth 21 *register-names*))   ; :$s5
    (:s6 (nth 22 *register-names*))   ; :$s6
    (:s7 (nth 23 *register-names*))   ; :$s7
    (:t0 (nth 8 *register-names*))    ; :$t0
    (:t1 (nth 9 *register-names*))    ; :$t1
    (:t2 (nth 10 *register-names*))   ; :$t2
    (:t3 (nth 11 *register-names*))   ; :$t3
    (:t4 (nth 12 *register-names*))   ; :$t4
    (:t5 (nth 13 *register-names*))   ; :$t5
    (:t6 (nth 14 *register-names*))   ; :$t6
    (:t7 (nth 15 *register-names*))   ; :$t7
    (:t8 (nth 24 *register-names*))   ; :$t8
    (:t9 (nth 25 *register-names*))   ; :$t9
    (:zero (nth 0 *register-names*))  ; :$zero
    (t (error "Registre inconnu: ~A" name))))

;;; ============================================================================
;;; ZONES MÉMOIRE
;;; ============================================================================

(defconstant +registers-start+ 1
  "Début de la zone des registres en mémoire")

(defconstant +registers-size+ 20
  "Taille de la zone réservée aux registres")

(defconstant +heap-start+ (+ +registers-start+ +registers-size+)
  "Début de la zone tas")

(defparameter *heap-size* 2000
  "Taille initiale du tas")

(defparameter *stack-size* 2000
  "Taille de la pile")

(defparameter *code-size* 5000
  "Taille réservée pour le code")

;;; ============================================================================
;;; FONCTIONS UTILITAIRES
;;; ============================================================================

(defun opcode-p (op)
  "Vérifie si OP est un opcode valide"
  (member op *opcodes*))

(defun register-p (reg)
  "Vérifie si REG est un registre valide"
  (member reg *register-names*))

(defun instruction-arity (opcode)
  "Retourne le nombre d'arguments attendus pour un opcode"
  (case opcode
    ;; 0 arguments
    ((:NOP :HALT :RET) 0)
    ;; 1 argument
    ((:J :JMP :JAL :JR :JALR :JEQ :JNE :JGT :JLT :JGE :JLE :JZ :JNZ :CALL :LABEL :PUSH :POP :PRINT :NOT
      :MFLO :MFHI) 1)
    ;; 2 arguments
    ((:MUL :DIV :MOVE :LOAD :STORE :LOADI :LI :CMP :MALLOC) 2)
    ;; 3 arguments
    ((:ADD :ADDI :SUB :AND :OR :LW :SW :BEQ :BNE :BLT :BGT :SLT
      :EQ :NE :GT :LT :GE :LE :LOAD-HEAP :STORE-HEAP) 3)
    (t (error "Opcode inconnu: ~A" opcode))))

(defun format-instruction (instr)
  "Formate une instruction pour l'affichage"
  (format nil "~{~A~^ ~}" instr))

;;; ============================================================================
;;; VALIDATION
;;; ============================================================================

(defun validate-instruction (instr)
  "Valide une instruction assembleur"
  (unless (listp instr)
    (error "L'instruction doit être une liste: ~A" instr))
  (when (null instr)
    (error "L'instruction ne peut pas être vide"))
  (let ((opcode (first instr))
        (args (rest instr)))
    (unless (opcode-p opcode)
      (error "Opcode invalide: ~A" opcode))
    ;; Instructions avec arité variable (compatibilité)
    (case opcode
      ((:ADD :SUB) (unless (or (= (length args) 2) (= (length args) 3))
                     (error "Nombre d'arguments incorrect pour ~A: attendu 2 ou 3, reçu ~A"
                            opcode (length args))))
      ((:MUL) (unless (or (= (length args) 2))
                (error "Nombre d'arguments incorrect pour ~A: attendu 2, reçu ~A"
                       opcode (length args))))
      (t (let ((expected-arity (instruction-arity opcode)))
           (unless (= (length args) expected-arity)
             (error "Nombre d'arguments incorrect pour ~A: attendu ~A, reçu ~A"
                    opcode expected-arity (length args)))))))
  t)

(defun validate-program (program)
  "Valide un programme assembleur complet"
  (unless (listp program)
    (error "Le programme doit être une liste d'instructions"))
  (dolist (instr program)
    (validate-instruction instr))
  t)
