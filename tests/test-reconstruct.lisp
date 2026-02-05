#!/usr/bin/env clisp
;;; Test reconstruct-symbols

(load "src/vm.lisp")
(initialize-compiler-symbols)

;; Copier les fonctions de test-true-bootstrap.lisp
(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords pour la comparaison
   INTELLIGENT: ne convertit que les opcodes et registres, pas les constantes"
  (cond
    ((null code) nil)
    ((listp code)
     ;; Si c'est une instruction MIPS (liste avec opcode en [0])
     (if (and (listp (first code))
              (numberp (first (first code))))
         ;; C'est une liste d'instructions
         (mapcar #'reconstruct-instruction code)
         ;; C'est autre chose, traiter récursivement
         (mapcar #'reconstruct-symbols code)))
    (t code)))

(defun reconstruct-instruction (instr)
  "Reconstruit une instruction MIPS: convertit opcode et registres, garde les constantes"
  (if (not (listp instr))
      instr
      (let ((opcode (first instr))
            (args (rest instr)))
        ;; Reconstruire l'instruction avec contexte
        (cons (reconstruct-opcode opcode)
              (reconstruct-args args opcode)))))

(defun reconstruct-opcode (opcode)
  "Convertit un opcode (ID ou keyword) en keyword"
  (if (numberp opcode)
      (let ((name (symbol-name-from-id opcode)))
        (if name
            (intern name :keyword)
            opcode))
      opcode))

(defun reconstruct-args (args opcode-id)
  "Reconstruit les arguments d'une instruction
   Convertit les registres mais garde les constantes numériques"
  (if (null args)
      nil
      (let ((arg (first args)))
        (cons (reconstruct-arg arg)
              (reconstruct-args (rest args) opcode-id)))))

(defun reconstruct-arg (arg)
  "Reconstruit un argument: convertit si c'est un registre, sinon garde tel quel"
  (if (numberp arg)
      (let ((name (symbol-name-from-id arg)))
        ;; Convertir seulement si c'est un registre (commence par $) ou un opcode connu
        (if (and name
                 (or (char= (char name 0) #\$)
                     (member (intern name :keyword) 
                             '(:LI :LW :SW :ADD :SUB :ADDI :LIST :MOVE :JAL :J :JR :HALT
                               :GLOBAL-GET :GLOBAL-SET :BEQ :BNE :BLT :BGT :LABEL))))
            (intern name :keyword)
            ;; Sinon c'est une constante numérique
            arg)))
      arg))

(format t "~%Test 1: Code brut ((1 42 27))~%")
(let ((code '((1 42 27))))
  (format t "  Input:  ~A~%" code)
  (format t "  Output: ~A~%" (reconstruct-symbols code)))

(format t "~%Test 2: Code avec liste d'instructions~%")
(let ((code '((1 42 27) (1 123 27))))
  (format t "  Input:  ~A~%" code)
  (format t "  Output: ~A~%" (reconstruct-symbols code)))
