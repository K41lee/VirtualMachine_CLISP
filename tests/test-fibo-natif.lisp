#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST SIMPLE: Affichage du code FIBO généré par le compilateur natif
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")

(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords"
  (cond
    ((null code) nil)
    ((numberp code)
     (let ((sym-name (symbol-name-from-id code)))
       (if (and sym-name
                (or (char= (char sym-name 0) #\$)
                    (find sym-name '("LI" "LW" "SW" "ADD" "SUB" "ADDI" "LIST" "MOVE" 
                                     "JAL" "J" "JR" "HALT" "GLOBAL-GET" "GLOBAL-SET"
                                     "BEQ" "BNE" "BLT" "BGT" "LABEL" "MUL" "DIV")
                          :test #'string=)))
           (intern sym-name :keyword)
           code)))
    ((listp code)
     (mapcar #'reconstruct-symbols code))
    (t code)))

(defun pretty-print-instruction (instr &optional (indent ""))
  "Affiche une instruction de manière lisible"
  (if (and (listp instr) (eq (first instr) :LABEL))
      (format t "~%~A:~%" (second instr))
      (format t "~A~A~%" indent instr)))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║            CODE FIBO - Compilateur Natif                       ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *fibo-def*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

(format t "~%Expression Lisp:~%")
(format t "~A~%" *fibo-def*)

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "Compilation avec le compilateur NATIF...~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *code-brut* (compile-lisp-to-mips-simplified *fibo-def*))

(format t "~%✓ ~A instructions générées~%" (length *code-brut*))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CODE ASSEMBLEUR MIPS (avec IDs numériques):~%")
(format t "════════════════════════════════════════════════════════════════~%")
(dolist (instr *code-brut*)
  (pretty-print-instruction instr "  "))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CODE ASSEMBLEUR MIPS (avec symboles lisibles):~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *code-lisible* (reconstruct-symbols *code-brut*))
(dolist (instr *code-lisible*)
  (pretty-print-instruction instr "  "))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "STATISTIQUES:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(let ((nb-labels 0)
      (nb-jumps 0)
      (nb-arithmetic 0)
      (nb-loads 0)
      (nb-stores 0))
  (dolist (instr *code-lisible*)
    (when (listp instr)
      (let ((op (first instr)))
        (cond
          ((eq op :LABEL) (incf nb-labels))
          ((member op '(:J :JAL :JR :JMP :BEQ :BNE :BLT :BGT)) (incf nb-jumps))
          ((member op '(:ADD :SUB :ADDI :MUL :DIV)) (incf nb-arithmetic))
          ((eq op :LW) (incf nb-loads))
          ((eq op :SW) (incf nb-stores))))))
  
  (format t "  Total instructions: ~A~%" (length *code-lisible*))
  (format t "  Labels:             ~A~%" nb-labels)
  (format t "  Sauts/Branches:     ~A~%" nb-jumps)
  (format t "  Arithmétique:       ~A~%" nb-arithmetic)
  (format t "  Loads (LW):         ~A~%" nb-loads)
  (format t "  Stores (SW):        ~A~%" nb-stores))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "NOTE SUR LE BOOTSTRAP:~%")
(format t "════════════════════════════════════════════════════════════════~%")
(format t "~%Le compilateur compilé dans la VM reçoit actuellement un HANDLE~%")
(format t "(nombre) au lieu de l'expression Lisp elle-même.~%~%")
(format t "Pour compiler réellement FIBO, il faudrait:~%")
(format t "  1. Implémenter une fonction 'compile-from-handle' qui:~%")
(format t "     - Lit l'expression depuis le handle~%")
(format t "     - Appelle compile-lisp-to-mips-simplified avec l'expression~%")
(format t "  2. OU modifier le code pour déréférencer automatiquement~%")
(format t "     les handles avant compilation~%")
(format t "~%Le code ci-dessus est ce que le compilateur DEVRAIT générer~%")
(format t "quand il sera capable de compiler depuis un handle.~%")
(format t "~%════════════════════════════════════════════════════════════════~%")
