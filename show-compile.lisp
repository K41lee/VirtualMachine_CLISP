#!/usr/bin/env clisp
;;;; ============================================================================
;;;; SHOW-COMPILE - Affiche le code compilé d'une fonction
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")

;;; ============================================================================
;;; Fonction utilitaire pour afficher le code
;;; ============================================================================

(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords pour un affichage lisible"
  (cond
    ((null code) nil)
    ((numberp code)
     (let ((sym-name (symbol-name-from-id code)))
       (if (and sym-name
                (or (char= (char sym-name 0) #\$)
                    (find sym-name '("LI" "LW" "SW" "ADD" "SUB" "ADDI" "LIST" "MOVE" 
                                     "JAL" "J" "JR" "HALT" "GLOBAL-GET" "GLOBAL-SET"
                                     "BEQ" "BNE" "BLT" "BGT" "LABEL" "MUL" "DIV" "JMP")
                          :test #'string=)))
           (intern sym-name :keyword)
           code)))
    ((listp code)
     (mapcar #'reconstruct-symbols code))
    (t code)))

(defun pretty-print-instruction (instr &optional (indent "    "))
  "Affiche une instruction de manière lisible avec indentation"
  (if (and (listp instr) (eq (first instr) :LABEL))
      (format t "~%~A:~%" (second instr))
      (format t "~A~A~%" indent instr)))

;;; ============================================================================
;;; Fonction à compiler: FIBONACCI
;;; ============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║              COMPILATION DE FIBONACCI                          ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *fibonacci-def*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

(format t "~%Expression Lisp:~%")
(format t "────────────────────────────────────────────────────────────────~%")
(format t "(defun fibo (n)~%")
(format t "  (if (< n 2)~%")
(format t "      n~%")
(format t "      (+ (fibo (- n 1))~%")
(format t "         (fibo (- n 2)))))~%")

;;; ============================================================================
;;; Compilation
;;; ============================================================================

(format t "~%Compilation en cours...~%")
(defparameter *code-brut* (compile-lisp-to-mips-simplified *fibonacci-def*))

(format t "✓ ~A instructions générées~%" (length *code-brut*))

;;; ============================================================================
;;; Affichage du code compilé (avec IDs)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CODE ASSEMBLEUR MIPS (IDs numériques):~%")
(format t "════════════════════════════════════════════════════════════════~%")

(dolist (instr *code-brut*)
  (pretty-print-instruction instr "  "))

;;; ============================================================================
;;; Affichage du code compilé (avec symboles)
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CODE ASSEMBLEUR MIPS (symboles lisibles):~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *code-lisible* (reconstruct-symbols *code-brut*))

(dolist (instr *code-lisible*)
  (pretty-print-instruction instr "  "))

;;; ============================================================================
;;; Statistiques
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "STATISTIQUES:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(let ((nb-labels 0)
      (nb-jumps 0)
      (nb-arithmetic 0)
      (nb-loads 0)
      (nb-stores 0)
      (nb-other 0))
  
  (dolist (instr *code-lisible*)
    (when (listp instr)
      (let ((op (first instr)))
        (cond
          ((eq op :LABEL) (incf nb-labels))
          ((member op '(:J :JAL :JR :JMP :BEQ :BNE :BLT :BGT)) (incf nb-jumps))
          ((member op '(:ADD :SUB :ADDI :MUL :DIV)) (incf nb-arithmetic))
          ((eq op :LW) (incf nb-loads))
          ((eq op :SW) (incf nb-stores))
          (t (incf nb-other))))))
  
  (format t "~%  Total instructions: ~A~%" (length *code-lisible*))
  (format t "  ├─ Labels:           ~A~%" nb-labels)
  (format t "  ├─ Sauts/Branches:   ~A~%" nb-jumps)
  (format t "  ├─ Arithmétique:     ~A~%" nb-arithmetic)
  (format t "  ├─ Loads (LW):       ~A~%" nb-loads)
  (format t "  ├─ Stores (SW):      ~A~%" nb-stores)
  (format t "  └─ Autres:           ~A~%" nb-other))

;;; ============================================================================
;;; Structure du code
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "STRUCTURE DU CODE:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%1. SAUT INITIAL~%")
(format t "   └─ J FIBO_END (évite l'exécution accidentelle)~%")

(format t "~%2. PROLOGUE (sauvegarde contexte)~%")
(format t "   ├─ ADDI $SP -12 $SP  (allouer espace pile)~%")
(format t "   ├─ SW $RA $SP 0      (sauver adresse retour)~%")
(format t "   ├─ SW $FP $SP 4      (sauver frame pointer)~%")
(format t "   ├─ MOVE $SP $FP      (nouveau frame)~%")
(format t "   └─ SW $A0 $FP 8      (sauver argument n)~%")

(format t "~%3. TEST (n < 2)~%")
(format t "   ├─ LW $V0 $FP 8      (charger n)~%")
(format t "   ├─ LI 2 $V0          (constante 2)~%")
(format t "   ├─ BLT $T0 $V0 ...   (si n < 2)~%")
(format t "   └─ BEQ ... ELSE      (sinon)~%")

(format t "~%4. CAS DE BASE (n < 2)~%")
(format t "   └─ LW $V0 $FP 8      (retourner n)~%")

(format t "~%5. CAS RÉCURSIF (n >= 2)~%")
(format t "   ├─ Calcul fibo(n-1)~%")
(format t "   │  ├─ SUB ... (n-1)~%")
(format t "   │  ├─ JAL FIBO~%")
(format t "   │  └─ Sauvegarder résultat~%")
(format t "   ├─ Calcul fibo(n-2)~%")
(format t "   │  ├─ SUB ... (n-2)~%")
(format t "   │  ├─ JAL FIBO~%")
(format t "   │  └─ Récupérer résultat~%")
(format t "   └─ ADD (additionner les deux résultats)~%")

(format t "~%6. ÉPILOGUE (restauration contexte)~%")
(format t "   ├─ LW $FP $SP 4      (restaurer frame pointer)~%")
(format t "   ├─ LW $RA $SP 0      (restaurer adresse retour)~%")
(format t "   ├─ ADDI $SP 12 $SP   (libérer pile)~%")
(format t "   └─ JR $RA            (retour)~%")

(format t "~%7. LABEL DE FIN~%")
(format t "   └─ FIBO_END: LI 0 $V0 (valeur par défaut)~%")

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "~%Compilation terminée avec succès!~%")
(format t "════════════════════════════════════════════════════════════════~%~%")
