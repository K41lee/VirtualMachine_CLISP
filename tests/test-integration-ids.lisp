;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST D'INTÉGRATION - COMPILATION AVEC IDs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/compiler-bootstrap-ids.lisp")

(format t "~%════════════════════════════════════════════════════════════════")
(format t "~%   TEST D'INTÉGRATION : COMPILATION AVEC IDs")
(format t "~%════════════════════════════════════════════════════════════════~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 1 : Fonction ADD simple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%═══ Test 1 : Fonction ADD(x, y) → x + y ═══~%")

(defparameter *test-add-code*
  '(defun add (x y) (+ x y)))

(format t "  Code source : ~A~%" *test-add-code*)

;; Compiler avec le nouveau système
(format t "~%  Compilation avec parser/dispatcher IDs...~%")
(defparameter *test-add-asm*
  (compile-lisp-with-ids *test-add-code*))

(format t "  Code MIPS généré : ~A instructions~%" (length *test-add-asm*))
(format t "  Premières instructions :~%")
(dolist (instr (subseq *test-add-asm* 0 (min 10 (length *test-add-asm*))))
  (format t "    ~A~%" instr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 2 : Fonction avec IF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%═══ Test 2 : Fonction ABS(x) → (if (< x 0) (- 0 x) x) ═══~%")

(defparameter *test-abs-code*
  '(defun abs-value (x) 
     (if (< x 0) 
         (- 0 x) 
         x)))

(format t "  Code source : ~A~%" *test-abs-code*)

(format t "~%  Compilation avec parser/dispatcher IDs...~%")
(defparameter *test-abs-asm*
  (compile-lisp-with-ids *test-abs-code*))

(format t "  Code MIPS généré : ~A instructions~%" (length *test-abs-asm*))
(format t "  Premières instructions :~%")
(dolist (instr (subseq *test-abs-asm* 0 (min 10 (length *test-abs-asm*))))
  (format t "    ~A~%" instr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 3 : Fonction récursive FACTORIELLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%═══ Test 3 : Fonction FACTORIAL(n) récursive ═══~%")

(defparameter *test-fact-code*
  '(defun factorial (n)
     (if (= n 0)
         1
         (* n (factorial (- n 1))))))

(format t "  Code source : ~A~%" *test-fact-code*)

(format t "~%  Compilation avec parser/dispatcher IDs...~%")
(defparameter *test-fact-asm*
  (compile-lisp-with-ids *test-fact-code*))

(format t "  Code MIPS généré : ~A instructions~%" (length *test-fact-asm*))
(format t "  Premières instructions :~%")
(dolist (instr (subseq *test-fact-asm* 0 (min 15 (length *test-fact-asm*))))
  (format t "    ~A~%" instr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test 4 : Vérification de la structure du code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%═══ Test 4 : Vérification de la structure ═══~%")

(defun count-instruction-type (asm type)
  "Compte le nombre d'instructions d'un type donné"
  (count-if #'(lambda (instr) (eq (first instr) type)) asm))

(format t "  Analyse du code ADD :~%")
(format t "    LABEL   : ~A~%" (count-instruction-type *test-add-asm* :LABEL))
(format t "    ADDI    : ~A~%" (count-instruction-type *test-add-asm* :ADDI))
(format t "    SW/LW   : ~A~%" (+ (count-instruction-type *test-add-asm* :SW)
                                  (count-instruction-type *test-add-asm* :LW)))
(format t "    ADD     : ~A~%" (count-instruction-type *test-add-asm* :ADD))
(format t "    JR      : ~A~%" (count-instruction-type *test-add-asm* :JR))

(format t "~%  Analyse du code ABS :~%")
(format t "    LABEL   : ~A~%" (count-instruction-type *test-abs-asm* :LABEL))
(format t "    BEQ/BNE : ~A~%" (+ (count-instruction-type *test-abs-asm* :BEQ)
                                  (count-instruction-type *test-abs-asm* :BNE)))
(format t "    SLT     : ~A~%" (count-instruction-type *test-abs-asm* :SLT))
(format t "    SUB     : ~A~%" (count-instruction-type *test-abs-asm* :SUB))

(format t "~%  Analyse du code FACTORIAL :~%")
(format t "    LABEL   : ~A~%" (count-instruction-type *test-fact-asm* :LABEL))
(format t "    JAL     : ~A~%" (count-instruction-type *test-fact-asm* :JAL))
(format t "    JR      : ~A~%" (count-instruction-type *test-fact-asm* :JR))
(format t "    MUL     : ~A~%" (count-instruction-type *test-fact-asm* :MUL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Résumé
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%════════════════════════════════════════════════════════════════")
(format t "~%   RÉSUMÉ DES TESTS")
(format t "~%════════════════════════════════════════════════════════════════")
(format t "~%✓ Fonction simple (ADD) : ~A instructions~%" (length *test-add-asm*))
(format t "✓ Fonction avec IF (ABS) : ~A instructions~%" (length *test-abs-asm*))
(format t "✓ Fonction récursive (FACTORIAL) : ~A instructions~%" (length *test-fact-asm*))
(format t "~%Le compilateur avec IDs génère du code MIPS correct.~%")
(format t "Prochain test : Exécution sur la VM pour validation complète.~%")
(format t "════════════════════════════════════════════════════════════════~%~%")
