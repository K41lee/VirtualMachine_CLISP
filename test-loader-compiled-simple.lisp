;;;; ============================================================================
;;;; TEST: Loader compilé avec un petit programme
;;;; ============================================================================

(load "main.lisp")

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  TEST: Loader compilé + petit programme               ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

;;; Compiler le loader
(defparameter *loader-source*
  '(defun simple-loader (code-addr data-addr count)
     "Charge 'count' instructions depuis data-addr vers code-addr"
     (let ((i 0))
       (while (< i count)
         (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
         (setq i (+ i 1)))
       code-addr)))

(format t "Compilation du loader...~%")
(defparameter *loader-mips* (compile-lisp *loader-source*))
(format t "✓ Loader compilé: ~A instructions~%~%" (length *loader-mips*))

;;; Compiler un petit programme de test: addition simple
(defparameter *test-prog-source*
  '(progn
     (defun add-five (n)
       (+ n 5))
     (add-five 10)))

(format t "Compilation du programme de test...~%")
(defparameter *test-prog-mips* 
  (append (compile-lisp *test-prog-source*)
          (list (list :PRINT *reg-v0*)
                (list :HALT))))
(format t "✓ Programme compilé: ~A instructions~%~%" (length *test-prog-mips*))

;;; Créer une VM et charger le loader
(defparameter *vm* (make-new-vm))
(load-code *vm* *loader-mips*)
(defparameter *loader-addr* (calculate-code-start *vm*))
(format t "Loader chargé à: ~A~%~%" *loader-addr*)

;;; Écrire le programme de test dans la zone DATA
(defparameter *data-zone* 10481000)
(defparameter *code-zone* 10482000)

(format t "Écriture du programme en zone DATA (~A)...~%"  *data-zone*)
(multiple-value-bind (resolved-prog labels)
    (preprocess-code *test-prog-mips* *data-zone*)
  (let ((addr *data-zone*))
    (dolist (instr resolved-prog)
      (mem-write *vm* addr instr)
      (incf addr))))
(format t "✓ ~A instructions écrites~%~%" (length *test-prog-mips*))

;;; Créer le code bootstrap
(defparameter *bootstrap-addr* (+ *loader-addr* (length *loader-mips*)))
(defparameter *bootstrap-code*
  (list
   (list :LI *code-zone* *reg-a0*)              ; dest
   (list :LI *data-zone* *reg-a1*)              ; src
   (list :LI (length *test-prog-mips*) *reg-a2*) ; count
   (list :JAL *loader-addr*)                    ; call loader
   (list :LI *code-zone* *reg-t0*)              ; prep jump
   (list :JR *reg-t0*)))                        ; execute

(format t "Écriture du bootstrap (~A)...~%" *bootstrap-addr*)
(multiple-value-bind (resolved-bootstrap labels)
    (preprocess-code *bootstrap-code* *bootstrap-addr*)
  (let ((addr *bootstrap-addr*))
    (dolist (instr resolved-bootstrap)
      (mem-write *vm* addr instr)
      (incf addr))))
(format t "✓ Bootstrap écrit~%~%")

;;; Exécuter
(set-register *vm* (get-reg :pc) *bootstrap-addr*)
(format t "Exécution...~%")
(defparameter *start-time* (get-internal-real-time))
(run-vm *vm*)
(defparameter *end-time* (get-internal-real-time))
(defparameter *exec-time* (/ (- *end-time* *start-time*) internal-time-units-per-second))

(format t "~%Temps d'exécution: ~,2F secondes~%" *exec-time*)
(format t "Instructions exécutées: ~:D~%" (vm-instruction-count *vm*))
(format t "Résultat: ~A (attendu: 15)~%~%" (get-register *vm* *reg-v0*))

;;; Vérifier que le code a été copié
(format t "Vérification de la copie:~%")
(format t "  Première instr DATA:   ~A~%" (mem-read *vm* *data-zone*))
(format t "  Première instr CODE:   ~A~%" (mem-read *vm* *code-zone*))

(if (= (get-register *vm* *reg-v0*) 15)
    (format t "~%✓✓✓ TEST RÉUSSI! ✓✓✓~%")
    (format t "~%✗✗✗ TEST ÉCHOUÉ ✗✗✗~%"))

(format t "~%Test terminé.~%")
