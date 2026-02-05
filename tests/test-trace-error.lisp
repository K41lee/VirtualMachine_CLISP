;;;; Test minimal pour trouver où se produit l'erreur SYMBOL-NAME
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%Test trace erreur SYMBOL-NAME~%")
(format t "═══════════════════════════════~%~%")

;; Code test avec COMMENT
(defparameter *test-code*
  '((LI 1 $V0)
    (COMMENT "test comment")
    (ADDI $V0 1 $V0)
    (HALT)))

(format t "Code test: ~A~%~%" *test-code*)

(format t "1. Test parse-asm...~%")
(defparameter *parsed* (parse-asm *test-code*))
(format t "   → OK: ~A~%~%" *parsed*)

(format t "2. Test collect-labels...~%")
(defparameter *labels* (collect-labels *parsed* 900000))
(format t "   → OK: ~A labels~%~%" (hash-table-count *labels*))

(format t "3. Test resolve-labels...~%")
(defparameter *resolved* (resolve-labels *parsed* *labels*))
(format t "   → OK: ~A instructions~%~%" (length *resolved*))

(format t "4. Test chargement dans VM...~%")
(defparameter *vm* (make-new-vm :verbose nil))
(handler-case
    (progn
      (load-code *vm* *test-code*)
      (format t "   → ✅ OK!~%"))
  (error (e)
    (format t "   → ❌ Erreur: ~A~%" e)))

(format t "~%Test terminé.~%")
