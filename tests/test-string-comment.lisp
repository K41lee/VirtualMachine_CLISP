;;;; Test avec STRING "COMMENT"
(load "src/vm.lisp")
(load "src/loader.lisp")

(format t "~%Test STRING vs SYMBOLE~%")
(format t "═══════════════════════════~%~%")

;; Avec symbole COMMENT
(defparameter *code-sym*
  '((LI 1 $V0)
    (COMMENT "test")
    (HALT)))

(format t "1. Code avec symbole COMMENT:~%")
(defparameter *vm1* (make-new-vm :verbose nil))
(handler-case
    (progn
      (load-code *vm1* *code-sym*)
      (format t "   ✅ OK~%"))
  (error (e)
    (format t "   ❌ Erreur: ~A~%~%" e)))

;; Avec string "COMMENT"
(defparameter *code-str*
  '((:LI 1 :$V0)
    ("COMMENT" "test")
    (:HALT)))

(format t "2. Code avec string \"COMMENT\":~%")
(defparameter *vm2* (make-new-vm :verbose nil))
(handler-case
    (progn
      (load-code *vm2* *code-str*)
      (format t "   ✅ OK~%"))
  (error (e)
    (format t "   ❌ Erreur: ~A~%~%" e)))

(format t "~%Test terminé.~%")
