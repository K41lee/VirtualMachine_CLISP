;;; Test simple multi-paramètres avec debug

(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════╗~%")
(format t "║   TEST DEBUG MULTI-PARAMÈTRES         ║~%")
(format t "╚════════════════════════════════════════╝~%")

;; Test le plus simple possible: addition de 2 nombres
(defvar *test-code* 
  '(progn
     (defun add (a b)
       (+ a b))
     (add 10 32)))

(format t "~%Code à compiler:~%~A~%~%" *test-code*)

(defvar *compiled* (compile-lisp-to-mips-simplified *test-code*))

(format t "~%Code compilé (~A instructions):~%" (length *compiled*))
(dolist (instr *compiled*)
  (format t "  ~A~%" instr))

(format t "~%Exécution...~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *compiled*)
(run-vm *vm*)

(defvar *result* (get-register *vm* :$V0))
(format t "~%Résultat: ~A (attendu: 42)~%" *result*)
(if (= *result* 42)
    (format t "✅ SUCCÈS!~%")
    (format t "❌ ÉCHEC!~%"))
