;;;; test-call.lisp  
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

;; Test: main appelle helper
(defvar *code-str* "(progn (defun helper (n) (+ n 10)) (defun main (n) (helper n)))")
(defvar *code* (read-from-string *code-str*))

;; Compiler les deux fonctions
(defvar *helper* (compile-lisp-to-mips-simplified '(defun helper (n) (+ n 10))))
(defvar *main* (compile-lisp-to-mips-simplified '(defun main (n) (helper n))))
(defvar *combined* (append *helper* *main*))

(format t "~%Code combiné (~D instructions):~%" (length *combined*))
(loop for instr in *combined*
      for i from 0 to 50
      do (format t "~3D: ~A~%" i instr))

(format t "~%~%Test: main(5) devrait appeler helper(5) → 15~%")
(defvar *vm* (make-new-vm))
(load-code *vm* *combined*)
;; On veut exécuter MAIN, pas HELPER
;; Trouver l'adresse de MAIN
(format t "Code start: ~A~%" (calculate-code-start *vm*))
(format t "Instructions HELPER: ~A~%" (length *helper*))
(format t "Instructions MAIN: ~A~%" (length *main*))
;; PC devrait pointer vers MAIN
(set-register *vm* :$PC (+ (calculate-code-start *vm*) (length *helper*)))
(set-register *vm* :$A0 5)
;; Initialiser $RA pour que MAIN puisse retourner
(set-register *vm* :$RA (+ (calculate-code-start *vm*) (length *combined*)))
(run-vm *vm* :max-instructions 200)
(format t "Résultat: ~A (attendu 15)~%" (get-register *vm* :$V0))
