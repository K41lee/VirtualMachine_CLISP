(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defvar *code* '(progn
                  (defun double (x)
                    (+ x x))
                  (double 21)))

(defvar *compiled* (compile-lisp-to-mips-simplified *code*))
(format t "Instructions (~A):~%" (length *compiled*))
(dotimes (i (min 20 (length *compiled*)))
  (format t "~2D: ~A~%" i (nth i *compiled*)))
