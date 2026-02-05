;;;; analyse-code-gen.lisp
;;;; Compare le code généré par les deux compilateurs

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")

(defvar *test-code* '(defun fibo (n)
                       (if (< n 2)
                           n
                           (+ (fibo (- n 1))
                              (fibo (- n 2))))))

(format t "~%═══════════════════════════════════════════════════════~%")
(format t "COMPILATION AVEC LE COMPILATEUR ORIGINAL~%")
(format t "═══════════════════════════════════════════════════════~%~%")

(defvar *code-original* (compile-lisp *test-code*))
(format t "~A instructions générées~%~%" (length *code-original*))

(format t "Instructions originales:~%")
(loop for inst in *code-original*
      for i from 0
      do (format t "~2D: ~A~%" i inst))

(format t "~%~%═══════════════════════════════════════════════════════~%")
(format t "COMPILATION AVEC LE COMPILATEUR SIMPLIFIÉ~%")
(format t "═══════════════════════════════════════════════════════~%~%")

(defvar *code-simplified* (compile-lisp-to-mips-simplified *test-code*))
(format t "~A instructions générées~%~%" (length *code-simplified*))

(format t "Instructions simplifiées:~%")
(loop for inst in *code-simplified*
      for i from 0
      do (format t "~2D: ~A~%" i inst))

(format t "~%~%═══════════════════════════════════════════════════════~%")
(format t "DIFFÉRENCES CLÉS~%")
(format t "═══════════════════════════════════════════════════════~%~%")

(format t "Original:   ~A instructions~%" (length *code-original*))
(format t "Simplifié:  ~A instructions~%" (length *code-simplified*))
(format t "~%")
