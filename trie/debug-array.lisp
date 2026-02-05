;;;; Debug pour voir le code généré pour les tableaux

(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(defun test-compile-make-array ()
  "Test compilation de make-array"
  (format t "~%=== Test make-array ===~%")
  (let ((code '(make-array 5)))
    (format t "Code: ~A~%" code)
    (let ((asm (compile-lisp-to-mips-simplified code)))
      (format t "~%Assembleur généré:~%")
      (dolist (instr asm)
        (format t "  ~A~%" instr))
      asm)))

(defun test-compile-aref ()
  "Test compilation de aref"
  (format t "~%=== Test aref ===~%")
  (let ((code '(let ((arr (make-array 5)))
                 (aref arr 2))))
    (format t "Code: ~A~%" code)
    (let ((asm (compile-lisp-to-mips-simplified code)))
      (format t "~%Assembleur généré:~%")
      (dolist (instr asm)
        (format t "  ~A~%" instr))
      asm)))

(defun test-compile-aset ()
  "Test compilation de setf aref"
  (format t "~%=== Test setf aref ===~%")
  (let ((code '(let ((arr (make-array 5)))
                 (setf (aref arr 2) 42))))
    (format t "Code: ~A~%" code)
    (let ((asm (compile-lisp-to-mips-simplified code)))
      (format t "~%Assembleur généré:~%")
      (dolist (instr asm)
        (format t "  ~A~%" instr))
      asm)))

(test-compile-make-array)
(test-compile-aref)
(test-compile-aset)
