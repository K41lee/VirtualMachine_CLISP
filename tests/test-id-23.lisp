(load "src/asm-ops.lisp")
(load "src/vm.lisp")

(initialize-compiler-symbols)

(format t "ID 23 correspond à: ~A~%" (symbol-name-from-id 23))
(format t "Keyword: ~A~%" (intern (symbol-name-from-id 23) :keyword))
(format t "Dans opcodes? ~A~%" (member (intern (symbol-name-from-id 23) :keyword) *opcodes*))
