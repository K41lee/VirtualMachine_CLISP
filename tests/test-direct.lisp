(sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/asm-ops.lisp")
(load "src/loader.lisp")

(defun test-make-array-direct ()
  "Test direct de MAKE-ARRAY au niveau VM"
  (let ((vm (make-new-vm :verbose t)))  ;; Enable verbose mode
    ;; Charger directement les instructions
    (let ((code '((:LI 5 :$V0)
                  (:MAKE-ARRAY :$V0)
                  (:HALT))))
      (format t "Code:~%")
      (dolist (instr code)
        (format t "  ~A~%" instr))
      
      (load-code vm code)
      (format t "~%Code loaded successfully~%")
      (format t "Avant exécution: $V0 = ~A~%" (get-register vm (get-reg :v0)))
      (format t "Heap pointer: ~A~%" *heap-pointer*)
      
      (format t "~%Starting VM execution...~%")
      (handler-case
          (run-vm vm :max-instructions 100)
        (error (e)
          (format t "ERROR during execution: ~A~%" e)))
      
      (format t "~%Après exécution: $V0 = ~A~%" (get-register vm (get-reg :v0)))
      (format t "Heap pointer: ~A~%" *heap-pointer*)
      
      ;; Lire la mémoire à l'adresse retournée
      (let ((addr (get-register vm (get-reg :v0))))
        (when (and (numberp addr) (>= addr 21))
          (format t "~%Contenu à l'adresse ~A:~%" addr)
          (dotimes (i 6)
            (let ((val (mem-read vm (+ addr i))))
              (format t "  [~A] = ~A~%" (+ addr i) val))))))))

(test-make-array-direct)
