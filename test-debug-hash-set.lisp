;;; Test simplifié pour déboguer le problème HASH-SET

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST DÉBOGAGE: collect-labels simplifié~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

;; Version sans let imbriqué
(defparameter *src1*
  '(defun test-simple (lst)
     (let ((h (vm-make-hash-table :test 'equal))
           (pos 0))
       (dolist (elem lst)
         (vm-hash-set h elem pos)
         (setq pos (+ pos 1)))
       h)))

(format t "Test 1: Version simple (sans CAR)~%")
(defparameter *mips1* (compile-lisp *src1*))
(format t "  Compilé: ~A instructions~%~%" (length *mips1*))

;; Test d'exécution
(defparameter *vm1* (make-new-vm :verbose nil))
(load-code *vm1* *mips1* :verbose nil)
(defparameter *addr1* (calculate-code-start *vm1*))

(defparameter *test-list* '(a b c))
(defparameter *handle1*
  (let ((h (incf *vm-lisp-handle-counter*)))
    (setf (gethash h *vm-lisp-objects*) *test-list*)
    h))

(set-register *vm1* *reg-a0* *handle1*)
(set-register *vm1* *reg-ra* 999999)
(set-register *vm1* (get-reg :pc) *addr1*)

(format t "  Exécution...~%")
(handler-case
    (progn
      (run-vm *vm1* :max-instructions 10000)
      (defparameter *res1* (get-register *vm1* *reg-v0*))
      (defparameter *hash1* (gethash *res1* *vm-hash-tables*))
      (if *hash1*
          (format t "  ✅ Test 1 réussi: ~A entrées~%~%" (hash-table-count *hash1*))
          (format t "  ✗ Test 1 échoué~%~%")))
  (error (e)
    (format t "  ✗ Erreur test 1: ~A~%~%" e)))

;; Version avec CAR mais sans let imbriqué
(defparameter *src2*
  '(defun test-with-car (lst)
     (let ((h (vm-make-hash-table :test 'equal))
           (pos 0)
           (elem-car nil))
       (dolist (elem lst)
         (setq elem-car (vm-car elem))
         (vm-hash-set h elem-car pos)
         (setq pos (+ pos 1)))
       h)))

(format t "Test 2: Version avec CAR (sans let imbriqué)~%")
(defparameter *mips2* (compile-lisp *src2*))
(format t "  Compilé: ~A instructions~%~%" (length *mips2*))

;; Test d'exécution
(defparameter *vm2* (make-new-vm :verbose nil))
(load-code *vm2* *mips2* :verbose nil)
(defparameter *addr2* (calculate-code-start *vm2*))

(defparameter *test-list2* (list (list :A 1) (list :B 2) (list :C 3)))
(defparameter *handle2*
  (let ((h (incf *vm-lisp-handle-counter*)))
    (setf (gethash h *vm-lisp-objects*) *test-list2*)
    h))

(set-register *vm2* *reg-a0* *handle2*)
(set-register *vm2* *reg-ra* 999999)
(set-register *vm2* (get-reg :pc) *addr2*)

(format t "  Exécution...~%")
(handler-case
    (progn
      (run-vm *vm2* :max-instructions 10000)
      (defparameter *res2* (get-register *vm2* *reg-v0*))
      (defparameter *hash2* (gethash *res2* *vm-hash-tables*))
      (if *hash2*
          (progn
            (format t "  ✅ Test 2 réussi: ~A entrées~%"  (hash-table-count *hash2*))
            (maphash #'(lambda (k v)
                        (let ((key-obj (gethash k *vm-lisp-objects* k)))
                          (format t "       ~A → ~A~%" key-obj v)))
                     *hash2*)
            (format t "~%"))
          (format t "  ✗ Test 2 échoué~%~%")))
  (error (e)
    (format t "  ✗ Erreur test 2: ~A~%~%" e)))

(format t "Tests terminés.~%~%")
