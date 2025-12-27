;;;; ============================================================================
;;;; Test APPEND - Concaténation de listes
;;;; ============================================================================

(load "main.lisp")

(format t "~%═══════════════════════════════════════════════════════════════════~%")
(format t "TEST APPEND - Concaténation de listes~%")
(format t "═══════════════════════════════════════════════════════════════════~%")

;; ============================================================================
;; Test 1 : (append '(1 2) '(3 4)) → (1 2 3 4)
;; ============================================================================

(format t "~%Test 1 : (append '(1 2) '(3 4)) → (1 2 3 4)~%")
(defparameter *test-append-1*
  '(progn
     (let ((list1 (cons 1 (cons 2 nil))))
       (let ((list2 (cons 3 (cons 4 nil))))
         (append list1 list2)))))

(defparameter *mips-append-1* (compile-lisp *test-append-1*))
(format t "  → Code généré : ~A instructions~%" (length *mips-append-1*))

(defparameter *vm1* (make-new-vm))
(load-code *vm1* *mips-append-1*)
(run-vm *vm1*)

(defparameter *result1* (get-register *vm1* '$V0))
(format t "  → Résultat (adresse) : ~A~%" *result1*)

(if (and (> *result1* 0)
         (= (mem-read *vm1* *result1* 0) 1)
         (let ((cdr1 (mem-read *vm1* *result1* 1)))
           (and (> cdr1 0)
                (= (mem-read *vm1* cdr1 0) 2)
                (let ((cdr2 (mem-read *vm1* cdr1 1)))
                  (and (> cdr2 0)
                       (= (mem-read *vm1* cdr2 0) 3)
                       (let ((cdr3 (mem-read *vm1* cdr2 1)))
                         (and (> cdr3 0)
                              (= (mem-read *vm1* cdr3 0) 4)
                              (= (mem-read *vm1* cdr3 1) 0))))))))
    (format t "  ✅ Test 1 RÉUSSI~%")
    (format t "  ❌ Test 1 ÉCHOUÉ~%"))

;; ============================================================================
;; Test 2 : (append nil '(1 2)) → (1 2)
;; ============================================================================

(format t "~%Test 2 : (append nil '(1 2)) → (1 2)~%")
(defparameter *test-append-2*
  '(progn
     (let ((list1 nil))
       (let ((list2 (cons 1 (cons 2 nil))))
         (append list1 list2)))))

(defparameter *mips-append-2* (compile-lisp *test-append-2*))
(format t "  → Code généré : ~A instructions~%" (length *mips-append-2*))

(defparameter *vm2* (make-new-vm))
(load-code *vm2* *mips-append-2*)
(run-vm *vm2*)

(defparameter *result2* (get-register *vm2* '$V0))
(format t "  → Résultat : ~A~%" *result2*)

(if (and (> *result2* 0)
         (= (mem-read *vm2* *result2* 0) 1)
         (let ((cdr1 (mem-read *vm2* *result2* 1)))
           (and (> cdr1 0)
                (= (mem-read *vm2* cdr1 0) 2)
                (= (mem-read *vm2* cdr1 1) 0))))
    (format t "  ✅ Test 2 RÉUSSI~%")
    (format t "  ❌ Test 2 ÉCHOUÉ~%"))

;; ============================================================================
;; Test 3 : (append '(5) nil) → (5)
;; ============================================================================

(format t "~%Test 3 : (append '(5) nil) → (5)~%")
(defparameter *test-append-3*
  '(progn
     (let ((list1 (cons 5 nil)))
       (let ((list2 nil))
         (append list1 list2)))))

(defparameter *mips-append-3* (compile-lisp *test-append-3*))
(format t "  → Code généré : ~A instructions~%" (length *mips-append-3*))

(defparameter *vm3* (make-new-vm))
(load-code *vm3* *mips-append-3*)
(run-vm *vm3*)

(defparameter *result3* (get-register *vm3* '$V0))
(format t "  → Résultat : ~A~%" *result3*)

(if (and (> *result3* 0)
         (= (mem-read *vm3* *result3* 0) 5)
         (= (mem-read *vm3* *result3* 1) 0))
    (format t "  ✅ Test 3 RÉUSSI~%")
    (format t "  ❌ Test 3 ÉCHOUÉ~%"))

;; ============================================================================
;; Test 4 : (append nil nil) → NIL
;; ============================================================================

(format t "~%Test 4 : (append nil nil) → NIL~%")
(defparameter *test-append-4*
  '(progn
     (append nil nil)))

(defparameter *mips-append-4* (compile-lisp *test-append-4*))
(format t "  → Code généré : ~A instructions~%" (length *mips-append-4*))

(defparameter *vm4* (make-new-vm))
(load-code *vm4* *mips-append-4*)
(run-vm *vm4*)

(defparameter *result4* (get-register *vm4* '$V0))
(format t "  → Résultat : ~A~%" *result4*)

(if (= *result4* 0)
    (format t "  ✅ Test 4 RÉUSSI~%")
    (format t "  ❌ Test 4 ÉCHOUÉ~%"))

(format t "~%")
(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                           RÉSUMÉ                                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%")
(format t "~%APPEND implémenté et testé.~%~%")
    
    ;; ========================================================================
    ;; Test 1 : (append '(1 2) '(3 4)) → (1 2 3 4)
    ;; ========================================================================
    
    (format t "Test 1 : (append '(1 2) '(3 4)) → (1 2 3 4)~%")
    (incf total-tests)
    
    (let* ((code (compile-lisp 
                   '(defun test1 ()
                      ;; Construire '(1 2)
                      (let ((list1 (cons 1 (cons 2 nil))))
                        ;; Construire '(3 4)
                        (let ((list2 (cons 3 (cons 4 nil))))
                          ;; Append
                          (append list1 list2))))))
           (asm-code (asm-instructions->vm-code code)))
      
      (format t "  → Code généré : ~A instructions~%" (length code))
      
      ;; Charger et exécuter
      (load-code vm asm-code)
      (let ((result (run-function vm "TEST1" '())))
        (format t "  → Résultat : ~A~%" result)
        
        ;; Vérifier : doit être une liste (1 2 3 4)
        (if (and (> result 0)
                 (= (mem-read vm result 0) 1)       ; CAR = 1
                 (let ((cdr1 (mem-read vm result 1)))
                   (and (> cdr1 0)
                        (= (mem-read vm cdr1 0) 2)   ; CADR = 2
                        (let ((cdr2 (mem-read vm cdr1 1)))
                          (and (> cdr2 0)
                               (= (mem-read vm cdr2 0) 3) ; CADDR = 3
                               (let ((cdr3 (mem-read vm cdr2 1)))
                                 (and (> cdr3 0)
                                      (= (mem-read vm cdr3 0) 4) ; CADDDR = 4
                                      (= (mem-read vm cdr3 1) 0))))))))
            (progn
              (format t "  ✅ Test 1 RÉUSSI (liste concaténée)~%~%")
              (incf passed-tests))
            (format t "  ❌ Test 1 ÉCHOUÉ (résultat incorrect)~%~%"))))
(format t "~%")
(format t "╔══════════════════════════════════════════════════════════════════╗~%")
(format t "║                           RÉSUMÉ                                 ║~%")
(format t "╚══════════════════════════════════════════════════════════════════╝~%")
(format t "~%APPEND implémenté et testé.~%~%")
