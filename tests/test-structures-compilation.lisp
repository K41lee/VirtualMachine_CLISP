;;;; Test des structures de données avec délégation à Lisp
;;;;

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST STRUCTURES DE DONNÉES - Délégation à Lisp            ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; Test 1: QUOTE
(format t "~%Test 1: QUOTE~%")
(defparameter test-quote
  '(defun test-quote (n)
     (if (= n 0)
         'ZERO
         'NONZERO)))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-quote)))
      (if compiled
          (format t "  ✅ QUOTE compile~%")
          (format t "  ❌ QUOTE ne compile pas~%")))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 2: CONS/CAR/CDR
(format t "~%Test 2: CONS/CAR/CDR~%")
(defparameter test-cons
  '(defun test-cons (a b)
     (cons a b)))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-cons)))
      (if compiled
          (format t "  ✅ CONS compile (~A instructions)~%" (length compiled))
          (format t "  ❌ CONS ne compile pas~%")))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 3: LIST
(format t "~%Test 3: LIST~%")
(defparameter test-list
  '(defun test-list (a b c)
     (list a b c)))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-list)))
      (if compiled
          (format t "  ✅ LIST compile (~A instructions)~%" (length compiled))
          (format t "  ❌ LIST ne compile pas~%")))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 4: Algo avec listes - Longueur de liste
(format t "~%Test 4: Longueur de liste récursive~%")
(defparameter test-length
  '(defun my-length (lst)
     (if (null lst)
         0
         (+ 1 (my-length (cdr lst))))))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-length)))
      (if compiled
          (format t "  ✅ my-length compile (~A instructions)~%" (length compiled))
          (format t "  ❌ my-length ne compile pas~%")))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 5: Somme d'une liste
(format t "~%Test 5: Somme d'une liste~%")
(defparameter test-sum-list
  '(defun sum-list (lst)
     (if (null lst)
         0
         (+ (car lst) (sum-list (cdr lst))))))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-sum-list)))
      (if compiled
          (format t "  ✅ sum-list compile (~A instructions)~%" (length compiled))
          (format t "  ❌ sum-list ne compile pas~%")))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

;;; Test 6: Reverse de liste
(format t "~%Test 6: Reverse de liste~%")
(defparameter test-reverse
  '(defun my-reverse (lst)
     (if (null lst)
         nil
         (append (my-reverse (cdr lst)) (list (car lst))))))

(handler-case
    (let* ((compiled (compile-lisp-to-mips-simplified test-reverse)))
      (if compiled
          (format t "  ✅ my-reverse compile (~A instructions)~%" (length compiled))
          (format t "  ❌ my-reverse ne compile pas~%")))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                    FIN DES TESTS DE COMPILATION               ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Note: Ces tests vérifient la compilation uniquement.~%")
(format t "L'exécution nécessite que la VM supporte les opérations sur listes.~%~%")

(quit)
