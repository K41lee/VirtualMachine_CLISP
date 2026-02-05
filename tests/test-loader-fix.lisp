;;;; Test de chargement du compilateur compilé
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%Test de chargement du compilateur compilé~%")
(format t "════════════════════════════════════════════~%~%")

;; Compiler une fonction simple
(defparameter *test-fn* '(defun add-two (a b) (+ a b)))
(format t "Compilation de: ~A~%" *test-fn*)
(defparameter *compiled* (compile-lisp-to-mips-simplified *test-fn*))
(format t "  → ~A instructions~%~%" (length *compiled*))

;; Tester le chargement
(format t "Test de chargement dans la VM...~%")
(defparameter *test-vm* (make-new-vm :verbose nil))

(handler-case
    (progn
      (load-code *test-vm* *compiled*)
      (format t "  ✅ Chargement réussi!~%~%"))
  (error (e)
    (format t "  ❌ Erreur: ~A~%~%" e)))

;; Compiler plusieurs fonctions du compilateur
(format t "Compilation de 10 fonctions du compilateur...~%")
(defun read-file-as-sexps (filename)
  (with-open-file (stream filename :direction :input)
    (let ((sexps nil))
      (handler-case
          (loop
            (let ((sexp (read stream nil :eof)))
              (if (eq sexp :eof)
                  (return (nreverse sexps))
                  (when (listp sexp) (push sexp sexps)))))
        (end-of-file () (nreverse sexps))))))

(defparameter *compiler-sexps* (read-file-as-sexps "src/compiler-simplified.lisp"))
(defparameter *compiler-defuns* 
  (remove-if-not (lambda (sexp) 
                   (and (listp sexp) (eq (first sexp) 'defun)))
                 *compiler-sexps*))

(defparameter *compiled-functions* nil)
(let ((count 0))
  (dolist (defun-form (subseq *compiler-defuns* 0 10))
    (handler-case
        (let ((code (compile-lisp-to-mips-simplified defun-form)))
          (push (list (second defun-form) code) *compiled-functions*)
          (incf count)
          (format t "  ✓ ~A (~A instructions)~%" (second defun-form) (length code)))
      (error (e)
        (format t "  ✗ ~A: ~A~%" (second defun-form) e))))
  (format t "~%Total: ~A/10 compilées~%~%" count))

;; Tester le chargement groupé
(format t "Test de chargement groupé...~%")
(defparameter *all-code* nil)
(dolist (entry *compiled-functions*)
  (setf *all-code* (append *all-code* (second entry))))

(format t "  Code total: ~A instructions~%" (length *all-code*))

(defparameter *test-vm2* (make-new-vm :verbose nil))
(handler-case
    (progn
      (load-code *test-vm2* *all-code*)
      (format t "  ✅ Chargement groupé réussi!~%"))
  (error (e)
    (format t "  ❌ Erreur: ~A~%" e)))

(format t "~%Test terminé.~%")
