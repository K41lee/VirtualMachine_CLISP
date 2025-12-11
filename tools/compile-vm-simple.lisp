;;;; Compilation complète de vm-compilable.lisp → MIPS
;;;; Version simplifiée et rapide

(load "src/compiler.lisp")

(format t "~%╔══════════════════════════════════════════════════════════╗~%")
(format t "║  COMPILATION VM-COMPILABLE.LISP → MIPS COMPLET          ║~%")
(format t "╚══════════════════════════════════════════════════════════╝~%~%")

(defun read-all-forms (filename)
  "Lit toutes les formes d'un fichier"
  (with-open-file (stream filename :direction :input)
    (loop for form = (read stream nil nil)
          while form
          collect form)))

(format t "Lecture de src/vm-compilable.lisp...~%")
(defparameter *vm-forms* (read-all-forms "src/vm-compilable.lisp"))
(format t "✓ ~A formes lues~%~%" (length *vm-forms*))

(defparameter *compiled-code* '())
(defparameter *compiled-count* 0)
(defparameter *failed-count* 0)

(format t "Étape 1: Compilation des DEFCONSTANT, DEFVAR et DEFPARAMETER...~%")
(dolist (form *vm-forms*)
  (when (and (consp form) (member (first form) '(defconstant defvar defparameter)))
    (let ((type (first form))
          (name (second form)))
      (handler-case
          (progn
            (let ((code (compile-lisp form)))
              (setf *compiled-code* (append *compiled-code* code))
              (format t "  ✓ ~A ~A (~A instructions)~%" type name (length code))))
        (error (e)
          (format t "  ✗ ~A ~A: ~A~%" type name e))))))

(format t "~%Étape 2: Compilation des DEFUN...~%")
(dolist (form *vm-forms*)
  (when (and (consp form) (eq (first form) 'defun))
    (let ((fn-name (second form)))
      (handler-case
          (progn
            (let ((code (compile-lisp form)))
              (setf *compiled-code* (append *compiled-code* code))
              (incf *compiled-count*)
              (format t "  ✓ ~A (~A instructions)~%" fn-name (length code))))
        (error (e)
          (incf *failed-count*)
          (format t "  ✗ ~A: ~A~%" fn-name e))))))

(format t "~%═══════════════════════════════════════════════════════════~%")
(format t "Compilation terminée :~%")
(format t "  Fonctions compilées : ~A~%" *compiled-count*)
(format t "  Échecs : ~A~%" *failed-count*)
(format t "  Instructions MIPS : ~A~%" (length *compiled-code*))
(format t "═══════════════════════════════════════════════════════════~%~%")

(when (> (length *compiled-code*) 0)
  (format t "Sauvegarde dans output/vm-compiled.mips...~%")
  (ensure-directories-exist "output/")
  (with-open-file (out "output/vm-compiled.mips"
                       :direction :output
                       :if-exists :supersede)
    (dolist (instr *compiled-code*)
      (format out "~A~%" instr)))
  (format t "✓ Fichier sauvegardé (~A instructions)~%~%" (length *compiled-code*)))

(format t "✅ Compilation complète terminée!~%")
