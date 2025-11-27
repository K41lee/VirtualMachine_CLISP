;;; Test de compilation en lisant le source directement

(load "src/compiler.lisp")
(load "src/asm-ops.lisp")  ; Charger les constantes nécessaires

(format t "~%╔════════════════════════════════════════════════════════╗~%")
(format t "║  COMPILATION : Lecture directe du source              ║~%")
(format t "╚════════════════════════════════════════════════════════╝~%~%")

;; Lire toutes les expressions du fichier
(defparameter *defuns* '())
(defparameter *other-forms* '())

(with-open-file (stream "src/vm-compilable.lisp" :direction :input)
  (do ((form (read stream nil 'eof) (read stream nil 'eof)))
      ((eq form 'eof))
    (when (and (consp form) (eq (first form) 'defun))
      (push form *defuns*))
    (when (and (consp form) 
               (member (first form) '(defvar defconstant defparameter)))
      (push form *other-forms*))))

;; Compiler les DEFVAR/DEFCONSTANT d'abord pour peupler les tables globales
(format t "Compilation des variables et constantes globales...~%")
(dolist (form (reverse *other-forms*))
  (handler-case
      (progn
        (compile-lisp form)
        (format t "."))
    (error (e)
      (format t "!"))))
(format t " OK~%~%")

(format t "Trouvé ~A DEFUNs et ~A autres formes~%~%" 
        (length *defuns*) 
        (length *other-forms*))

(defparameter *compiled* 0)
(defparameter *failed* 0)
(defparameter *total-instructions* 0)
(defparameter *errors* '())

(format t "Compilation des fonctions :~%~%")

(dolist (defun-form (reverse *defuns*))
  (let ((fn-name (second defun-form)))
    (format t "~A : " fn-name)
    (handler-case
        (let ((compiled-code (compile-lisp defun-form)))
          (incf *compiled*)
          (incf *total-instructions* (length compiled-code))
          (format t "✓ ~A instructions~%" (length compiled-code)))
      (error (e)
        (incf *failed*)
        (push (cons fn-name (format nil "~A" e)) *errors*)
        (format t "✗ ~A~%" e)))))

(let ((total (+ *compiled* *failed*)))
  (format t "~%╔════════════════════════════════════════════════════════╗~%")
  (format t "║  RÉSULTATS                                             ║~%")
  (format t "╚════════════════════════════════════════════════════════╝~%~%")
  
  (format t "Total DEFUN          : ~A~%" total)
  (format t "Compilées            : ~A~%" *compiled*)
  (format t "Échecs               : ~A~%" *failed*)
  (format t "Taux                 : ~A%~%~%" 
          (if (> total 0)
              (round (* 100 (/ *compiled* total)))
              0))
  
  (format t "Total instructions   : ~A~%~%" *total-instructions*)
  
  (when (> *failed* 0)
    (format t "~%═══ ÉCHECS ═══~%~%")
    (dolist (err (reverse *errors*))
      (format t "✗ ~A~%  → ~A~%~%" (car err) (cdr err))))
  
  (let ((pct (if (> total 0)
                 (round (* 100 (/ *compiled* total)))
                 0)))
    (format t "~%Session 3 : 62%% (16/26 DEFUN, 1130 lignes)~%")
    (format t "Session 4 : ~A%% (~A/~A DEFUN, ~A lignes)~%~%" 
            pct *compiled* total *total-instructions*)
    
    (cond
      ((>= pct 77)
       (format t "🎉 OBJECTIF 77%% ATTEINT ! (~A%%)~%" pct))
      ((>= pct 70)
       (format t "Proche : ~A%% / 77%% (manque ~A%%)~%" pct (- 77 pct)))
      (t
       (format t "Progression : 62%% → ~A%% (+~A points)~%" pct (- pct 62))))))
