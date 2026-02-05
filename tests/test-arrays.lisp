;;; Tests pour les tableaux (arrays/matrices)

;; Unlock package only for SBCL
#+sbcl (sb-ext:unlock-package :common-lisp)

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TESTS TABLEAUX ET MATRICES                                  ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-array (name code expected)
  "Teste une opération sur tableau"
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let* ((compiled (compile-lisp-to-mips-simplified code))
             (vm (make-new-vm)))
        (if (null compiled)
            (progn
              (format t "  ❌ ÉCHEC COMPILATION~%")
              (incf *tests-failed*))
            (progn
              (load-code vm compiled)
              (run-vm vm)
              (let ((result (get-register vm :$V0)))
                (if (equal result expected)
                    (progn
                      (format t "  ✅ = ~A~%" result)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ Attendu ~A, obtenu ~A~%" expected result)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

;; ============================================================================
;; TEST 1: Création de tableau simple
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TEST 1: CRÉATION DE TABLEAUX~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-array
  "make-array simple"
  '(progn
     (defun test-create ()
       (let ((arr (make-array 5)))
         arr))
     (test-create))
  21)  ; Devrait retourner l'adresse heap (21 = heap-start)

;; ============================================================================
;; TEST 2: Accès aux éléments
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TEST 2: ACCÈS AUX ÉLÉMENTS~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-array
  "aref simple (valeur par défaut)"
  '(progn
     (defun test-aref ()
       (let ((arr (make-array 5)))
         (aref arr 0)))
     (test-aref))
  0)  ; Les éléments sont initialisés à 0

;; ============================================================================
;; TEST 3: Modification d'éléments
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TEST 3: MODIFICATION D'ÉLÉMENTS~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-array
  "setf aref"
  '(progn
     (defun test-setf ()
       (let ((arr (make-array 5)))
         (setf (aref arr 0) 42)
         (aref arr 0)))
     (test-setf))
  42)

(test-array
  "setf plusieurs éléments"
  '(progn
     (defun test-multiple ()
       (let ((arr (make-array 5)))
         (setf (aref arr 0) 10)
         (setf (aref arr 1) 20)
         (setf (aref arr 2) 30)
         (+ (aref arr 0) (+ (aref arr 1) (aref arr 2)))))
     (test-multiple))
  60)

;; ============================================================================
;; TEST 4: Fonctions avec tableaux
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TEST 4: FONCTIONS AVEC TABLEAUX~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-array
  "Somme des éléments"
  '(progn
     (defun array-sum (arr n)
       (if (= n 0)
           0
           (+ (aref arr (- n 1)) (array-sum arr (- n 1)))))
     
     (defun test-sum ()
       (let ((arr (make-array 4)))
         (setf (aref arr 0) 1)
         (setf (aref arr 1) 2)
         (setf (aref arr 2) 3)
         (setf (aref arr 3) 4)
         (array-sum arr 4)))
     
     (test-sum))
  10)  ; 1+2+3+4 = 10

(test-array
  "Trouver le maximum"
  '(progn
     (defun array-max (arr n current-max)
       (if (= n 0)
           current-max
           (let ((elem (aref arr (- n 1))))
             (if (> elem current-max)
                 (array-max arr (- n 1) elem)
                 (array-max arr (- n 1) current-max)))))
     
     (defun test-max ()
       (let ((arr (make-array 5)))
         (setf (aref arr 0) 3)
         (setf (aref arr 1) 7)
         (setf (aref arr 2) 2)
         (setf (aref arr 3) 9)
         (setf (aref arr 4) 5)
         (array-max arr 5 0)))
     
     (test-max))
  9)

;; ============================================================================
;; TEST 5: Remplir un tableau
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TEST 5: REMPLIR UN TABLEAU~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-array
  "Remplir avec une valeur constante"
  '(progn
     (defun fill-array (arr n value)
       (if (= n 0)
           arr
           (progn
             (setf (aref arr (- n 1)) value)
             (fill-array arr (- n 1) value))))
     
     (defun test-fill ()
       (let ((arr (make-array 3)))
         (fill-array arr 3 7)
         (+ (aref arr 0) (+ (aref arr 1) (aref arr 2)))))
     
     (test-fill))
  21)  ; 7+7+7 = 21

(test-array
  "Remplir avec suite arithmétique"
  '(progn
     (defun fill-sequence (arr n)
       (if (= n 0)
           arr
           (progn
             (setf (aref arr (- n 1)) n)
             (fill-sequence arr (- n 1)))))
     
     (defun test-sequence ()
       (let ((arr (make-array 5)))
         (fill-sequence arr 5)
         (+ (aref arr 0) (+ (aref arr 1) (+ (aref arr 2) (+ (aref arr 3) (aref arr 4)))))))
     
     (test-sequence))
  15)  ; 1+2+3+4+5 = 15

;; ============================================================================
;; RÉSULTATS
;; ============================================================================
(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   RÉSULTATS                                                    ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")
(format t "Tests réussis: ~A~%" *tests-passed*)
(format t "Tests échoués: ~A~%" *tests-failed*)
(format t "Total: ~A~%" (+ *tests-passed* *tests-failed*))
(format t "Taux de réussite: ~,1F%~%" 
        (* 100.0 (/ *tests-passed* (max 1 (+ *tests-passed* *tests-failed*)))))
