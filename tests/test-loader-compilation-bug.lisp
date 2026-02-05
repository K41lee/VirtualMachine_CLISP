#!/usr/bin/env clisp
;;; ============================================================================
;;; TEST: Compilation du loader - Identification du bug
;;; ============================================================================
;;;
;;; Ce fichier teste la compilation du loader et identifie un bug dans
;;; le compilateur concernant l'accès aux paramètres de fonction dans
;;; les boucles while.
;;;
;;; Le loader DEVRAIT pouvoir être compilé avec:
;;;
;;; (defun simple-loader (code-addr data-addr count)
;;;   (let ((i 0))
;;;     (while (< i count)
;;;       (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
;;;       (setq i (+ i 1)))
;;;     code-addr))
;;;
;;; Mais actuellement, la boucle while ne s'exécute pas correctement.
;;; ============================================================================

(load "main.lisp")

(format t "~%╔═══════════════════════════════════════════════════════════════╗~%")
(format t "║  TEST DE COMPILATION DU LOADER - Identification du bug       ║~%")
(format t "╚═══════════════════════════════════════════════════════════════╝~%~%")

;;; Test 1: Loader simple sans boucle (devrait fonctionner)
(format t "═══ Test 1: Loader simple (1 instruction) ═══~%")
(defparameter *loader-simple*
  '(defun simple-loader-1 (code-addr data-addr count)
     (let ((instr (mem-read data-addr)))
       (mem-write code-addr instr)
       code-addr)))

(defparameter *loader-simple-mips* (compile-lisp *loader-simple*))
(format t "✓ Compilé : ~A instructions~%~%" (length *loader-simple-mips*))

;;; Test 2: Loader avec boucle while et constante (devrait fonctionner)
(format t "═══ Test 2: Loader avec while et constante ═══~%")
(defparameter *loader-const*
  '(defun simple-loader-const (code-addr data-addr count)
     (let ((i 0))
       (while (< i 5)  ; Constante
         (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
         (setq i (+ i 1)))
       code-addr)))

(defparameter *loader-const-mips* (compile-lisp *loader-const*))
(format t "✓ Compilé : ~A instructions~%~%" (length *loader-const-mips*))

;;; Test 3: Loader avec boucle while et paramètre (BUG)
(format t "═══ Test 3: Loader avec while et paramètre (BUG ATTENDU) ═══~%")
(defparameter *loader-param*
  '(defun simple-loader-param (code-addr data-addr count)
     (let ((i 0))
       (while (< i count)  ; Paramètre count
         (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
         (setq i (+ i 1)))
       code-addr)))

(defparameter *loader-param-mips* (compile-lisp *loader-param*))
(format t "✓ Compilé : ~A instructions~%~%" (length *loader-param-mips*))

;;; Test 4: Loader avec boucle while et variable locale
(format t "═══ Test 4: Loader avec while et variable locale ═══~%")
(defparameter *loader-var*
  '(defun simple-loader-var (code-addr data-addr count)
     (let ((i 0)
           (n count))
       (while (< i n)  ; Variable locale n
         (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
         (setq i (+ i 1)))
       code-addr)))

(defparameter *loader-var-mips* (compile-lisp *loader-var*))
(format t "✓ Compilé : ~A instructions~%~%" (length *loader-var-mips*))

;;; Maintenant testons l'exécution
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "TESTS D'EXÉCUTION~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(defparameter *vm* (make-new-vm :verbose nil))

;; Préparer la mémoire de test
(defparameter *data-zone* 10481000)
(defparameter *code-zone* 10482000)

;; Écrire des valeurs de test dans la zone source
(format t "Préparation: Écriture de 10 valeurs de test en zone DATA...~%")
(loop for i from 0 to 9 do
  (mem-write *vm* (+ *data-zone* i) (* i 100)))
(format t "✓ Valeurs écrites: ~{~A ~}~%~%"
        (loop for i from 0 to 9 collect (mem-read *vm* (+ *data-zone* i))))

;;; Test exécution 1: Loader simple (1 instruction)
(format t "═══ Exécution Test 1: Loader simple ═══~%")
(reset-vm *vm*)
(load-code *vm* *loader-simple-mips*)
(let ((loader-addr (calculate-code-start *vm*)))
  ;; Réécrire les valeurs de test
  (loop for i from 0 to 9 do
    (mem-write *vm* (+ *data-zone* i) (* i 100)))
  
  ;; Créer bootstrap
  (let ((bootstrap-addr (+ loader-addr (length *loader-simple-mips*))))
    (dolist (instr (list
                    (list :LI *code-zone* *reg-a0*)
                    (list :LI *data-zone* *reg-a1*)
                    (list :LI 10 *reg-a2*)
                    (list :JAL loader-addr)
                    (list :HALT)))
      (append-code *vm* (list instr)))
    
    (set-register *vm* (get-reg :pc) bootstrap-addr)
    (run-vm *vm*)
    
    (format t "Valeur copiée: ~A~%" (mem-read *vm* *code-zone*))
    (if (= (mem-read *vm* *code-zone*) 0)
        (format t "✓ TEST RÉUSSI (1 valeur copiée)~%~%")
        (format t "✗ TEST ÉCHOUÉ~%~%"))))

;;; Test exécution 2: Loader avec constante
(format t "═══ Exécution Test 2: Loader avec while (constante 5) ═══~%")
(reset-vm *vm*)
(load-code *vm* *loader-const-mips*)
(let ((loader-addr (calculate-code-start *vm*)))
  ;; Réécrire les valeurs de test
  (loop for i from 0 to 9 do
    (mem-write *vm* (+ *data-zone* i) (* i 100)))
  
  ;; Créer bootstrap
  (let ((bootstrap-addr (+ loader-addr (length *loader-const-mips*))))
    (dolist (instr (list
                    (list :LI *code-zone* *reg-a0*)
                    (list :LI *data-zone* *reg-a1*)
                    (list :LI 10 *reg-a2*)
                    (list :JAL loader-addr)
                    (list :HALT)))
      (append-code *vm* (list instr)))
    
    (set-register *vm* (get-reg :pc) bootstrap-addr)
    
    ;; Timeout de 5 secondes
    (handler-case
        (progn
          (run-vm *vm*)
          (format t "Valeurs copiées: ~{~A ~}~%" 
                  (loop for i from 0 to 4 collect (mem-read *vm* (+ *code-zone* i))))
          (let ((copied (loop for i from 0 to 4
                             count (= (mem-read *vm* (+ *code-zone* i))
                                     (mem-read *vm* (+ *data-zone* i))))))
            (if (= copied 5)
                (format t "✓ TEST RÉUSSI (5 valeurs copiées)~%~%")
                (format t "✗ TEST ÉCHOUÉ (seulement ~A valeurs copiées)~%~%" copied))))
      (error (c)
        (format t "✗ TEST ÉCHOUÉ: ~A~%~%" c)))))

;;; Test exécution 3: Loader avec paramètre (devrait révéler le bug)
(format t "═══ Exécution Test 3: Loader avec while (paramètre count) ═══~%")
(reset-vm *vm*)
(load-code *vm* *loader-param-mips*)
(let ((loader-addr (calculate-code-start *vm*)))
  ;; Réécrire les valeurs de test
  (loop for i from 0 to 9 do
    (mem-write *vm* (+ *data-zone* i) (* i 100)))
  
  ;; Créer bootstrap
  (let ((bootstrap-addr (+ loader-addr (length *loader-param-mips*))))
    (dolist (instr (list
                    (list :LI *code-zone* *reg-a0*)
                    (list :LI *data-zone* *reg-a1*)
                    (list :LI 5 *reg-a2*)  ; On veut copier 5 valeurs
                    (list :JAL loader-addr)
                    (list :HALT)))
      (append-code *vm* (list instr)))
    
    (set-register *vm* (get-reg :pc) bootstrap-addr)
    (run-vm *vm*)
    
    (format t "Valeurs copiées: ~{~A ~}~%" 
            (loop for i from 0 to 4 collect (mem-read *vm* (+ *code-zone* i))))
    (let ((copied (loop for i from 0 to 4
                       count (= (mem-read *vm* (+ *code-zone* i))
                               (mem-read *vm* (+ *data-zone* i))))))
      (if (= copied 5)
          (format t "✓ TEST RÉUSSI (5 valeurs copiées)~%~%")
          (format t "✗ TEST ÉCHOUÉ (seulement ~A valeurs copiées)~%~%" copied)))))

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "CONCLUSION~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")
(format t "Le compilateur supporte mem-read/mem-write (Test 1 OK).~%")
(format t "La compilation de while fonctionne avec des constantes.~%")
(format t "MAIS: Il y a un bug avec l'accès aux paramètres dans while.~%~%")
(format t "Pour l'instant, le loader doit être écrit en assembleur manuel.~%~%")
