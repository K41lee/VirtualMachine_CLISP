#!/usr/bin/env clisp
;;;; ============================================================================
;;;; VRAI BOOTSTRAP - EXÉCUTION RÉELLE DU COMPILATEUR DANS LA VM
;;;; Appeler compile-constant-simplified depuis la VM
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║     VRAI BOOTSTRAP - EXÉCUTION RÉELLE DU COMPILATEUR         ║~%")
(format t "║     Appeler compile-constant-simplified dans la VM           ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

;;; ============================================================================
;;; PHASE 1: Compiler compile-constant-simplified
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "PHASE 1: Compilation de compile-constant-simplified~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *fn-to-test* 
  '(defun compile-constant-simplified (value env)
     "Compile une constante"
     (list (list :LI value :$V0))))

(format t "~%Fonction à compiler:~%")
(format t "  ~A~%" *fn-to-test*)

(format t "~%Compilation en MIPS...~%")
(defparameter *compiled-fn* (compile-lisp-to-mips-simplified *fn-to-test*))
(format t "  ✓ ~A instructions générées~%" (length *compiled-fn*))

(format t "~%Premières 20 instructions:~%")
(dotimes (i (min 20 (length *compiled-fn*)))
  (format t "  [~2D] ~A~%" i (nth i *compiled-fn*)))

;;; ============================================================================
;;; PHASE 2: Charger dans la VM
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "PHASE 2: Chargement dans la VM~%")
(format t "════════════════════════════════════════════════════════════════~%")

(defparameter *vm* (make-new-vm :verbose nil))

(handler-case
    (progn
      (load-code *vm* *compiled-fn*)
      (format t "  ✓ Fonction chargée (~A instructions)~%"  (length *compiled-fn*))
      
      ;; Debug: afficher ce qui est en mémoire
      (format t "~%  Debug: Premières instructions en mémoire:~%")
      (let ((code-start (calculate-code-start *vm*)))
        (dotimes (i (min 10 (length *compiled-fn*)))
          (let ((instr (mem-read *vm* (+ code-start i))))
            (format t "    [~A] ~A~%" (+ code-start i) instr)))))
  (error (e)
    (format t "  ✗ Erreur de chargement: ~A~%" e)
    (quit)))

;;; ============================================================================
;;; PHASE 3: Trouver l'adresse de la fonction
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "PHASE 3: Localisation de la fonction~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; Le loader résout les labels et les retire.
;; La première instruction est (J COMPILE-CONSTANT-SIMPLIFIED_END)
;; La fonction commence à l'instruction suivante
(defparameter *fn-addr* (+ (calculate-code-start *vm*) 1))
(format t "  ✓ Fonction commence à l'adresse: ~A~%" *fn-addr*)
(format t "    (juste après le J initial)~%")

;;; ============================================================================
;;; PHASE 4: Préparer l'appel
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "PHASE 4: Préparation de l'appel~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%La fonction compile-constant-simplified prend 2 paramètres:~%")
(format t "  1. value: la constante à compiler (ex: 42)~%")
(format t "  2. env: l'environnement (on passera NIL)~%")

;; En MIPS, les paramètres sont passés dans $a0, $a1, ...
;; Mais compile-constant-simplified utilise 'list' qui n'est pas en MIPS

(format t "~%⚠️  PROBLÈME: compile-constant-simplified utilise 'list'~%")
(format t "    qui doit créer une liste Lisp.~%")
(format t "~%Solution: Utiliser notre instruction LIST de la VM!~%")
(format t "    Le compilateur a déjà généré le code pour créer la liste.~%")

;;; ============================================================================
;;; PHASE 5: Test avec compile-expr-simplified à la place
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "PHASE 5: Test alternatif - Compiler une constante directement~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Approche: Tester le compilateur natif vs le code qu'il générerait dans la VM~%")

;; Test 1: Compilateur natif
(format t "~%Test avec compilateur NATIF:~%")
(defparameter *test-expr* 42)
(defparameter *native-result* (compile-constant-simplified *test-expr* nil))
(format t "  compile-constant-simplified(42, nil) = ~A~%" *native-result*)

;; Test 2: Vérifier que le code compilé produit le même résultat
(format t "~%Analyse du code généré pour compile-constant-simplified:~%")
(format t "  La fonction crée une liste avec LIST~%")
(format t "  Cette liste contient: (:LI value :$V0)~%")

(format t "~%Vérification: Le code généré contient-il LIST?~%")
(let ((has-list (find-if (lambda (instr)
                           (and (listp instr)
                                (or (eq (first instr) :LIST)
                                    (eq (first instr) 'LIST))))
                         *compiled-fn*)))
  (if has-list
      (format t "  ✓ Oui, instruction LIST trouvée: ~A~%" has-list)
      (format t "  ✗ Non, pas d'instruction LIST~%")))

;;; ============================================================================
;;; PHASE 6: Créer un wrapper pour tester l'exécution
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "PHASE 6: Création d'un wrapper d'exécution~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Pour exécuter compile-constant-simplified dans la VM, il faut:~%")
(format t "  1. ✓ Charger le code compilé (FAIT)~%")
(format t "  2. ✓ Localiser la fonction (FAIT)~%")
(format t "  3. ⚠️  Passer les paramètres selon la convention d'appel MIPS~%")
(format t "  4. ⚠️  Appeler la fonction avec JAL~%")
(format t "  5. ⚠️  Récupérer le résultat dans $V0~%")

(format t "~%Créons un code wrapper qui appelle la fonction:~%")

(defparameter *wrapper-code*
  `(;; Setup des paramètres
    (:LI 42 :$A0)              ; value = 42
    (:LI 0 :$A1)               ; env = 0 (NIL représenté par 0)
    ;; Appeler la fonction
    (:JAL ,*fn-addr*)          ; Appel de compile-constant-simplified
    ;; Résultat dans $V0 (handle de la liste)
    (:HALT)))

(format t "~%Code wrapper:~%")
(dolist (instr *wrapper-code*)
  (format t "  ~A~%" instr))

(format t "~%Chargement et exécution du wrapper...~%")

(defparameter *wrapper-vm* (make-new-vm :verbose t))

(handler-case
    (progn
      ;; D'abord charger la fonction compilée
      (load-code *wrapper-vm* *compiled-fn*)
      (format t "~%  ✓ Fonction chargée~%")
      
      ;; Puis charger le wrapper à la fin
      (let ((wrapper-start (+ (calculate-code-start *wrapper-vm*) 
                             (length *compiled-fn*))))
        (format t "  Wrapper commence à: ~A~%" wrapper-start)
        
        ;; Charger les instructions du wrapper
        (dotimes (i (length *wrapper-code*))
          (mem-write *wrapper-vm* (+ wrapper-start i) (nth i *wrapper-code*)))
        
        ;; Positionner PC au début du wrapper
        (set-register *wrapper-vm* (get-reg :pc) wrapper-start)
        
        (format t "~%  Exécution du wrapper...~%~%")
        (run-vm *wrapper-vm*)
        
        (format t "~%  ✓ Exécution terminée~%")
        (let ((result-handle (get-value *wrapper-vm* :$v0)))
          (format t "  Résultat dans $V0: ~A~%" result-handle)
          (let ((result-obj (gethash result-handle *vm-lisp-objects*)))
            (format t "  Objet Lisp: ~A~%" result-obj)
            
            (format t "~%Comparaison avec le compilateur natif:~%")
            (format t "  Natif:  ~A~%" *native-result*)
            (format t "  VM:     ~A~%" result-obj)
            
            (if (equal *native-result* result-obj)
                (format t "~%  🎉 RÉSULTATS IDENTIQUES! Bootstrap réussi! 🎉~%")
                (format t "~%  ⚠️  Résultats différents~%"))))))
  (error (e)
    (format t "~%  ✗ Erreur: ~A~%" e)))

(format t "~%════════════════════════════════════════════════════════════════~%")
