#!/usr/bin/env clisp
;;;; ============================================================================
;;;; TEST FINAL: Comparaison FIBO Natif vs compile-from-handle
;;;; ============================================================================

(load "src/asm-ops.lisp")
(load "src/vm.lisp")
(load "src/loader.lisp")

;; Initialiser les symboles
(initialize-compiler-symbols)

(load "src/compiler-simplified.lisp")
(load "utils-bootstrap.lisp")

(defun reconstruct-symbols (code)
  "Convertit les IDs numériques en symboles keywords"
  (cond
    ((null code) nil)
    ((numberp code)
     (let ((sym-name (symbol-name-from-id code)))
       (if (and sym-name
                (or (char= (char sym-name 0) #\$)
                    (find sym-name '("LI" "LW" "SW" "ADD" "SUB" "ADDI" "LIST" "MOVE" 
                                     "JAL" "J" "JR" "HALT" "GLOBAL-GET" "GLOBAL-SET"
                                     "BEQ" "BNE" "BLT" "BGT" "LABEL" "MUL" "DIV")
                          :test #'string=)))
           (intern sym-name :keyword)
           code)))
    ((listp code)
     (mapcar #'reconstruct-symbols code))
    (t code)))

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║       BOOTSTRAP FINAL: FIBO compilé natif vs handle            ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defparameter *fibo-def*
  '(defun fibo (n)
     (if (< n 2)
         n
         (+ (fibo (- n 1)) (fibo (- n 2))))))

(defparameter *ack-def*
  '(defun ack (m n)
     (cond
       ((= m 0) (+ n 1))
       ((= n 0) (ack (- m 1) 1))
       (t (ack (- m 1) (ack m (- n 1)))))))

;;; ============================================================================
;;; Test avec FIBO
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 1: FIBO~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Compilation NATIVE...~%")
(defparameter *fibo-native* (compile-lisp-to-mips-simplified *fibo-def*))
(format t "✓ ~A instructions~%" (length *fibo-native*))

(format t "~%Compilation via compile-from-handle...~%")
(defparameter *fibo-handle* (build-expression-in-vm *fibo-def*))
(format t "  Handle: ~A~%" *fibo-handle*)
(defparameter *fibo-from-handle* (compile-from-handle *fibo-handle*))
(format t "✓ ~A instructions~%" (length *fibo-from-handle*))

(if (equal *fibo-native* *fibo-from-handle*)
    (format t "~%✅ FIBO: IDENTIQUE!~%")
    (format t "~%❌ FIBO: DIFFÉRENT~%"))

;;; ============================================================================
;;; Test avec ACK
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST 2: ACK~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Compilation NATIVE...~%")
(defparameter *ack-native* (compile-lisp-to-mips-simplified *ack-def*))
(format t "✓ ~A instructions~%" (length *ack-native*))

(format t "~%Compilation via compile-from-handle...~%")
(defparameter *ack-handle* (build-expression-in-vm *ack-def*))
(format t "  Handle: ~A~%" *ack-handle*)
(defparameter *ack-from-handle* (compile-from-handle *ack-handle*))
(format t "✓ ~A instructions~%" (length *ack-from-handle*))

(if (equal *ack-native* *ack-from-handle*)
    (format t "~%✅ ACK: IDENTIQUE!~%")
    (format t "~%❌ ACK: DIFFÉRENT~%"))

;;; ============================================================================
;;; Affichage du code FIBO
;;; ============================================================================

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "CODE FIBO (premiers 20 instructions):~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%NATIF (brut):~%")
(let ((sample (subseq *fibo-native* 0 (min 20 (length *fibo-native*)))))
  (dolist (instr sample)
    (format t "  ~A~%" instr)))

(format t "~%VIA HANDLE (brut):~%")
(let ((sample (subseq *fibo-from-handle* 0 (min 20 (length *fibo-from-handle*)))))
  (dolist (instr sample)
    (format t "  ~A~%" instr)))

(format t "~%NATIF (reconstruit):~%")
(let ((sample (subseq (reconstruct-symbols *fibo-native*) 0 (min 20 (length *fibo-native*)))))
  (dolist (instr sample)
    (if (and (listp instr) (eq (first instr) :LABEL))
        (format t "~%~A:~%" (second instr))
        (format t "  ~A~%" instr))))

;;; ============================================================================
;;; RÉSUMÉ
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                        RÉSUMÉ FINAL                            ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(let ((total-tests 2)
      (passed-tests (+ (if (equal *fibo-native* *fibo-from-handle*) 1 0)
                       (if (equal *ack-native* *ack-from-handle*) 1 0))))
  
  (format t "~%Tests réussis: ~A/~A~%" passed-tests total-tests)
  
  (if (= passed-tests total-tests)
      (progn
        (format t "~%✅ SUCCESS COMPLET!~%")
        (format t "~%compile-from-handle génère EXACTEMENT le même code que~%")
        (format t "le compilateur natif.~%")
        (format t "~%CONCLUSION:~%")
        (format t "Les 3 points du plan d'action sont COMPLÈTEMENT implémentés:~%")
        (format t "  1. ✅ Système d'interning de symboles~%")
        (format t "  2. ✅ Représentation des expressions en mémoire~%")
        (format t "  3. ✅ Compilation depuis handles (compile-from-handle)~%")
        (format t "~%Le bootstrap est maintenant FONCTIONNEL en mode natif.~%")
        (format t "Pour compiler dans la VM, il faudrait résoudre les appels~%")
        (format t "de fonctions (compile-lisp-to-mips-simplified, etc.).~%"))
      (format t "~%⚠ Certains tests ont échoué.~%")))

;;; ============================================================================
;;; ÉTAPE BONUS: Exécution des fonctions compilées
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                    ÉTAPE BONUS: EXÉCUTION                      ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(format t "~%Création d'une VM et chargement du code FIBO...~%")
(defparameter *vm-fibo* (make-new-vm :verbose nil))
(load-code *vm-fibo* *fibo-from-handle*)

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST: fibo(20) - Calcul du 20ème nombre de Fibonacci~%")
(format t "════════════════════════════════════════════════════════════════~%")

;; Trouver l'adresse de FIBO
(defparameter *fibo-addr* nil)
(let ((code-start (calculate-code-start *vm-fibo*))
      (addr 0))
  (dolist (instr *fibo-from-handle*)
    (when (and (listp instr)
               (eq (first instr) :LABEL)
               (let ((label-str (if (symbolp (second instr))
                                   (symbol-name (second instr))
                                   (second instr))))
                 (string= label-str "FIBO")))
      (setf *fibo-addr* (+ code-start addr))
      (return))
    (incf addr)))

(if *fibo-addr*
    (progn
      (format t "~%Fonction FIBO à l'adresse: ~A~%" *fibo-addr*)
      
      ;; Préparer l'appel: fibo(20)
      (set-register *vm-fibo* (get-reg :a0) 20)  ; Argument n = 20
      (set-register *vm-fibo* (get-reg :pc) *fibo-addr*)
      (set-register *vm-fibo* (get-reg :ra) 0)   ; Adresse de retour = 0 (HALT)
      
      (format t "Calcul de fibo(20)...~%")
      (run-vm *vm-fibo*)
      
      (defparameter *result-fibo* (get-register *vm-fibo* (get-reg :v0)))
      (format t "~%Résultat: fibo(20) = ~A~%" *result-fibo*)
      (format t "Attendu:  fibo(20) = 6765~%")
      
      (if (= *result-fibo* 6765)
          (format t "~%✅ SUCCESS! fibo(20) est correct!~%")
          (format t "~%❌ ERREUR: Résultat incorrect!~%")))
    (format t "~%✗ Impossible de trouver la fonction FIBO~%"))

(format t "~%════════════════════════════════════════════════════════════════~%")
(format t "TEST: ack(3,3) - Fonction d'Ackermann~%")
(format t "════════════════════════════════════════════════════════════════~%")

(format t "~%Création d'une VM et chargement du code ACK...~%")
(defparameter *vm-ack* (make-new-vm :verbose nil))
(load-code *vm-ack* *ack-from-handle*)

;; Trouver l'adresse de ACK
(defparameter *ack-addr* nil)
(let ((code-start (calculate-code-start *vm-ack*))
      (addr 0))
  (dolist (instr *ack-from-handle*)
    (when (and (listp instr)
               (eq (first instr) :LABEL)
               (let ((label-str (if (symbolp (second instr))
                                   (symbol-name (second instr))
                                   (second instr))))
                 (string= label-str "ACK")))
      (setf *ack-addr* (+ code-start addr))
      (return))
    (incf addr)))

(if *ack-addr*
    (progn
      (format t "~%Fonction ACK à l'adresse: ~A~%" *ack-addr*)
      
      ;; Préparer l'appel: ack(3, 3)
      (set-register *vm-ack* (get-reg :a0) 3)   ; Argument m = 3
      (set-register *vm-ack* (get-reg :a1) 3)   ; Argument n = 3
      (set-register *vm-ack* (get-reg :pc) *ack-addr*)
      (set-register *vm-ack* (get-reg :ra) 0)   ; Adresse de retour = 0 (HALT)
      
      (format t "Calcul de ack(3,3)...~%")
      (run-vm *vm-ack*)
      
      (defparameter *result-ack* (get-register *vm-ack* (get-reg :v0)))
      (format t "~%Résultat: ack(3,3) = ~A~%" *result-ack*)
      (format t "Attendu:  ack(3,3) = 61~%")
      
      (if (= *result-ack* 61)
          (format t "~%✅ SUCCESS! ack(3,3) est correct!~%")
          (format t "~%❌ ERREUR: Résultat incorrect!~%")))
    (format t "~%✗ Impossible de trouver la fonction ACK~%"))

(format t "~%════════════════════════════════════════════════════════════════~%")
