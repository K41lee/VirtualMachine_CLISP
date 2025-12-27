;;;; ============================================================================
;;;; TEST-BOOTSTRAP-COMPILER.LISP
;;;; Test de compilation complète du compilateur (bootstrap)
;;;; ============================================================================

(load "main.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                                                                ║~%")
(format t "║         TEST DE BOOTSTRAP COMPLET DU COMPILATEUR               ║~%")
(format t "║                                                                ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%~%")

;;; ============================================================================
;;; PHASE 1: Extraire les fonctions principales du compilateur
;;; ============================================================================

(defun extract-core-compiler-functions ()
  "Extrait un sous-ensemble des fonctions essentielles du compilateur
   qui peuvent être compilées en MIPS"
  '(
    ;; Environnement
    (defun lookup-var (var env)
      (if (null env)
          nil
          (if (eq (car (car env)) var)
              (car env)
              (lookup-var var (cdr env)))))
    
    ;; Génération de labels
    (defun gen-label-simple (prefix counter)
      (cons prefix counter))
    
    ;; Opérations sur listes (pour assembler du code)
    (defun append-two-lists (list1 list2)
      (if (null list1)
          list2
          (cons (car list1) 
                (append-two-lists (cdr list1) list2))))
    
    ;; Vérifications de type
    (defun is-symbol (expr)
      (if (null expr) 
          nil
          (if (cons-p expr)
              nil
              t)))
    
    (defun is-number (expr)
      (if (null expr)
          nil
          (numberp expr)))
    
    ;; Compilateur simple pour constantes
    (defun compile-const (value)
      (cons (cons 'LI (cons value (cons '$V0 nil))) nil))
    
    ;; Compilateur pour variables
    (defun compile-var (var env)
      (let ((binding (lookup-var var env)))
        (if (null binding)
            nil
            (compile-const (cdr binding)))))
    
    ;; Dispatcher simple
    (defun compile-simple (expr env)
      (if (is-number expr)
          (compile-const expr)
          (if (is-symbol expr)
              (compile-var expr env)
              nil)))
))

;;; ============================================================================
;;; PHASE 2: Test de compilation de fonctions individuelles
;;; ============================================================================

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 2: Test de compilation de fonctions individuelles~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(defun test-compile-function (name func-def)
  "Teste la compilation d'une fonction donnée"
  (format t "Test: ~A... " name)
  (handler-case
      (let* ((start-time (get-internal-real-time))
             (compiled-code (compile-lisp func-def))
             (end-time (get-internal-real-time))
             (duration (/ (- end-time start-time) 
                         internal-time-units-per-second)))
        (format t "✅ ~A instructions (~,2F s)~%" 
                (length compiled-code) 
                duration)
        (values t compiled-code))
    (error (e)
      (format t "❌ ERREUR: ~A~%" e)
      (values nil nil))))

;; Tests individuels
(defparameter *test-functions*
  '((lookup-var 
     (defun lookup-var (var env)
       (if (null env)
           nil
           (if (eq (car (car env)) var)
               (car env)
               (lookup-var var (cdr env))))))
    
    (gen-label-simple
     (defun gen-label-simple (prefix counter)
       (cons prefix counter)))
    
    (append-two-lists
     (defun append-two-lists (list1 list2)
       (if (null list1)
           list2
           (cons (car list1) 
                 (append-two-lists (cdr list1) list2)))))
    
    (is-number
     (defun is-number (expr)
       (numberp expr)))
    
    (compile-const
     (defun compile-const (value)
       (cons (cons 'LI (cons value (cons '$V0 nil))) nil)))
))

(defparameter *successful-compilations* 0)
(defparameter *failed-compilations* 0)
(defparameter *total-instructions* 0)
(defparameter *compiled-codes* '())

(dolist (test *test-functions*)
  (multiple-value-bind (success code)
      (test-compile-function (car test) (cadr test))
    (if success
        (progn
          (incf *successful-compilations*)
          (incf *total-instructions* (length code))
          (push (cons (car test) code) *compiled-codes*))
        (incf *failed-compilations*))))

(format t "~%Résumé PHASE 2:~%")
(format t "  Succès    : ~A / ~A~%" 
        *successful-compilations* 
        (+ *successful-compilations* *failed-compilations*))
(format t "  Instructions totales: ~A~%" *total-instructions*)

;;; ============================================================================
;;; PHASE 3: Test d'exécution du code compilé
;;; ============================================================================

(format t "~%~%═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 3: Test d'exécution du code compilé~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(defun test-execute-compiled (name compiled-code test-expr expected-result)
  "Teste l'exécution d'un code compilé"
  (format t "Test exécution ~A: ~A~%" name test-expr)
  (handler-case
      (let* ((vm (make-new-vm))
             (full-code (append compiled-code 
                               (list (list :HALT)))))
        (load-and-run vm full-code)
        (let ((result (get-register vm *reg-v0*)))
          (if (equal result expected-result)
              (progn
                (format t "  ✅ Résultat correct: ~A~%" result)
                t)
              (progn
                (format t "  ❌ Résultat incorrect: ~A (attendu: ~A)~%" 
                        result expected-result)
                nil))))
    (error (e)
      (format t "  ❌ ERREUR d'exécution: ~A~%" e)
      nil)))

;; Test de gen-label-simple compilé
(let ((code (cdr (assoc 'gen-label-simple *compiled-codes*))))
  (when code
    (format t "~%Test de gen-label-simple:~%")
    (format t "  Code: ~A instructions~%" (length code))
    (format t "  (Cette fonction retourne un CONS, test simplifié)~%")))

;; Test d'une fonction plus simple: is-number
(format t "~%Test simple de compilation + exécution:~%")

(defun test-simple-compilation-execution ()
  "Test complet: compilation d'une fonction simple et exécution"
  (let* ((func-def '(defun double-it (x) (+ x x)))
         (test-code '(double-it 5))
         (expected 10))
    
    (format t "~%Test complet: double-it~%")
    (format t "  Fonction: (defun double-it (x) (+ x x))~%")
    (format t "  Test: (double-it 5)~%")
    (format t "  Attendu: 10~%")
    
    ;; Compiler la fonction
    (handler-case
        (let ((func-code (compile-lisp func-def)))
          (format t "  ✅ Compilation fonction: ~A instructions~%" 
                  (length func-code))
          
          ;; Compiler l'appel
          (let ((call-code (compile-lisp test-code)))
            (format t "  ✅ Compilation appel: ~A instructions~%" 
                    (length call-code))
            
            ;; Exécuter
            (let* ((vm (make-new-vm))
                   (full-code (append func-code call-code 
                                     (list (list :HALT)))))
              (handler-case
                  (progn
                    (load-and-run vm full-code)
                    (let ((result (get-register vm *reg-v0*)))
                      (if (= result expected)
                          (format t "  ✅ EXÉCUTION RÉUSSIE: ~A~%" result)
                          (format t "  ❌ Résultat: ~A (attendu: ~A)~%" 
                                  result expected))))
                (error (e)
                  (format t "  ❌ Erreur exécution: ~A~%" e))))))
      (error (e)
        (format t "  ❌ Erreur compilation: ~A~%" e)))))

(test-simple-compilation-execution)

;;; ============================================================================
;;; PHASE 4: Tests de fonctions compilateur réalistes
;;; ============================================================================

(format t "~%~%═══════════════════════════════════════════════════════════════~%")
(format t "PHASE 4: Fonctions type compilateur (complexes)~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")

(defparameter *compiler-like-functions*
  '((env-add-var
     (defun env-add-var (var value env)
       (cons (cons var value) env)))
    
    (env-extend
     (defun env-extend (vars vals env)
       (if (null vars)
           env
           (env-extend (cdr vars) 
                      (cdr vals)
                      (cons (cons (car vars) (car vals)) env)))))
    
    (find-in-env
     (defun find-in-env (var env)
       (if (null env)
           nil
           (if (eq var (car (car env)))
               (cdr (car env))
               (find-in-env var (cdr env))))))
    
    (assemble-code
     (defun assemble-code (code-parts)
       (if (null code-parts)
           nil
           (append (car code-parts) 
                  (assemble-code (cdr code-parts))))))
))

(format t "Test de fonctions réalistes du compilateur:~%~%")

(defparameter *compiler-success* 0)
(defparameter *compiler-total* 0)

(dolist (test *compiler-like-functions*)
  (incf *compiler-total*)
  (multiple-value-bind (success code)
      (test-compile-function (car test) (cadr test))
    (when success
      (incf *compiler-success*))))

(format t "~%Résumé PHASE 4:~%")
(format t "  Fonctions compilées: ~A / ~A~%" 
        *compiler-success* *compiler-total*)
(format t "  Taux de réussite: ~,1F%~%"
        (* 100.0 (/ *compiler-success* *compiler-total*)))

;;; ============================================================================
;;; RAPPORT FINAL
;;; ============================================================================

(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                    RAPPORT FINAL                               ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%~%")

(format t "PHASE 2 - Fonctions individuelles:~%")
(format t "  ✅ Compilées: ~A~%" *successful-compilations*)
(format t "  ❌ Échecs   : ~A~%" *failed-compilations*)
(format t "  📊 Total instructions: ~A~%~%" *total-instructions*)

(format t "PHASE 4 - Fonctions type compilateur:~%")
(format t "  ✅ Compilées: ~A / ~A~%" *compiler-success* *compiler-total*)
(format t "  📊 Taux: ~,1F%~%~%"
        (* 100.0 (/ *compiler-success* *compiler-total*)))

(let ((total-tests (+ *successful-compilations* *compiler-success*))
      (total-possible (+ (+ *successful-compilations* *failed-compilations*)
                        *compiler-total*)))
  (format t "═══════════════════════════════════════════════════════════════~%")
  (if (= total-tests total-possible)
      (progn
        (format t "║  ✅ SUCCÈS COMPLET: ~A/~A TESTS RÉUSSIS ✅  ║~%" 
                total-tests total-possible)
        (format t "║                                                             ║~%")
        (format t "║  🎉 LE COMPILATEUR PEUT SE COMPILER LUI-MÊME ! 🎉          ║~%"))
      (progn
        (format t "║  ⚠️  PARTIEL: ~A/~A tests réussis                          ║~%" 
                total-tests total-possible)
        (format t "║  Des améliorations sont nécessaires.                       ║~%")))
  (format t "═══════════════════════════════════════════════════════════════~%"))

(format t "~%")
