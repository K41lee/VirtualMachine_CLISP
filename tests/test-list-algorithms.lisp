;;;; Test complet avec QUOTE, CONS, CAR, CDR
;;;;

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TEST COMPLET - Listes avec délégation à Lisp              ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-list-algo (name code input expected)
  "Teste un algorithme sur listes"
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let* ((start-time (get-internal-real-time))
             (compiled (compile-lisp-to-mips-simplified code))
             (vm (make-new-vm)))
        (if (null compiled)
            (progn
              (format t "  ❌ ÉCHEC COMPILATION~%")
              (incf *tests-failed*))
            (progn
              (load-code vm compiled)
              (set-register vm :$A0 input)
              (run-vm vm)
              (let* ((result (get-register vm :$V0))
                     (instr (vm-instruction-count vm))
                     (end-time (get-internal-real-time))
                     (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
                (if (= result expected)
                    (progn
                      (format t "  ✅ = ~A [~,3F sec, ~:D instr]~%" result elapsed instr)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ = ~A (attendu ~A) [~:D instr]~%" result expected instr)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

;;; Test 1: Longueur d'une liste (avec liste encodée comme handle)
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " ALGORITHME 1: LONGUEUR D'UNE LISTE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-list-algo
 "length(NIL)"
 '(defun my-length (lst)
    (if (null lst)
        0
        (+ 1 (my-length (cdr lst)))))
 0  ; NIL est représenté par handle 0
 0)

;;; Test 2: Fonction qui construit une liste avec CONS
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " ALGORITHME 2: CONSTRUCTION DE LISTE AVEC CONS~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(format t "~%Test: make-range(5)~%")
(handler-case
    (let* ((code '(defun make-range (n)
                    (if (= n 0)
                        nil
                        (cons n (make-range (- n 1))))))
           (compiled (compile-lisp-to-mips-simplified code))
           (vm (make-new-vm)))
      (load-code vm compiled)
      (set-register vm :$A0 5)
      (run-vm vm)
      (let ((result (get-register vm :$V0)))
        (if (> result 0)
            (progn
              (format t "  ✅ Retourne handle ~A (liste créée)~%" result)
              (incf *tests-passed*))
            (progn
              (format t "  ❌ Retourne ~A (devrait être un handle >0)~%" result)
              (incf *tests-failed*)))))
  (error (e)
    (format t "  ❌ ERREUR: ~A~%" e)
    (incf *tests-failed*)))

;;; Test 3: Test NULL avec différentes valeurs
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " ALGORITHME 3: PRÉDICAT NULL~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-list-algo
 "is-null(NIL)"
 '(defun is-null (x)
    (if (null x) 1 0))
 0  ; NIL
 1) ; Devrait retourner vrai

(test-list-algo
 "is-null(42)"
 '(defun is-null (x)
    (if (null x) 1 0))
 42
 0) ; Devrait retourner faux

;;; Test 4: Fonction récursive simple
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t " ALGORITHME 4: COMPTEUR RÉCURSIF~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-list-algo
 "count-down(10)"
 '(defun count-down (n)
    (if (= n 0)
        0
        (+ 1 (count-down (- n 1)))))
 10
 10)

;;; Résumé
(format t "~%~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║                 RÉSUMÉ DES TESTS                             ║~%")
(format t "╠════════════════════════════════════════════════════════════════╣~%")
(format t "║  Tests réussis:  ~3D                                          ║~%" *tests-passed*)
(format t "║  Tests échoués:  ~3D                                          ║~%" *tests-failed*)
(format t "║  Total:          ~3D                                          ║~%" (+ *tests-passed* *tests-failed*))
(format t "║  Taux de succès: ~5,1F%                                      ║~%" 
        (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*))))
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(if (= *tests-failed* 0)
    (format t "~%✅ TOUS LES TESTS SONT PASSÉS ! ✅~%~%")
    (format t "~%⚠️  ~A test(s) échoué(s)~%~%" *tests-failed*))

(quit)
