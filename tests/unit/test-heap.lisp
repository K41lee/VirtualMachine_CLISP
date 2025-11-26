;;;; test-heap.lisp
;;;; Tests unitaires pour les instructions du tas dynamique (PHASE 9 - CLOSURES)

(load "src/vm.lisp")
(load "src/loader.lisp")

;;; ============================================================================
;;; TEST 1: MALLOC - Allocation simple
;;; ============================================================================

(defun test-malloc-simple ()
  "Test d'allocation simple avec MALLOC"
  (format t "~%=== TEST MALLOC SIMPLE ===~%")
  (let ((vm (make-new-vm :verbose t)))
    (reset-heap)
    ;; Programme: alloue 5 mots, stocke l'adresse dans $t0
    (let ((code '((:MALLOC 5 :$t0)       ; Alloue 5 mots, adresse dans $t0
                  (:PRINT :$t0)          ; Affiche l'adresse
                  (:HALT))))
      (load-code vm code)
      (format t "~%Programme chargé. Heap pointer initial: ~A~%" *heap-pointer*)
      (run-vm vm)
      ;; Vérification
      (let ((addr (get-register vm (get-reg :t0))))
        (format t "Adresse allouée: ~A~%" addr)
        (format t "Heap pointer après: ~A~%" *heap-pointer*)
        (if (and (= addr +heap-start+)
                 (= *heap-pointer* (+ +heap-start+ 5)))
            (format t "✓ TEST MALLOC SIMPLE: RÉUSSI~%")
            (format t "✗ TEST MALLOC SIMPLE: ÉCHEC~%"))))))

;;; ============================================================================
;;; TEST 2: MALLOC Multiple - Plusieurs allocations
;;; ============================================================================

(defun test-malloc-multiple ()
  "Test de plusieurs allocations successives"
  (format t "~%=== TEST MALLOC MULTIPLE ===~%")
  (let ((vm (make-new-vm :verbose t)))
    (reset-heap)
    ;; Programme: 3 allocations successives
    (let ((code '((:MALLOC 3 :$t0)       ; Première allocation (3 mots)
                  (:MALLOC 5 :$t1)       ; Deuxième allocation (5 mots)
                  (:MALLOC 2 :$t2)       ; Troisième allocation (2 mots)
                  (:PRINT :$t0)
                  (:PRINT :$t1)
                  (:PRINT :$t2)
                  (:HALT))))
      (load-code vm code)
      (run-vm vm)
      ;; Vérification
      (let ((addr1 (get-register vm (get-reg :t0)))
            (addr2 (get-register vm (get-reg :t1)))
            (addr3 (get-register vm (get-reg :t2))))
        (format t "Adresses: ~A, ~A, ~A~%" addr1 addr2 addr3)
        (format t "Heap pointer final: ~A~%" *heap-pointer*)
        (if (and (= addr1 +heap-start+)
                 (= addr2 (+ +heap-start+ 3))
                 (= addr3 (+ +heap-start+ 8))
                 (= *heap-pointer* (+ +heap-start+ 10)))
            (format t "✓ TEST MALLOC MULTIPLE: RÉUSSI~%")
            (format t "✗ TEST MALLOC MULTIPLE: ÉCHEC~%"))))))

;;; ============================================================================
;;; TEST 3: STORE-HEAP et LOAD-HEAP - Écriture et lecture
;;; ============================================================================

(defun test-store-load-heap ()
  "Test d'écriture et lecture dans le tas"
  (format t "~%=== TEST STORE/LOAD HEAP ===~%")
  (let ((vm (make-new-vm :verbose t)))
    (reset-heap)
    ;; Programme: alloue 3 mots, écrit des valeurs, les relit
    (let ((code '((:MALLOC 3 :$t0)       ; Alloue 3 mots, adresse dans $t0
                  ;; Écrire des valeurs dans le tas
                  (:LI 42 :$t1)          ; $t1 = 42
                  (:LI 0 :$t2)           ; $t2 = 0 (offset)
                  (:STORE-HEAP :$t1 :$t0 :$t2)  ; MEM[$t0 + 0] = 42
                  (:LI 100 :$t1)         ; $t1 = 100
                  (:LI 1 :$t2)           ; $t2 = 1
                  (:STORE-HEAP :$t1 :$t0 :$t2)  ; MEM[$t0 + 1] = 100
                  (:LI 255 :$t1)         ; $t1 = 255
                  (:LI 2 :$t2)           ; $t2 = 2
                  (:STORE-HEAP :$t1 :$t0 :$t2)  ; MEM[$t0 + 2] = 255
                  ;; Relire les valeurs
                  (:LI 0 :$t2)           ; offset 0
                  (:LOAD-HEAP :$t0 :$t2 :$t3)   ; $t3 = MEM[$t0 + 0]
                  (:PRINT :$t3)          ; Devrait afficher 42
                  (:LI 1 :$t2)           ; offset 1
                  (:LOAD-HEAP :$t0 :$t2 :$t4)   ; $t4 = MEM[$t0 + 1]
                  (:PRINT :$t4)          ; Devrait afficher 100
                  (:LI 2 :$t2)           ; offset 2
                  (:LOAD-HEAP :$t0 :$t2 :$t5)   ; $t5 = MEM[$t0 + 2]
                  (:PRINT :$t5)          ; Devrait afficher 255
                  (:HALT))))
      (load-code vm code)
      (run-vm vm)
      ;; Vérification
      (let ((val1 (get-register vm (get-reg :t3)))
            (val2 (get-register vm (get-reg :t4)))
            (val3 (get-register vm (get-reg :t5))))
        (format t "Valeurs lues: ~A, ~A, ~A~%" val1 val2 val3)
        (if (and (= val1 42) (= val2 100) (= val3 255))
            (format t "✓ TEST STORE/LOAD HEAP: RÉUSSI~%")
            (format t "✗ TEST STORE/LOAD HEAP: ÉCHEC~%"))))))

;;; ============================================================================
;;; TEST 4: Structure de fermeture simple
;;; ============================================================================

(defun test-closure-structure ()
  "Test de création d'une structure de fermeture simple dans le tas"
  (format t "~%=== TEST STRUCTURE FERMETURE ===~%")
  (let ((vm (make-new-vm :verbose t)))
    (reset-heap)
    ;; Programme: crée une structure [Label][Size][Var]
    ;; Fermeture simple avec 1 variable capturée
    (let ((code '((:MALLOC 3 :$t0)       ; Alloue 3 mots pour fermeture
                  ;; Structure: [Label][Size][Var0]
                  (:LI 5000 :$t1)        ; Adresse du code de la fonction
                  (:LI 0 :$t2)           ; offset 0
                  (:STORE-HEAP :$t1 :$t0 :$t2)  ; MEM[$t0 + 0] = 5000 (label)
                  (:LI 1 :$t1)           ; Taille env = 1
                  (:LI 1 :$t2)           ; offset 1
                  (:STORE-HEAP :$t1 :$t0 :$t2)  ; MEM[$t0 + 1] = 1 (size)
                  (:LI 42 :$t1)          ; Variable capturée
                  (:LI 2 :$t2)           ; offset 2
                  (:STORE-HEAP :$t1 :$t0 :$t2)  ; MEM[$t0 + 2] = 42 (var)
                  ;; Relire la structure
                  (:LI 0 :$t2)
                  (:LOAD-HEAP :$t0 :$t2 :$t3)   ; Label
                  (:PRINT :$t3)
                  (:LI 1 :$t2)
                  (:LOAD-HEAP :$t0 :$t2 :$t4)   ; Size
                  (:PRINT :$t4)
                  (:LI 2 :$t2)
                  (:LOAD-HEAP :$t0 :$t2 :$t5)   ; Var
                  (:PRINT :$t5)
                  (:HALT))))
      (load-code vm code)
      (run-vm vm)
      ;; Vérification
      (let ((label (get-register vm (get-reg :t3)))
            (size (get-register vm (get-reg :t4)))
            (var (get-register vm (get-reg :t5))))
        (format t "Structure fermeture: Label=~A, Size=~A, Var=~A~%" 
                label size var)
        (if (and (= label 5000) (= size 1) (= var 42))
            (format t "✓ TEST STRUCTURE FERMETURE: RÉUSSI~%")
            (format t "✗ TEST STRUCTURE FERMETURE: ÉCHEC~%"))))))

;;; ============================================================================
;;; LANCEUR DE TESTS
;;; ============================================================================

(defun run-heap-tests ()
  "Lance tous les tests du tas dynamique"
  (format t "~%╔════════════════════════════════════════════════╗~%")
  (format t "║  TESTS UNITAIRES - TAS DYNAMIQUE (PHASE 9)   ║~%")
  (format t "╚════════════════════════════════════════════════╝~%")
  
  (let ((tests '(test-malloc-simple
                 test-malloc-multiple
                 test-store-load-heap
                 test-closure-structure)))
    (dolist (test tests)
      (handler-case
          (funcall test)
        (error (e)
          (format t "~%✗ Erreur dans ~A: ~A~%" test e)))))
  
  (format t "~%╔════════════════════════════════════════════════╗~%")
  (format t "║          TESTS TAS TERMINÉS                    ║~%")
  (format t "╚════════════════════════════════════════════════╝~%"))

;;; Lancer les tests
(run-heap-tests)
