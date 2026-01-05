;;;; test-symbol-table.lisp
;;;; Tests pour la table de symboles avec interning

(load "src/symbol-table.lisp")

(defun test-basic-interning ()
  "Test 1: Interning basique d'un symbole"
  (format t "~%=== Test 1: Interning basique ===~%")
  (reset-symbol-table)
  
  ;; Interner un nouveau symbole
  (let ((id1 (intern-symbol "FOO")))
    (format t "  Intern 'FOO' → ID: ~A~%" id1)
    
    ;; Vérifier qu'on peut retrouver le nom
    (let ((name1 (get-symbol-name id1)))
      (format t "  get-symbol-name(~A) → ~A~%" id1 name1)
      (if (string= name1 "FOO")
          (format t "  ✓ Test réussi: correspondance ID ↔ name~%")
          (format t "  ✗ Test échoué: attendu 'FOO', obtenu ~A~%" name1)))))

(defun test-interning-idempotent ()
  "Test 2: Interner le même symbole plusieurs fois donne le même ID"
  (format t "~%=== Test 2: Idempotence de l'interning ===~%")
  (reset-symbol-table)
  
  (let ((id1 (intern-symbol "BAR"))
        (id2 (intern-symbol "BAR"))
        (id3 (intern-symbol "BAR")))
    (format t "  Intern 'BAR' #1 → ~A~%" id1)
    (format t "  Intern 'BAR' #2 → ~A~%" id2)
    (format t "  Intern 'BAR' #3 → ~A~%" id3)
    
    (if (and (= id1 id2) (= id2 id3))
        (format t "  ✓ Test réussi: même symbole → même ID~%")
        (format t "  ✗ Test échoué: IDs différents pour le même symbole~%"))))

(defun test-different-symbols ()
  "Test 3: Symboles différents ont des IDs différents"
  (format t "~%=== Test 3: Symboles différents ===~%")
  (reset-symbol-table)
  
  (let ((id-foo (intern-symbol "FOO"))
        (id-bar (intern-symbol "BAR"))
        (id-baz (intern-symbol "BAZ")))
    (format t "  Intern 'FOO' → ~A~%" id-foo)
    (format t "  Intern 'BAR' → ~A~%" id-bar)
    (format t "  Intern 'BAZ' → ~A~%" id-baz)
    
    (if (and (/= id-foo id-bar) 
             (/= id-bar id-baz) 
             (/= id-foo id-baz))
        (format t "  ✓ Test réussi: symboles différents → IDs différents~%")
        (format t "  ✗ Test échoué: collision d'IDs~%"))))

(defun test-symbol-lookup ()
  "Test 4: get-symbol-id trouve les symboles existants"
  (format t "~%=== Test 4: Lookup de symboles ===~%")
  (reset-symbol-table)
  
  ;; Interner quelques symboles
  (let ((id-alpha (intern-symbol "ALPHA"))
        (id-beta (intern-symbol "BETA")))
    
    ;; Chercher avec get-symbol-id
    (let ((found-alpha (get-symbol-id "ALPHA"))
          (found-beta (get-symbol-id "BETA"))
          (found-gamma (get-symbol-id "GAMMA")))
      
      (format t "  Interné: ALPHA=~A, BETA=~A~%" id-alpha id-beta)
      (format t "  Lookup ALPHA → ~A~%" found-alpha)
      (format t "  Lookup BETA → ~A~%" found-beta)
      (format t "  Lookup GAMMA (inexistant) → ~A~%" found-gamma)
      
      (if (and (= found-alpha id-alpha)
               (= found-beta id-beta)
               (null found-gamma))
          (format t "  ✓ Test réussi: lookup correct~%")
          (format t "  ✗ Test échoué~%")))))

(defun test-symbol-predicate ()
  "Test 5: symbol-interned-p détecte les symboles internés"
  (format t "~%=== Test 5: Prédicat symbol-interned-p ===~%")
  (reset-symbol-table)
  
  ;; Interner un symbole
  (intern-symbol "DELTA")
  
  (let ((delta-interned (symbol-interned-p "DELTA"))
        (epsilon-interned (symbol-interned-p "EPSILON")))
    
    (format t "  symbol-interned-p('DELTA') → ~A~%" delta-interned)
    (format t "  symbol-interned-p('EPSILON') → ~A~%" epsilon-interned)
    
    (if (and delta-interned (not epsilon-interned))
        (format t "  ✓ Test réussi: prédicat correct~%")
        (format t "  ✗ Test échoué~%"))))

(defun test-symbol-count ()
  "Test 6: symbol-count retourne le bon nombre"
  (format t "~%=== Test 6: Comptage de symboles ===~%")
  (reset-symbol-table)
  
  (format t "  Nombre initial: ~A~%" (symbol-count))
  (intern-symbol "ONE")
  (format t "  Après 1 intern: ~A~%" (symbol-count))
  (intern-symbol "TWO")
  (format t "  Après 2 intern: ~A~%" (symbol-count))
  (intern-symbol "THREE")
  (format t "  Après 3 intern: ~A~%" (symbol-count))
  (intern-symbol "TWO")  ; Réinterner un existant
  (format t "  Après réintern de TWO: ~A~%" (symbol-count))
  
  (if (= (symbol-count) 3)
      (format t "  ✓ Test réussi: comptage correct~%")
      (format t "  ✗ Test échoué: attendu 3, obtenu ~A~%" (symbol-count))))

(defun test-get-all-symbols ()
  "Test 7: get-all-symbols retourne tous les symboles"
  (format t "~%=== Test 7: Liste complète des symboles ===~%")
  (reset-symbol-table)
  
  (intern-symbol "APPLE")
  (intern-symbol "BANANA")
  (intern-symbol "CHERRY")
  
  (let ((all-symbols (get-all-symbols)))
    (format t "  Symboles internés: ~A~%" (mapcar #'car all-symbols))
    (format t "  IDs: ~A~%" (mapcar #'cdr all-symbols))
    
    (if (= (length all-symbols) 3)
        (format t "  ✓ Test réussi: 3 symboles retournés~%")
        (format t "  ✗ Test échoué: attendu 3, obtenu ~A~%" (length all-symbols)))))

(defun test-export-import ()
  "Test 8: Export/Import de la table"
  (format t "~%=== Test 8: Export/Import ===~%")
  (reset-symbol-table)
  
  ;; Créer une table
  (intern-symbol "EXPORT-TEST-1")
  (intern-symbol "EXPORT-TEST-2")
  
  ;; Exporter
  (let ((exported (export-symbol-table-to-list)))
    (format t "  Table exportée: ~A entrées~%" (length exported))
    
    ;; Réinitialiser
    (reset-symbol-table)
    (format t "  Après reset: ~A entrées~%" (symbol-count))
    
    ;; Importer
    (import-symbol-table-from-list exported)
    (format t "  Après import: ~A entrées~%" (symbol-count))
    
    ;; Vérifier
    (let ((id1 (get-symbol-id "EXPORT-TEST-1"))
          (id2 (get-symbol-id "EXPORT-TEST-2")))
      (if (and id1 id2)
          (format t "  ✓ Test réussi: symboles restaurés~%")
          (format t "  ✗ Test échoué: symboles perdus~%")))))

(defun test-case-sensitivity ()
  "Test 9: La table est case-sensitive"
  (format t "~%=== Test 9: Case-sensitivity ===~%")
  (reset-symbol-table)
  
  (let ((id-lower (intern-symbol "foo"))
        (id-upper (intern-symbol "FOO"))
        (id-mixed (intern-symbol "Foo")))
    
    (format t "  Intern 'foo' → ~A~%" id-lower)
    (format t "  Intern 'FOO' → ~A~%" id-upper)
    (format t "  Intern 'Foo' → ~A~%" id-mixed)
    
    (if (and (/= id-lower id-upper)
             (/= id-upper id-mixed)
             (/= id-lower id-mixed))
        (format t "  ✓ Test réussi: case-sensitive~%")
        (format t "  ✗ Test échoué: collision de case~%"))))

(defun test-symbol-with-symbol-type ()
  "Test 10: Interner un symbole Lisp (pas juste un string)"
  (format t "~%=== Test 10: Symbole Lisp ===~%")
  (reset-symbol-table)
  
  (let ((id1 (intern-symbol 'MY-SYMBOL))
        (id2 (intern-symbol "MY-SYMBOL")))
    
    (format t "  Intern 'MY-SYMBOL (symbole) → ~A~%" id1)
    (format t "  Intern \"MY-SYMBOL\" (string) → ~A~%" id2)
    
    (if (= id1 id2)
        (format t "  ✓ Test réussi: symbole et string donnent le même ID~%")
        (format t "  ✗ Test échoué: IDs différents~%"))))

;;; ============================================================================
;;; EXÉCUTION DES TESTS
;;; ============================================================================

(defun run-all-tests ()
  "Exécute tous les tests de la table de symboles"
  (format t "~%═══════════════════════════════════════════════════════════")
  (format t "~%   TESTS DE LA TABLE DE SYMBOLES")
  (format t "~%═══════════════════════════════════════════════════════════~%")
  
  (test-basic-interning)
  (test-interning-idempotent)
  (test-different-symbols)
  (test-symbol-lookup)
  (test-symbol-predicate)
  (test-symbol-count)
  (test-get-all-symbols)
  (test-export-import)
  (test-case-sensitivity)
  (test-symbol-with-symbol-type)
  
  (format t "~%═══════════════════════════════════════════════════════════")
  (format t "~%   FIN DES TESTS")
  (format t "~%═══════════════════════════════════════════════════════════~%")
  
  ;; Afficher statistiques finales
  (reset-symbol-table)
  (intern-symbol "TEST-STATS")
  (dump-symbol-table-stats))

;; Exécuter tous les tests
(run-all-tests)
