;;;; test-predefined-symbols.lisp
;;;; Test des symboles prédéfinis

(load "src/symbol-table.lisp")

(format t "~%═══════════════════════════════════════════════════════════")
(format t "~%   TEST DES SYMBOLES PRÉDÉFINIS")
(format t "~%═══════════════════════════════════════════════════════════~%")

(defparameter *test-count* 0)
(defparameter *test-passed* 0)

(defun test (name condition)
  "Enregistre un test et affiche le résultat"
  (incf *test-count*)
  (if condition
      (progn
        (incf *test-passed*)
        (format t "  ✓ ~A~%" name))
      (format t "  ✗ ~A~%" name)))

;; Test 1: Vérifier que les IDs sont définis
(format t "~%=== Test 1: Constantes définies ===~%")
(test "DEFUN défini" (not (null *defun-id*)))
(test "IF défini" (not (null *if-id*)))
(test "QUOTE défini" (not (null *quote-id*)))
(test "LET défini" (not (null *let-id*)))
(test "CAR défini" (not (null *car-id*)))
(test "CONS défini" (not (null *cons-id*)))

;; Test 2: Vérifier les valeurs
(format t "~%=== Test 2: Valeurs des IDs ===~%")
(format t "  DEFUN → ~A~%" *defun-id*)
(format t "  IF → ~A~%" *if-id*)
(format t "  QUOTE → ~A~%" *quote-id*)
(format t "  CAR → ~A~%" *car-id*)
(test "IDs >= 10001" (and (>= *defun-id* 10001)
                           (>= *if-id* 10001)
                           (>= *quote-id* 10001)))

;; Test 3: Lookup bidirectionnel
(format t "~%=== Test 3: Lookup bidirectionnel ===~%")
(test "DEFUN → nom" (string= (get-symbol-name *defun-id*) "DEFUN"))
(test "IF → nom" (string= (get-symbol-name *if-id*) "IF"))
(test "QUOTE → nom" (string= (get-symbol-name *quote-id*) "QUOTE"))
(test "CAR → nom" (string= (get-symbol-name *car-id*) "CAR"))

;; Test 4: Vérifier l'idempotence
(format t "~%=== Test 4: Idempotence ===~%")
(let ((new-defun-id (intern-symbol "DEFUN"))
      (new-if-id (intern-symbol "IF")))
  (test "Réinterner DEFUN" (= new-defun-id *defun-id*))
  (test "Réinterner IF" (= new-if-id *if-id*)))

;; Test 5: Compter les symboles
(format t "~%=== Test 5: Nombre de symboles ===~%")
(let ((count (symbol-count)))
  (format t "  Nombre de symboles internés: ~A~%" count)
  (test "Au moins 59 symboles" (>= count 59)))

;; Test 6: Lister quelques symboles importants
(format t "~%=== Test 6: Symboles clés importants ===~%")
(let ((important '("DEFUN" "IF" "QUOTE" "LET" "LAMBDA" 
                   "CAR" "CDR" "CONS" "LIST"
                   "+" "-" "*" "/" "=")))
  (dolist (sym-name important)
    (let ((id (get-symbol-id sym-name)))
      (if id
          (progn
            (format t "  ✓ ~A → ~A~%" sym-name id)
            (incf *test-count*)
            (incf *test-passed*))
          (progn
            (format t "  ✗ ~A → NON TROUVÉ~%" sym-name)
            (incf *test-count*))))))

;; Test 7: Vérifier que tous les symboles de la liste sont bien là
(format t "~%=== Test 7: Intégrité de *predefined-symbols* ===~%")
(format t "  Nombre dans *predefined-symbols*: ~A~%" (length *predefined-symbols*))
(test "Liste cohérente" (= (length *predefined-symbols*) (symbol-count)))

;; Test 8: Afficher quelques IDs pour référence
(format t "~%=== Test 8: Référence rapide des IDs ===~%")
(format t "  Formes spéciales:~%")
(format t "    DEFUN   = ~5A    LAMBDA  = ~5A~%" *defun-id* *lambda-id*)
(format t "    QUOTE   = ~5A    IF      = ~5A~%" *quote-id* *if-id*)
(format t "    LET     = ~5A    SETQ    = ~5A~%" *let-id* *setq-id*)
(format t "~%  Opérateurs:~%")
(format t "    +       = ~5A    -       = ~5A~%" *+-id* *--id*)
(format t "    =       = ~5A    <       = ~5A~%" *=-id* *<-id*)
(format t "~%  Listes:~%")
(format t "    CAR     = ~5A    CDR     = ~5A~%" *car-id* *cdr-id*)
(format t "    CONS    = ~5A    LIST    = ~5A~%" *cons-id* *list-id*)

;; Test 9: Vérifier qu'on peut utiliser les IDs pour comparaison
(format t "~%=== Test 9: Comparaison par IDs ===~%")
(let ((parsed-defun-id (intern-symbol "DEFUN"))
      (parsed-if-id (intern-symbol "IF")))
  (test "Comparer DEFUN par ID" (= parsed-defun-id *defun-id*))
  (test "Comparer IF par ID" (= parsed-if-id *if-id*))
  (test "DEFUN ≠ IF" (/= *defun-id* *if-id*)))

;; Test 10: Performance - Interner un nouveau symbole après les prédéfinis
(format t "~%=== Test 10: Interner de nouveaux symboles ===~%")
(let ((new-id-1 (intern-symbol "MY-CUSTOM-SYMBOL"))
      (new-id-2 (intern-symbol "ANOTHER-SYMBOL")))
  (format t "  MY-CUSTOM-SYMBOL → ~A~%" new-id-1)
  (format t "  ANOTHER-SYMBOL → ~A~%" new-id-2)
  (test "Nouveau symbole a ID > prédéfinis" (> new-id-1 (+ 10000 59)))
  (test "Nouveaux symboles distincts" (/= new-id-1 new-id-2)))

;; Résumé
(format t "~%═══════════════════════════════════════════════════════════")
(format t "~%   RÉSUMÉ")
(format t "~%═══════════════════════════════════════════════════════════")
(format t "~%  Tests exécutés: ~A~%" *test-count*)
(format t "  Tests réussis:   ~A~%" *test-passed*)
(format t "  Tests échoués:   ~A~%" (- *test-count* *test-passed*))
(format t "  Taux de réussite: ~,1F%~%" 
        (* 100.0 (/ *test-passed* *test-count*)))

(if (= *test-passed* *test-count*)
    (format t "~%  ✅ TOUS LES TESTS ONT RÉUSSI!~%")
    (format t "~%  ⚠ CERTAINS TESTS ONT ÉCHOUÉ~%"))

(format t "═══════════════════════════════════════════════════════════~%")

;; Afficher la table complète si demandé
(when nil  ; Mettre T pour afficher
  (print-symbol-table))
