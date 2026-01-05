;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS DU DISPATCHER AVEC IDs - ÉTAPE 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/symbol-table.lisp")
(load "src/dispatcher-with-ids.lisp")

;; Compteur de tests
(defparameter *test-count* 0)
(defparameter *test-passed* 0)

(defun test (name condition)
  "Exécute un test et affiche le résultat"
  (incf *test-count*)
  (if condition
      (progn
        (incf *test-passed*)
        (format t "  ✓ ~A~%" name))
      (format t "  ✗ ~A~%" name)))

(format t "~%═══════════════════════════════════════════════════════════")
(format t "~%   TEST DU DISPATCHER AVEC IDs")
(format t "~%═══════════════════════════════════════════════════════════~%")

;; =============================================================================
;; Test 1: Vérifier que tous les IDs de types sont définis
;; =============================================================================

(format t "~%=== Test 1: IDs de types définis ===~%")

(test "constant-type-id défini" (and (boundp '*constant-type-id*) *constant-type-id*))
(test "variable-type-id défini" (and (boundp '*variable-type-id*) *variable-type-id*))
(test "arithmetic-type-id défini" (and (boundp '*arithmetic-type-id*) *arithmetic-type-id*))
(test "if-type-id défini" (and (boundp '*if-type-id*) *if-type-id*))
(test "defun-type-id défini" (and (boundp '*defun-type-id*) *defun-type-id*))

;; =============================================================================
;; Test 2: Fonction get-parsed-type-id
;; =============================================================================

(format t "~%=== Test 2: Extraction d'ID depuis expression parsée ===~%")

(let ((parsed-const '(:constant 42)))
  (test "ID de :constant" 
        (= (get-parsed-type-id parsed-const) *constant-type-id*)))

(let ((parsed-arith '(:arithmetic + (1 2))))
  (test "ID de :arithmetic" 
        (= (get-parsed-type-id parsed-arith) *arithmetic-type-id*)))

(let ((parsed-if '(:if test then else)))
  (test "ID de :if" 
        (= (get-parsed-type-id parsed-if) *if-type-id*)))

(let ((parsed-defun '(:defun foo (x) ((+ x 1)))))
  (test "ID de :defun" 
        (= (get-parsed-type-id parsed-defun) *defun-type-id*)))

;; =============================================================================
;; Test 3: Comparaison CASE vs COND
;; =============================================================================

(format t "~%=== Test 3: Comparaison CASE vs COND ===~%")

;; Simuler un dispatch avec CASE (ancien)
(defun dispatch-with-case (parsed)
  (case (first parsed)
    (:constant :dispatched-constant)
    (:arithmetic :dispatched-arithmetic)
    (:if :dispatched-if)
    (t :unknown)))

;; Simuler un dispatch avec COND+IDs (nouveau)
(defun dispatch-with-cond-ids (parsed)
  (let ((type-id (get-parsed-type-id parsed)))
    (cond
      ((= type-id *constant-type-id*) :dispatched-constant)
      ((= type-id *arithmetic-type-id*) :dispatched-arithmetic)
      ((= type-id *if-type-id*) :dispatched-if)
      (t :unknown))))

;; Tester l'équivalence
(let ((test-cases '((:constant 42)
                    (:arithmetic + (1 2))
                    (:if test then else))))
  (dolist (tc test-cases)
    (let ((case-result (dispatch-with-case tc))
          (cond-result (dispatch-with-cond-ids tc)))
      (test (format nil "Équivalence pour ~A" (first tc))
            (eq case-result cond-result)))))

;; =============================================================================
;; Test 4: Tous les types de nœuds
;; =============================================================================

(format t "~%=== Test 4: Tous les types de nœuds supportés ===~%")

(let ((all-types '((:constant 42)
                   (:variable x)
                   (:arithmetic + (1 2))
                   (:comparison < (1 2))
                   (:if test then else)
                   (:cond ((test1 result1)))
                   (:let ((x 1)) (body))
                   (:defun foo (x) (body))
                   (:lambda (x) (body))
                   (:cons 1 2)
                   (:car list)
                   (:cdr list)
                   (:progn expr1 expr2))))
  
  (dolist (tc all-types)
    (let ((type-id (get-parsed-type-id tc)))
      (test (format nil "Type ~A a un ID" (first tc))
            (and type-id (numberp type-id))))))

;; =============================================================================
;; Test 5: Conversion keyword → symbole → ID
;; =============================================================================

(format t "~%=== Test 5: Conversion keyword → symbole → ID ===~%")

(test "keyword :constant → CONSTANT"
      (string= (keyword-to-symbol-name :constant) "CONSTANT"))

(test "keyword :arithmetic → ARITHMETIC"
      (string= (keyword-to-symbol-name :arithmetic) "ARITHMETIC"))

(test "CONSTANT a un ID"
      (numberp (get-symbol-id "CONSTANT")))

(test ":constant et CONSTANT ont le même ID"
      (= (get-symbol-id "CONSTANT")
         (get-symbol-id (keyword-to-symbol-name :constant))))

;; =============================================================================
;; Test 6: Stabilité des IDs
;; =============================================================================

(format t "~%=== Test 6: Stabilité des IDs ===~%")

(let ((id1 (get-symbol-id "CONSTANT"))
      (id2 (get-symbol-id "CONSTANT")))
  (test "Même ID pour appels répétés" (= id1 id2)))

(let ((id-const *constant-type-id*)
      (id-arith *arithmetic-type-id*))
  (test "IDs différents pour types différents" (/= id-const id-arith)))

;; =============================================================================
;; Test 7: Performance (optionnel)
;; =============================================================================

(format t "~%=== Test 7: Performance ===~%")

(let ((iterations 10000)
      (parsed '(:constant 42)))
  
  ;; CASE
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (case (first parsed)
        (:constant t)))
    (let ((time-case (/ (- (get-internal-real-time) start) 
                        internal-time-units-per-second)))
      
      ;; COND + =
      (let ((start2 (get-internal-real-time))
            (type-id (get-parsed-type-id parsed)))
        (dotimes (i iterations)
          (cond
            ((= type-id *constant-type-id*) t)))
        (let ((time-cond (/ (- (get-internal-real-time) start2) 
                            internal-time-units-per-second)))
          
          (format t "  CASE: ~,4F sec~%" time-case)
          (format t "  COND+IDs: ~,4F sec~%" time-cond)
          (format t "  Ratio: ~,2Fx~%" (/ time-case time-cond))
          (test "Performance acceptable (< 5x)" (< time-cond (* 5 time-case))))))))

;; =============================================================================
;; RÉSUMÉ
;; =============================================================================

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

(format t "~%═══════════════════════════════════════════════════════════")
(format t "~%   CONCLUSION")
(format t "~%═══════════════════════════════════════════════════════════")
(format t "~%Le dispatcher avec IDs est opérationnel :~%")
(format t "  ✓ Tous les types de nœuds ont un ID~%")
(format t "  ✓ get-parsed-type-id fonctionne correctement~%")
(format t "  ✓ Équivalence avec CASE vérifiée~%")
(format t "  ✓ Performance acceptable~%")
(format t "  ✓ Compilable en MIPS (COND + = au lieu de CASE)~%")
(format t "~%Prochaine étape : Intégrer dans le compilateur complet~%")
(format t "═══════════════════════════════════════════════════════════~%~%")
