;;;; Test des algorithmes sur listes (version simplifiée)
;;;;

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║   TESTS ALGORITHMES SUR LISTES (DOC_STRUCTURES_DONNEES.txt)   ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-algo (name code input expected)
  "Teste un algorithme"
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
                     (end-time (get-internal-real-time))
                     (elapsed (/ (- end-time start-time) internal-time-units-per-second))
                     (instructions (vm-instruction-count vm)))
                (if (or (= result expected)
                        (and (> result 0) (eq expected 'handle)))
                    (progn
                      (format t "  ✅ = ~A [~,3F sec, ~A instr]~%" result elapsed instructions)
                      (incf *tests-passed*))
                    (progn
                      (format t "  ❌ Attendu ~A, obtenu ~A~%" expected result)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "  ❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

;; ============================================================================
;; ALGORITHME 1: Longueur de liste
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 1: LONGUEUR DE LISTE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "my-length(NIL)"
 '(defun my-length (lst)
    (if (null lst)
        0
        (+ 1 (my-length (cdr lst)))))
 0  ; NIL
 0)

;; ============================================================================
;; ALGORITHME 2: Somme d'une liste
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 2: SOMME D'UNE LISTE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "sum-list(NIL)"
 '(defun sum-list (lst)
    (if (null lst)
        0
        (+ (car lst) (sum-list (cdr lst)))))
 0  ; NIL
 0)

(test-algo 
 "sum-list(make-range(10))"
 '(progn
    (defun sum-list (lst)
      (if (null lst)
          0
          (+ (car lst) (sum-list (cdr lst)))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (sum-list (make-range x))))
 10  ; Input
 55) ; 1+2+...+10

;; ============================================================================
;; ALGORITHME 3: Membre (recherche dans liste)
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 3: MEMBRE (RECHERCHE)~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "member-rec: 5 trouvé dans liste"
 '(progn
    (defun member-rec (x lst)
      (if (null lst)
          0
          (if (= x (car lst))
              1
              (member-rec x (cdr lst)))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (member-rec 5 (make-range 10))))
 0  ; Input (non utilisé)
 1) ; Trouvé

(test-algo 
 "member-rec: 20 non trouvé"
 '(progn
    (defun member-rec (x lst)
      (if (null lst)
          0
          (if (= x (car lst))
              1
              (member-rec x (cdr lst)))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (member-rec 20 (make-range 10))))
 0  ; Input
 0) ; Non trouvé

;; ============================================================================
;; ALGORITHME 4: Count (compter éléments > seuil)
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 4: COUNT ÉLÉMENTS > SEUIL~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "count-greater(3) dans make-range(5)"
 '(progn
    (defun count-greater (lst threshold)
      (if (null lst)
          0
          (if (> (car lst) threshold)
              (+ 1 (count-greater (cdr lst) threshold))
              (count-greater (cdr lst) threshold))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (count-greater (make-range 5) 3)))
 0  ; Input
 2) ; 5 et 4 sont > 3

;; ============================================================================
;; ALGORITHME 5: Maximum d'une liste
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 5: MAXIMUM D'UNE LISTE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "max-list de make-range(10)"
 '(progn
    (defun max2 (a b)
      (if (> a b) a b))
    (defun max-list-aux (lst current-max)
      (if (null lst)
          current-max
          (max-list-aux (cdr lst) (max2 (car lst) current-max))))
    (defun max-list (lst)
      (if (null lst)
          0
          (max-list-aux (cdr lst) (car lst))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (max-list (make-range x))))
 10  ; Input
 10) ; Maximum

;; ============================================================================
;; ALGORITHME 6: Arbre binaire - Somme des noeuds
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 6: ARBRE BINAIRE - SOMME~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "tree-sum sur arbre (10 (5 nil nil) (15 nil nil))"
 '(progn
    (defun tree-sum (tree)
      (if (null tree)
          0
          (+ (car tree)
             (tree-sum (car (cdr tree)))
             (tree-sum (car (cdr (cdr tree)))))))
    (defun make-leaf (val)
      (cons val (cons nil (cons nil nil))))
    (defun make-node (val left right)
      (cons val (cons left (cons right nil))))
    (defun main (x)
      (tree-sum (make-node 10 (make-leaf 5) (make-leaf 15)))))
 0   ; Input
 30) ; 10+5+15

;; ============================================================================
;; ALGORITHME 7: Profondeur d'arbre
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 7: PROFONDEUR D'ARBRE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "tree-depth sur arbre déséquilibré"
 '(progn
    (defun max2 (a b)
      (if (> a b) a b))
    (defun tree-depth (tree)
      (if (null tree)
          0
          (+ 1 (max2 (tree-depth (car (cdr tree)))
                     (tree-depth (car (cdr (cdr tree))))))))
    (defun make-leaf (val)
      (cons val (cons nil (cons nil nil))))
    (defun make-node (val left right)
      (cons val (cons left (cons right nil))))
    (defun main (x)
      (tree-depth (make-node 10 
                            (make-node 5 (make-leaf 3) nil)
                            (make-leaf 15)))))
 0  ; Input
 3) ; Profondeur

;; ============================================================================
;; ALGORITHME 8: Construction de liste (map-inc)
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 8: MAP - INCRÉMENTER CHAQUE ÉLÉMENT~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "map-inc sur make-range(3)"
 '(progn
    (defun map-inc (lst)
      (if (null lst)
          nil
          (cons (+ 1 (car lst)) (map-inc (cdr lst)))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (map-inc (make-range 3))))
 0      ; Input
 'handle) ; Retourne handle

;; ============================================================================
;; ALGORITHME 9: Filter (éléments positifs)
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 9: FILTER - ÉLÉMENTS POSITIFS~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "filter-positifs sur liste avec négatifs"
 '(progn
    (defun filter-positifs (lst)
      (if (null lst)
          nil
          (if (> (car lst) 0)
              (cons (car lst) (filter-positifs (cdr lst)))
              (filter-positifs (cdr lst)))))
    (defun build-list (a b c d)
      (cons a (cons b (cons c (cons d nil)))))
    (defun main (x)
      (filter-positifs (build-list 5 -2 3 -1))))
 0      ; Input
 'handle) ; Retourne liste (5 3)

;; ============================================================================
;; ALGORITHME 10: Reverse avec accumulateur
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "ALGORITHME 10: REVERSE D'UNE LISTE~%")
(format t "═══════════════════════════════════════════════════════════════~%")

(test-algo 
 "my-reverse avec make-range(5)"
 '(progn
    (defun my-reverse-acc (lst acc)
      (if (null lst)
          acc
          (my-reverse-acc (cdr lst) (cons (car lst) acc))))
    (defun my-reverse (lst)
      (my-reverse-acc lst nil))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (defun main (x)
      (my-reverse (make-range 5))))
 0      ; Input
 'handle) ; Retourne liste inversée

;; ============================================================================
;; RÉSUMÉ
;; ============================================================================
(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ~%")
(format t "═══════════════════════════════════════════════════════════════~%")
(format t "Tests réussis: ~A~%" *tests-passed*)
(format t "Tests échoués: ~A~%" *tests-failed*)
(format t "Total: ~A~%" (+ *tests-passed* *tests-failed*))
(format t "Taux de succès: ~,1F%~%" 
        (if (= (+ *tests-passed* *tests-failed*) 0) 
            0.0
            (* 100.0 (/ *tests-passed* (+ *tests-passed* *tests-failed*)))))

(if (= *tests-failed* 0)
    (format t "~%✅ TOUS LES TESTS SONT PASSÉS ! ✅~%")
    (format t "~%❌ CERTAINS TESTS ONT ÉCHOUÉ ❌~%"))

(format t "═══════════════════════════════════════════════════════════════~%")

(quit)
