#!/usr/bin/clisp
;;;; TEST DES ALGORITHMES SUR LISTES
;;;; Tests des 10 algorithmes documentés dans DOC_STRUCTURES_DONNEES.txt

(load "src/vm.lisp")
(load "src/loader.lisp")
(load "src/compiler.lisp")
(load "src/compiler-simplified.lisp")
(load "src/loader-simplified.lisp")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-algorithm (name code input expected &optional (desc ""))
  "Teste un algorithme et affiche le résultat"
  (format t "~%Test: ~A" name)
  (when (> (length desc) 0)
    (format t " (~A)" desc))
  (format t "~%")
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
              (run vm)
              (let* ((result (get-register vm :$V0))
                     (end-time (get-internal-real-time))
                     (elapsed (/ (- end-time start-time) internal-time-units-per-second))
                     (instructions (vm-instruction-count vm)))
                (if (or (equal result expected)
                        (and (numberp result) (numberp expected) (= result expected))
                        (and (> result 0) (eq expected 'handle)))  ; Pour handles de listes
                    (progn
                      (format t "✅ Résultat: ~A~%" result)
                      (format t "   [~,3F sec, ~A instructions]~%" elapsed instructions)
                      (incf *tests-passed*))
                    (progn
                      (format t "❌ Attendu: ~A, Obtenu: ~A~%" expected result)
                      (incf *tests-failed*)))))))
    (error (e)
      (format t "❌ ERREUR: ~A~%" e)
      (incf *tests-failed*))))

(format t "═══════════════════════════════════════════════════════════════════════════════~%")
(format t "     TESTS DES ALGORITHMES SUR LISTES (DOC_STRUCTURES_DONNEES.txt)~%")
(format t "═══════════════════════════════════════════════════════════════════════════════~%")

;; ============================================================================
;; ALGORITHME 1: LONGUEUR DE LISTE
;; ============================================================================
(format t "~%~%ALGORITHME 1: LONGUEUR DE LISTE (my-length)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "length(NIL)" 
 '(defun my-length (lst)
    (if (null lst)
        0
        (+ 1 (my-length (cdr lst)))))
 0  ; input (NIL)
 0  ; expected
 "Liste vide")

;; ============================================================================
;; ALGORITHME 2: SOMME D'UNE LISTE
;; ============================================================================
(format t "~%~%ALGORITHME 2: SOMME D'UNE LISTE (sum-list)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "sum-list avec make-range(10)" 
 '(progn
    (defun sum-list (lst)
      (if (null lst)
          0
          (+ (car lst) (sum-list (cdr lst)))))
    (defun make-range (n)
      (if (= n 0)
          nil
          (cons n (make-range (- n 1)))))
    (sum-list (make-range 10)))
 55
 "1+2+...+10 = 55")

(test-algorithm 
 "sum-list(NIL)" 
 '(progn
    (defun sum-list (lst)
      (if (null lst)
          0
          (+ (car lst) (sum-list (cdr lst)))))
    (sum-list nil))
 0
 "Liste vide")

;; ============================================================================
;; ALGORITHME 3: REVERSE D'UNE LISTE (version accumulator)
;; ============================================================================
(format t "~%~%ALGORITHME 3: REVERSE D'UNE LISTE (my-reverse-acc)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "reverse-acc avec make-range(5)" 
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
    (my-reverse (make-range 5)))
 'handle
 "Retourne handle de liste inversée")

;; ============================================================================
;; ALGORITHME 4: MEMBRE (recherche dans liste)
;; ============================================================================
(format t "~%~%ALGORITHME 4: MEMBRE - RECHERCHE DANS LISTE (member-rec)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "member-rec: 5 dans (5 4 3 2 1)" 
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
    (member-rec 5 (make-range 5)))
 1
 "Trouvé")

(test-algorithm 
 "member-rec: 10 dans (5 4 3 2 1)" 
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
    (member-rec 10 (make-range 5)))
 0
 "Non trouvé")

;; ============================================================================
;; ALGORITHME 5: MAP (appliquer fonction)
;; ============================================================================
(format t "~%~%ALGORITHME 5: MAP - INCRÉMENTER CHAQUE ÉLÉMENT (map-inc)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
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
    (map-inc (make-range 3)))
 'handle
 "Retourne liste (4 3 2)")

;; ============================================================================
;; ALGORITHME 6: FILTER (filtrer liste)
;; ============================================================================
(format t "~%~%ALGORITHME 6: FILTER - ÉLÉMENTS POSITIFS (filter-positifs)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "filter-positifs sur liste mixte" 
 '(progn
    (defun filter-positifs (lst)
      (if (null lst)
          nil
          (if (> (car lst) 0)
              (cons (car lst) (filter-positifs (cdr lst)))
              (filter-positifs (cdr lst)))))
    (defun build-list (a b c d)
      (cons a (cons b (cons c (cons d nil)))))
    (filter-positifs (build-list 5 -2 3 -1)))
 'handle
 "Retourne liste (5 3)")

;; ============================================================================
;; ALGORITHME 7: COUNT (compter éléments > seuil)
;; ============================================================================
(format t "~%~%ALGORITHME 7: COUNT - COMPTER ÉLÉMENTS > SEUIL~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "count éléments > 3 dans (5 4 3 2 1)" 
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
    (count-greater (make-range 5) 3))
 2
 "5 et 4 sont > 3")

;; ============================================================================
;; ALGORITHME 8: MAX-LIST (maximum d'une liste)
;; ============================================================================
(format t "~%~%ALGORITHME 8: MAX-LIST - MAXIMUM D'UNE LISTE~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
 "max-list sur (3 7 2 9 1)" 
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
    (defun build-list5 (a b c d e)
      (cons a (cons b (cons c (cons d (cons e nil))))))
    (max-list (build-list5 3 7 2 9 1)))
 9
 "Maximum est 9")

;; ============================================================================
;; ALGORITHME 9: ARBRE BINAIRE - SOMME DES NOEUDS
;; ============================================================================
(format t "~%~%ALGORITHME 9: ARBRE BINAIRE - SOMME DES NOEUDS (tree-sum)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
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
    (tree-sum (make-node 10 (make-leaf 5) (make-leaf 15))))
 30
 "10+5+15 = 30")

;; ============================================================================
;; ALGORITHME 10: PROFONDEUR D'ARBRE
;; ============================================================================
(format t "~%~%ALGORITHME 10: PROFONDEUR D'ARBRE (tree-depth)~%")
(format t "─────────────────────────────────────────────────────────────────────────────~%")

(test-algorithm 
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
    (tree-depth (make-node 10 
                          (make-node 5 (make-leaf 3) nil)
                          (make-leaf 15))))
 3
 "Profondeur = 3")

;; ============================================================================
;; RÉSUMÉ
;; ============================================================================
(format t "~%~%═══════════════════════════════════════════════════════════════════════════════~%")
(format t "RÉSUMÉ DES TESTS~%")
(format t "═══════════════════════════════════════════════════════════════════════════════~%")
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

(format t "═══════════════════════════════════════════════════════════════════════════════~%")

(quit)
