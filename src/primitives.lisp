;;;; primitives.lisp
;;;; Primitives LISP réécrites pour auto-compilation (Phase 10)
;;;; Ces fonctions remplacent les fonctions natives pour permettre
;;;; au compilateur de se compiler lui-même.

;;; ============================================================================
;;; ASSOCIATION LISTS
;;; ============================================================================

(defun my-assoc (key alist)
  "Chercher key dans alist (remplace ASSOC native).
   Retourne la paire (key . value) ou NIL si non trouvé."
  (cond
    ((null alist) nil)
    ((equal key (caar alist)) (car alist))
    (t (my-assoc key (cdr alist)))))

;;; ============================================================================
;;; TRANSFORMATION DE LISTES
;;; ============================================================================

(defun my-mapcar (fn lst)
  "Version simplifiée de mapcar pour une seule liste.
   Applique fn à chaque élément de lst et retourne la liste des résultats."
  (if (null lst)
      nil
      (cons (funcall fn (car lst))
            (my-mapcar fn (cdr lst)))))

;; Versions spécialisées sans funcall (plus robustes pour bootstrap)

(defun my-map-first (lst)
  "Extrait le premier élément de chaque sous-liste.
   Équivalent à (mapcar #'first lst)"
  (if (null lst)
      nil
      (cons (car (car lst))
            (my-map-first (cdr lst)))))

(defun my-map-second (lst)
  "Extrait le second élément de chaque sous-liste.
   Équivalent à (mapcar #'second lst)"
  (if (null lst)
      nil
      (cons (car (cdr (car lst)))
            (my-map-second (cdr lst)))))

(defun my-map-car (lst)
  "Applique CAR à chaque élément de lst.
   Équivalent à (mapcar #'car lst)"
  (if (null lst)
      nil
      (cons (car (car lst))
            (my-map-car (cdr lst)))))

(defun my-map-cdr (lst)
  "Applique CDR à chaque élément de lst.
   Équivalent à (mapcar #'cdr lst)"
  (if (null lst)
      nil
      (cons (cdr (car lst))
            (my-map-cdr (cdr lst)))))

;;; ============================================================================
;;; GÉNÉRATION DE STRINGS (remplace FORMAT simple)
;;; ============================================================================

(defun my-concat-strings (&rest strings)
  "Concaténation de strings (remplace CONCATENATE).
   Usage: (my-concat-strings \"Hello\" \" \" \"World\")"
  (apply #'concatenate 'string strings))

(defun my-int-to-string (n)
  "Convertit un entier en string.
   Équivalent à (princ-to-string n)"
  (princ-to-string n))

(defun my-format-label (prefix counter)
  "Génère un label unique de la forme PREFIX_COUNTER.
   Remplace: (format nil \"~A_~A\" prefix counter)
   Usage: (my-format-label \"LOOP\" 42) => \"LOOP_42\""
  (concatenate 'string 
               (if (symbolp prefix) (string prefix) prefix)
               "_" 
               (princ-to-string counter)))

(defun my-format-register (reg-num)
  "Génère un nom de registre temporaire.
   Remplace: (format nil \":$T~A\" reg-num)
   Usage: (my-format-register 0) => \":$T0\""
  (concatenate 'string ":$T" (princ-to-string reg-num)))

;;; ============================================================================
;;; UTILITAIRES DE LISTES (bonus)
;;; ============================================================================

(defun my-append (lst1 lst2)
  "Concaténation de deux listes (tail-recursive).
   Équivalent à (append lst1 lst2)"
  (labels ((append-aux (l1 acc)
             (if (null l1)
                 (nreverse acc)
                 (append-aux (cdr l1) (cons (car l1) acc)))))
    (append-aux (reverse lst1) (reverse lst2))))

(defun my-reverse (lst)
  "Inversion de liste (tail-recursive).
   Équivalent à (reverse lst)"
  (labels ((rev-aux (l acc)
             (if (null l)
                 acc
                 (rev-aux (cdr l) (cons (car l) acc)))))
    (rev-aux lst nil)))

(defun my-length (lst)
  "Longueur d'une liste.
   Équivalent à (length lst)"
  (if (null lst)
      0
      (+ 1 (my-length (cdr lst)))))

(defun my-nth (n lst)
  "N-ième élément d'une liste (0-indexé).
   Équivalent à (nth n lst)"
  (if (= n 0)
      (car lst)
      (my-nth (- n 1) (cdr lst))))

(defun my-remove-if (pred lst)
  "Retirer les éléments satisfaisant pred.
   Équivalent à (remove-if pred lst)"
  (cond
    ((null lst) nil)
    ((funcall pred (car lst))
     (my-remove-if pred (cdr lst)))
    (t (cons (car lst)
             (my-remove-if pred (cdr lst))))))

(defun my-find (item lst &key (test #'equal))
  "Trouver item dans lst.
   Équivalent à (find item lst :test test)"
  (cond
    ((null lst) nil)
    ((funcall test item (car lst)) (car lst))
    (t (my-find item (cdr lst) :test test))))

;;; ============================================================================
;;; TESTS DES PRIMITIVES
;;; ============================================================================

(defun test-primitives ()
  "Teste toutes les primitives implémentées.
   Retourne T si tous les tests passent, sinon signale une erreur."
  (format t "~%╔════════════════════════════════════════╗~%")
  (format t "║  TESTS DES PRIMITIVES (Phase 10)      ║~%")
  (format t "╚════════════════════════════════════════╝~%~%")
  
  ;; Test my-assoc
  (format t "Test my-assoc...~%")
  (assert (equal (my-assoc 'b '((a 1) (b 2) (c 3))) '(b 2)))
  (assert (equal (my-assoc 'x '((a 1) (b 2) (c 3))) nil))
  (assert (equal (cdr (my-assoc 'c '((a 1) (b 2) (c 3)))) '(3)))
  (format t "  ✓ my-assoc OK~%")
  
  ;; Test my-mapcar
  (format t "Test my-mapcar...~%")
  (assert (equal (my-mapcar #'car '((a 1) (b 2) (c 3))) '(a b c)))
  (assert (equal (my-mapcar (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6)))
  (format t "  ✓ my-mapcar OK~%")
  
  ;; Test my-map-first
  (format t "Test my-map-first...~%")
  (assert (equal (my-map-first '((a 1) (b 2) (c 3))) '(a b c)))
  (assert (equal (my-map-first '((x 10) (y 20))) '(x y)))
  (format t "  ✓ my-map-first OK~%")
  
  ;; Test my-map-second
  (format t "Test my-map-second...~%")
  (assert (equal (my-map-second '((a 1) (b 2) (c 3))) '(1 2 3)))
  (assert (equal (my-map-second '((x 10) (y 20))) '(10 20)))
  (format t "  ✓ my-map-second OK~%")
  
  ;; Test my-format-label
  (format t "Test my-format-label...~%")
  (assert (equal (my-format-label "LOOP" 42) "LOOP_42"))
  (assert (equal (my-format-label 'LABEL 0) "LABEL_0"))
  (format t "  ✓ my-format-label OK~%")
  
  ;; Test my-format-register
  (format t "Test my-format-register...~%")
  (assert (equal (my-format-register 0) ":$T0"))
  (assert (equal (my-format-register 5) ":$T5"))
  (format t "  ✓ my-format-register OK~%")
  
  ;; Test my-reverse
  (format t "Test my-reverse...~%")
  (assert (equal (my-reverse '(1 2 3)) '(3 2 1)))
  (assert (equal (my-reverse '(a)) '(a)))
  (assert (equal (my-reverse '()) '()))
  (format t "  ✓ my-reverse OK~%")
  
  ;; Test my-length
  (format t "Test my-length...~%")
  (assert (= (my-length '(1 2 3)) 3))
  (assert (= (my-length '()) 0))
  (format t "  ✓ my-length OK~%")
  
  ;; Test my-nth
  (format t "Test my-nth...~%")
  (assert (equal (my-nth 0 '(a b c)) 'a))
  (assert (equal (my-nth 2 '(a b c)) 'c))
  (format t "  ✓ my-nth OK~%")
  
  (format t "~%╔════════════════════════════════════════╗~%")
  (format t "║  ✅ TOUS LES TESTS RÉUSSIS!           ║~%")
  (format t "╚════════════════════════════════════════╝~%~%")
  
  t)

;;; ============================================================================
;;; COMPARAISON AVEC FONCTIONS NATIVES (optionnel, pour validation)
;;; ============================================================================

(defun compare-with-native ()
  "Compare les implémentations avec les fonctions natives.
   Utile pour vérifier que les primitives sont équivalentes."
  (format t "~%=== COMPARAISON AVEC FONCTIONS NATIVES ===~%~%")
  
  (let ((test-alist '((a 1) (b 2) (c 3)))
        (test-list '((x 10) (y 20) (z 30))))
    
    ;; Test assoc vs my-assoc
    (format t "ASSOC vs MY-ASSOC:~%")
    (format t "  Native: ~A~%" (assoc 'b test-alist))
    (format t "  Custom: ~A~%" (my-assoc 'b test-alist))
    (format t "  Match:  ~A~%~%" (equal (assoc 'b test-alist) 
                                          (my-assoc 'b test-alist)))
    
    ;; Test mapcar vs my-mapcar
    (format t "MAPCAR vs MY-MAPCAR:~%")
    (format t "  Native: ~A~%" (mapcar #'car test-list))
    (format t "  Custom: ~A~%" (my-mapcar #'car test-list))
    (format t "  Match:  ~A~%~%" (equal (mapcar #'car test-list) 
                                          (my-mapcar #'car test-list)))
    
    ;; Test format vs my-format-label
    (format t "FORMAT vs MY-FORMAT-LABEL:~%")
    (format t "  Native: ~A~%" (format nil "~A_~A" "LOOP" 42))
    (format t "  Custom: ~A~%" (my-format-label "LOOP" 42))
    (format t "  Match:  ~A~%~%" (equal (format nil "~A_~A" "LOOP" 42) 
                                          (my-format-label "LOOP" 42)))
    
    t))

;;; ============================================================================
;;; MESSAGE DE CHARGEMENT
;;; ============================================================================

(format t "~%✅ Primitives LISP chargées (Phase 10 Bootstrap).~%")
(format t "   Fonctions disponibles:~%")
(format t "   - (my-assoc key alist)~%")
(format t "   - (my-mapcar fn lst)~%")
(format t "   - (my-map-first lst)~%")
(format t "   - (my-map-second lst)~%")
(format t "   - (my-format-label prefix counter)~%")
(format t "   - (my-format-register reg-num)~%")
(format t "   - ... et autres utilitaires~%")
(format t "~%   Tests: (test-primitives)~%")
(format t "   Comparaison: (compare-with-native)~%~%")
