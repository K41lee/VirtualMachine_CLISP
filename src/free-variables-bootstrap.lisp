;;;; free-variables-bootstrap.lisp
;;;; Version simplifiée de free-variables sans MAPCAR/LAMBDA
;;;; pour permettre l'auto-compilation du compilateur

;;; Cette version remplace les appels à mapcar #'first et mapcar #'second
;;; par des boucles explicites pour éviter la dépendance aux fonctions
;;; d'ordre supérieur.

(defun extract-first-elements (list-of-pairs)
  "Extrait le premier élément de chaque paire.
   Remplace (mapcar #'first list-of-pairs)"
  (if (null list-of-pairs)
      nil
      (cons (first (first list-of-pairs))
            (extract-first-elements (rest list-of-pairs)))))

(defun extract-second-elements (list-of-pairs)
  "Extrait le second élément de chaque paire.
   Remplace (mapcar #'second list-of-pairs)"
  (if (null list-of-pairs)
      nil
      (cons (second (first list-of-pairs))
            (extract-second-elements (rest list-of-pairs)))))

(defun flatten-clauses (clauses)
  "Aplatit une liste de clauses.
   Remplace (mapcan (lambda (clause) clause) clauses)"
  (if (null clauses)
      nil
      (append (first clauses)
              (flatten-clauses (rest clauses)))))

(defun extract-clause-bodies (clauses)
  "Extrait les corps des clauses (tout sauf le premier élément).
   Remplace (mapcan (lambda (clause) (rest clause)) clauses)"
  (if (null clauses)
      nil
      (append (rest (first clauses))
              (extract-clause-bodies (rest clauses)))))

;;; Insérer ce code dans compiler.lisp avant la fonction free-variables
;;; et remplacer les appels :
;;;
;;; (mapcar #'first bindings)           → (extract-first-elements bindings)
;;; (mapcar #'second bindings)          → (extract-second-elements bindings)
;;; (mapcan (lambda (clause) clause) args) → (flatten-clauses args)
;;; (mapcan (lambda (clause) (rest clause)) args) → (extract-clause-bodies args)
