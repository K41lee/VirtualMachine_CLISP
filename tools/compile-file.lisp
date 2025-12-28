;;; ============================================================================
;;; UTILITAIRE : COMPILATION DE FICHIER COMPLET
;;; ============================================================================
;;;
;;; Cet utilitaire permet de charger un fichier Lisp et de compiler
;;; toutes ses fonctions DEFUN en MIPS.
;;;
;;; Usage:
;;;   (load "tools/compile-file.lisp")
;;;   (compile-file-to-mips "src/loader-compilable.lisp")
;;;
;;; ============================================================================

(defun extract-defuns (file-path)
  "Lit un fichier et extrait toutes les formes DEFUN.
   Retourne une liste de (nom-fonction . source-defun)"
  (with-open-file (stream file-path :direction :input)
    (let ((defuns '())
          (form nil))
      (loop
        (setf form (read stream nil :eof))
        (when (eq form :eof)
          (return))
        (when (and (consp form) (eq (first form) 'defun))
          (push (cons (second form) form) defuns)))
      (nreverse defuns))))

(defun compile-file-to-mips (file-path &key (verbose t))
  "Compile toutes les fonctions DEFUN d'un fichier en MIPS.
   Retourne une hash-table: nom-fonction → code-mips"
  (let ((defuns (extract-defuns file-path))
        (compiled (make-hash-table :test 'eq))
        (total-instructions 0))
    
    (when verbose
      (format t "~%═══════════════════════════════════════════════════════════════════~%")
      (format t "Compilation de ~A~%" file-path)
      (format t "═══════════════════════════════════════════════════════════════════~%~%"))
    
    (dolist (entry defuns)
      (let* ((func-name (car entry))
             (func-source (cdr entry))
             (mips-code nil))
        
        (when verbose
          (format t "  → Compilation de ~A...~%" func-name))
        
        (handler-case
            (progn
              (setf mips-code (compile-lisp func-source))
              (setf (gethash func-name compiled) mips-code)
              (incf total-instructions (length mips-code))
              (when verbose
                (format t "     ✓ ~A instructions~%" (length mips-code))))
          (error (e)
            (when verbose
              (format t "     ✗ ERREUR: ~A~%" e))))))
    
    (when verbose
      (format t "~%  ✅ Total: ~A fonctions, ~A instructions~%~%" 
              (hash-table-count compiled)
              total-instructions))
    
    compiled))

(defun get-compiled-function (compiled-table func-name)
  "Récupère le code MIPS d'une fonction compilée"
  (gethash func-name compiled-table))

(defun save-compiled-to-vars (compiled-table &key (verbose t))
  "Sauvegarde chaque fonction compilée dans une variable globale.
   Crée *FUNC-NAME-mips* pour chaque fonction."
  (when verbose
    (format t "~%Création de variables globales pour le code compilé:~%"))
  
  (maphash 
   #'(lambda (func-name mips-code)
       (let ((var-name (intern (format nil "*~A-MIPS*" 
                                      (string-upcase (symbol-name func-name))))))
         (setf (symbol-value var-name) mips-code)
         (when verbose
           (format t "  • ~A → ~A instructions~%" var-name (length mips-code)))))
   compiled-table)
  
  (when verbose
    (format t "~%✅ Variables créées~%")))

(defun compile-and-save-file (file-path &key (verbose t))
  "Compile un fichier et sauvegarde chaque fonction dans une variable globale.
   Retourne la hash-table des fonctions compilées."
  (let ((compiled (compile-file-to-mips file-path :verbose verbose)))
    (save-compiled-to-vars compiled :verbose verbose)
    compiled))

(defun compile-file-to-block (file-path &key (verbose t))
  "Compile toutes les fonctions d'un fichier en un seul bloc MIPS.
   Retourne une liste de toutes les instructions concaténées.
   Optionnellement crée aussi les variables globales individuelles."
  (let ((defuns (extract-defuns file-path))
        (all-instructions '())
        (function-map (make-hash-table :test 'eq))
        (total-instructions 0))
    
    (when verbose
      (format t "~%═══════════════════════════════════════════════════════════════════~%")
      (format t "Compilation de ~A en bloc~%" file-path)
      (format t "═══════════════════════════════════════════════════════════════════~%~%"))
    
    ;; Compiler chaque fonction
    (dolist (entry defuns)
      (let* ((func-name (car entry))
             (func-source (cdr entry))
             (mips-code nil)
             (start-offset total-instructions))
        
        (when verbose
          (format t "  → Compilation de ~A...~%" func-name))
        
        (handler-case
            (progn
              (setf mips-code (compile-lisp func-source))
              
              ;; Sauvegarder dans la hash-table avec position
              (setf (gethash func-name function-map) 
                    (list :code mips-code 
                          :offset start-offset 
                          :length (length mips-code)))
              
              ;; Ajouter au bloc total
              (setf all-instructions (append all-instructions mips-code))
              (incf total-instructions (length mips-code))
              
              ;; Créer la variable globale pour cette fonction
              (let ((var-name (intern (format nil "*~A-MIPS*" 
                                            (string-upcase (symbol-name func-name))))))
                (setf (symbol-value var-name) mips-code))
              
              (when verbose
                (format t "     ✓ ~A instructions (offset ~A)~%" 
                        (length mips-code) start-offset)))
          (error (e)
            (when verbose
              (format t "     ✗ ERREUR: ~A~%" e))))))
    
    (when verbose
      (format t "~%  ✅ Total: ~A fonctions, ~A instructions en bloc~%" 
              (hash-table-count function-map)
              total-instructions)
      (format t "~%Variables globales créées pour chaque fonction.~%"))
    
    ;; Retourner le bloc complet et la map des fonctions
    (values all-instructions function-map)))

(defun compile-and-load-file (vm file-path &key (verbose t))
  "Compile un fichier entier et le charge d'un coup dans la VM.
   Retourne l'adresse de début et la map des fonctions avec leurs offsets."
  (multiple-value-bind (all-code function-map)
      (compile-file-to-block file-path :verbose verbose)
    
    (when verbose
      (format t "~%  → Chargement du bloc complet dans la VM...~%"))
    
    ;; Obtenir l'adresse de début (chercher la première cellule vide)
    (let* ((initial-addr (calculate-code-start vm))
           (base-addr initial-addr))
      
      ;; Chercher la première adresse libre (cellule vide)
      (loop while (and (< base-addr *maxmem*)
                       (not (zerop (mem-read vm base-addr))))
            do (incf base-addr))
      
      ;; Charger tout le code d'un coup SANS passer par load-code
      ;; (pour éviter l'ajout automatique de HALT)
      ;; Nous allons écrire directement en mémoire
      (multiple-value-bind (resolved-code labels)
          (preprocess-code all-code base-addr)
        
        ;; Écrire chaque instruction en mémoire
        (let ((addr base-addr))
          (dolist (instr resolved-code)
            (mem-write vm addr instr)
            (incf addr)))
        
        (when verbose
          (format t "  ✓ ~A instructions chargées à l'adresse ~A~%~%" 
                  (length resolved-code) base-addr))
        
        ;; Calculer les adresses absolues de chaque fonction
        (let ((addresses (make-hash-table :test 'eq))
              (end-addr (+ base-addr (length resolved-code))))
          (maphash 
           #'(lambda (func-name info)
               (let ((offset (getf info :offset)))
                 (setf (gethash func-name addresses) (+ base-addr offset))))
           function-map)
          
          (when verbose
            (format t "  Adresses des fonctions:~%")
            (maphash 
             #'(lambda (func-name addr)
                 (format t "     • ~A : ~A~%" func-name addr))
             addresses))
          
          ;; Retourner l'adresse de début, la map des adresses, et le code
          (values base-addr addresses all-code end-addr))))))
