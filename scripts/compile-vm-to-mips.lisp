;;;; compile-vm-to-mips.lisp
;;;; Script pour compiler vm-compilable.lisp vers MIPS
;;;;
;;;; Stratégie:
;;;;   1. Charger le compilateur avec extensions (WHILE, ARRAYS)
;;;;   2. Lire vm-compilable.lisp et extraire chaque DEFUN
;;;;   3. Compiler chaque fonction séparément
;;;;   4. Assembler le tout dans un fichier MIPS cohérent

(load "src/asm-ops.lisp")
(load "src/compiler.lisp")

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║  COMPILATION VM → MIPS                                         ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%~%")

(defun extract-toplevel-forms (filename)
  "Extrait toutes les formes top-level d'un fichier: DEFUN, DEFVAR, DEFCONSTANT"
  (with-open-file (in filename :direction :input)
    (let ((forms '())
          (expr nil))
      (loop
        (setf expr (read in nil nil))
        (when (null expr) (return))
        ;; Ignorer LOAD et commentaires
        (when (and (listp expr) 
                   (member (first expr) '(defun defvar defconstant defparameter)))
          (push expr forms)))
      (reverse forms))))

(defun categorize-forms (forms)
  "Sépare les formes en: (defvars defconstants defuns)"
  (let ((defvars '())
        (defconstants '())
        (defuns '()))
    (dolist (form forms)
      (case (first form)
        ((defvar defparameter) (push form defvars))
        (defconstant (push form defconstants))
        (defun (push form defuns))))
    (values (reverse defvars) (reverse defconstants) (reverse defuns))))

(defun remove-docstring (expr)
  "Retire la docstring d'une expression DEFUN/DEFVAR/DEFCONSTANT si présente"
  (let ((form-type (first expr)))
    (case form-type
      (defun
       (let ((fname (second expr))
             (params (third expr))
             (body (cdddr expr)))
         ;; Si le premier élément du body est une string, c'est une docstring
         (if (and body (stringp (first body)))
             `(defun ,fname ,params ,@(rest body))  ; Retirer la docstring
             expr)))  ; Pas de docstring
      
      ((defvar defparameter defconstant)
       (let ((name (second expr))
             (value (third expr))
             (rest-args (cdddr expr)))
         ;; Si 4ème élément est une string, c'est une docstring
         (if (and rest-args (stringp (first rest-args)))
             `(,form-type ,name ,value)  ; Retirer la docstring
             expr)))  ; Pas de docstring
      
      (t expr))))

(defun compile-vm-to-mips (deps-files input-file output-file)
  "Compile toutes les formes de input-file vers output-file en MIPS.
   deps-files: liste de fichiers à compiler en premier pour les dépendances"
  (format t "Lecture des dépendances...~%")
  
  ;; Reset tables globales
  (reset-global-tables)
  
  ;; ÉTAPE 0: Compiler les fichiers de dépendances (asm-ops.lisp, etc.)
  (dolist (dep-file deps-files)
    (format t "  - ~A~%" dep-file)
    (let ((dep-forms (extract-toplevel-forms dep-file)))
      (multiple-value-bind (dep-defvars dep-defconstants dep-defuns) 
          (categorize-forms dep-forms)
        ;; Compiler seulement DEFCONSTANT et DEFPARAMETER des dépendances
        (dolist (const-expr dep-defconstants)
          (handler-case
              (compile-lisp const-expr)
            (error (e)
              (format t "    Warning: ~A~%" e))))
        (dolist (var-expr dep-defvars)
          (handler-case
              (compile-lisp var-expr)
            (error (e)
              (format t "    Warning: ~A~%" e)))))))
  
  (format t "~%Lecture de ~A...~%" input-file)
  
  (let ((forms (extract-toplevel-forms input-file))
        (all-code '()))
    
    (multiple-value-bind (defvars defconstants defuns) (categorize-forms forms)
      (format t "Trouvé: ~A DEFCONSTANT, ~A DEFVAR, ~A DEFUN~%~%" 
              (length defconstants) (length defvars) (length defuns))
      
      ;; Header du fichier MIPS
      (push ";;; vm-compiled.mips" all-code)
      (push ";;; VM compilée en MIPS depuis vm-compilable.lisp" all-code)
      (push "" all-code)
      
      ;; ÉTAPE 1 : Compiler les DEFCONSTANT (pas de code généré, juste enregistrés)
      (format t "=== Phase 1: DEFCONSTANT ===~%")
      (dolist (const-expr defconstants)
        (let ((name (second const-expr))
              (value (third const-expr)))
          (format t "  ~A = ~A~%" name value)
          (handler-case
              (compile-lisp const-expr)
            (error (e)
              (format t "    ✗ ERREUR: ~A~%" e)))))
      
      (format t "~%=== Phase 2: DEFVAR ===~%")
      ;; ÉTAPE 2 : Compiler les DEFVAR (génère code d'initialisation)
      (dolist (var-expr defvars)
        (let* ((var-clean (remove-docstring var-expr))
               (vname (second var-clean)))
          (format t "Compilation de ~A...~%" vname)
          (handler-case
              (let ((vcode (compile-lisp var-clean)))
                (when vcode
                  (push (format nil "~%; Global var: ~A" vname) all-code)
                  (dolist (instr vcode)
                    (push (format nil "~A" instr) all-code))
                  (push "" all-code)
                  (format t "  ✓ ~A instructions générées~%" (length vcode))))
            (error (e)
              (format t "  ✗ ERREUR: ~A~%" e)))))
      
      (format t "~%=== Phase 3: DEFUN ===~%")
      ;; ÉTAPE 3 : Compiler les fonctions
      (dolist (defun-expr defuns)
        (let* ((defun-clean (remove-docstring defun-expr))
               (fname (second defun-clean))
               (fcode nil))
          (format t "Compilation de ~A...~%" fname)
          (handler-case
              (progn
                (setf fcode (compile-lisp defun-clean))
                (push (format nil "~%; Function: ~A" fname) all-code)
                (dolist (instr fcode)
                  (push (format nil "~A" instr) all-code))
                (push "" all-code)
                (format t "  ✓ ~A instructions générées~%" (length fcode)))
            (error (e)
              (format t "  ✗ ERREUR: ~A~%" e))))))
    
    ;; Écrire le fichier de sortie
    (format t "~%Écriture dans ~A...~%" output-file)
    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede)
      (dolist (line (reverse all-code))
        (format out "~A~%" line)))
    
    (format t "✓ Compilation terminée !~%")
    (format t "   Fichier: ~A~%" output-file)
    (format t "   Lignes: ~A~%" (length all-code))
    t))

;; Exécution
(compile-vm-to-mips 
  '("src/asm-ops.lisp")  ; Fichiers de dépendances
  "src/vm-compilable.lisp" 
  "output/vm-compiled.mips")

(format t "~%Appuyez sur Entrée pour quitter...")
(read-line)
(quit)
