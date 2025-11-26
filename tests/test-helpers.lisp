;;;; test-helpers.lisp - Utilitaires pour les tests
;;;; Charge automatiquement les bons chemins

(defvar *project-root* 
  (make-pathname :directory 
    (pathname-directory 
      (truename (or *load-pathname* *default-pathname-defaults*)))))

(defun load-compiler ()
  "Charge le compilateur depuis src/"
  (load (merge-pathnames "../../src/loader.lisp" *project-root*))
  (load (merge-pathnames "../../src/compiler.lisp" *project-root*))
  (load (merge-pathnames "../../src/vm.lisp" *project-root*)))

;; Charger automatiquement au load de ce fichier
(load-compiler)
