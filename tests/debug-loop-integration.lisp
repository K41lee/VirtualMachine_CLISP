;;;; DEBUG LOOP INTEGRATION
;;;; Tester l'intégration LOOP dans le compilateur

;; Charger les modules
(load "src/vm-primitives-structs.lisp")
(load "src/compiler-env-accessors.lisp")
(load "src/loop-parser.lisp")
(load "src/loop-expander.lisp")
(load "src/loop-integration.lisp")
(load "src/compiler.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "DEBUG LOOP INTEGRATION~%")
(format t "═══════════════════════════════════════════════════~%~%")

;; Test simple : parser
(format t "TEST 1 : Parser~%")
(let ((parsed (parse-loop-advanced '(for i from 1 to 3 do (print i)))))
  (format t "  Parsed: ~A~%" parsed)
  (format t "  Type: ~A~%" (type-of parsed))
  (format t "  First: ~A~%" (first parsed))
  (format t "  Second: ~A~%" (second parsed))
  (format t "  Third: ~A~%" (third parsed))
  (format t "  getf :clauses: ~A~%" (getf parsed :clauses))
  (format t "  getf :action: ~A~%" (getf parsed :action))
  (format t "  getf :body: ~A~%" (getf parsed :body))
  (format t "~%")
  
  ;; Essayer l'expansion
  (format t "TEST 2 : Expansion~%")
  (let ((expanded (expand-loop-from-parsed parsed)))
    (format t "  Expanded: ~A~%" expanded)
    (format t "  Type: ~A~%" (type-of expanded))
    (format t "~%")
    
    ;; Essayer de parser l'expansion
    (format t "TEST 3 : Parse de l'expansion~%")
    (handler-case
        (let ((parsed-expanded (parse-lisp-expr expanded)))
          (format t "  Parsed expanded OK: ~A~%" parsed-expanded)
          (format t "~%"))
      (error (e)
        (format t "  ERREUR: ~A~%" e)
        (format t "~%")))))

(format t "Fin du debug~%")
