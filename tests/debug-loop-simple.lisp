;;;; DEBUG SIMPLE LOOP EXPANSION
;;;; Version minimale sans charger compiler.lisp

;; Charger UNIQUEMENT les modules LOOP
(load "src/loop-parser.lisp")
(load "src/loop-expander.lisp")
(load "src/loop-integration.lisp")

(format t "~%═══════════════════════════════════════════════════~%")
(format t "DEBUG EXPANSION LOOP~%")
(format t "═══════════════════════════════════════════════════~%~%")

;; Test : parser
(format t "TEST 1 : Parser~%")
(let ((parsed (parse-loop-advanced '(for i from 1 to 3 do (print i)))))
  (format t "  Parsed: ~S~%" parsed)
  (format t "  Type: ~A~%" (type-of parsed))
  (format t "  (car parsed): ~A~%" (car parsed))
  (format t "  (cadr parsed): ~A~%" (cadr parsed))
  (format t "  (caddr parsed): ~A~%" (caddr parsed))
  (format t "  (getf parsed :clauses): ~S~%" (getf parsed :clauses))
  (format t "  (getf parsed :action): ~A~%" (getf parsed :action))
  (format t "  (getf parsed :body): ~S~%" (getf parsed :body))
  (format t "~%")
  
  ;; Essayer l'expansion
  (format t "TEST 2 : Expansion~%")
  (handler-case
      (let ((expanded (expand-loop-from-parsed parsed)))
        (format t "  Expanded: ~S~%" expanded)
        (format t "  Type: ~A~%" (type-of expanded))
        (format t "  SUCCESS!~%"))
    (error (e)
      (format t "  ERREUR dans expansion: ~A~%" e))))

(format t "~%Fin du debug~%")
