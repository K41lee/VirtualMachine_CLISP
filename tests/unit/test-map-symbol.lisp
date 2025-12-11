(defun map-symbol-to-register-keyword (symbol)
  "Test de mapping"
  (case symbol
    ($SP :$SP)
    ($GP :$GP)
    (SP :$SP)
    (GP :$GP)
    (t nil)))

(print (list '$SP '-> (map-symbol-to-register-keyword '$SP)))
(print (list '$GP '-> (map-symbol-to-register-keyword '$GP)))
(print (list 'SP '-> (map-symbol-to-register-keyword 'SP)))
(print (list 'GP '-> (map-symbol-to-register-keyword 'GP)))
