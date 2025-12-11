(defun reg-keyword-to-index (keyword)
  (case keyword
    (:$GP 28) (:$SP 29) (:$FP 30)
    (t 'NOT-FOUND)))

(print (list ':$GP '-> (reg-keyword-to-index :$GP)))
(print (list ':$gp '-> (reg-keyword-to-index :$gp)))
(print (list ':$SP '-> (reg-keyword-to-index :$SP)))
