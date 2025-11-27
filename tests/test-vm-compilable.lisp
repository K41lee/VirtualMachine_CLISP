;;;; test-vm-compilable.lisp
;;;; Tests basiques pour vm-compilable.lisp

(load "src/vm-compilable.lisp")

(format t "~%=== TEST VM-COMPILABLE ===~%~%")

;; Test 1: Initialisation
(format t "Test 1: Initialisation de la VM...~%")
(make-new-vm :verbose nil)
(format t "  État initial: ~A (attendu: ~A)~%" *vm-state* +state-ready+)
(assert (= *vm-state* +state-ready+) () "État devrait être READY")
(format t "  ✓ PASS~%~%")

;; Test 2: Registres
(format t "Test 2: Lecture/écriture registres...~%")
(set-register (get-reg :t0) 42)
(let ((val (get-register (get-reg :t0))))
  (format t "  Valeur $t0: ~A (attendu: 42)~%" val)
  (assert (= val 42) () "Registre devrait contenir 42"))
(format t "  ✓ PASS~%~%")

;; Test 3: Mémoire
(format t "Test 3: Lecture/écriture mémoire...~%")
(mem-write 100 123)
(let ((val (mem-read 100)))
  (format t "  Valeur mem[100]: ~A (attendu: 123)~%" val)
  (assert (= val 123) () "Mémoire devrait contenir 123"))
(format t "  ✓ PASS~%~%")

;; Test 4: Pile
(format t "Test 4: Pile (push/pop)...~%")
(push-stack 999)
(let ((val (pop-stack)))
  (format t "  Valeur dépilée: ~A (attendu: 999)~%" val)
  (assert (= val 999) () "Valeur dépilée devrait être 999"))
(format t "  ✓ PASS~%~%")

;; Test 5: Programme simple
(format t "Test 5: Exécution programme simple (HALT)...~%")
(make-new-vm :verbose nil)
(let ((code-start (calculate-code-start)))
  ;; Charger un simple HALT
  (mem-write code-start '(:HALT))
  (set-register (get-reg :pc) code-start)
  (run-vm 100)
  (format t "  État final: ~A (attendu: ~A)~%" *vm-state* +state-halted+)
  (assert (= *vm-state* +state-halted+) () "État devrait être HALTED"))
(format t "  ✓ PASS~%~%")

(format t "=== TOUS LES TESTS PASSÉS ! ===~%")
(quit)
