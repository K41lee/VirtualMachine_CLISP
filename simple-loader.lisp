;;;; simple-loader.lisp
;;;; Version simplifiée du loader pour compilation en MIPS
;;;;
;;;; Ce loader est entièrement compilable en MIPS et fonctionne dans la VM.
;;;; Il copie du code d'une zone mémoire (data-addr) vers une autre (code-addr).
;;;;
;;;; Utilisé dans test-compilation-full-fibo.lisp pour démontrer que le
;;;; compilateur peut générer du code MIPS fonctionnel pour un loader basique.

(defun simple-loader (code-addr data-addr count)
  "Copie count mots de data-addr vers code-addr
   Retourne l'adresse de début du code copié (code-addr)"
  (let ((i 0))
    (while (< i count)
      (mem-write (+ code-addr i) (mem-read (+ data-addr i)))
      (setq i (+ i 1)))
    code-addr))

;;; ============================================================================
;;; RÉSULTAT DE COMPILATION
;;; ============================================================================
;;;
;;; Ce code génère 66 instructions MIPS qui:
;;; 1. Initialisent un compteur i à 0
;;; 2. Bouclent tant que i < count:
;;;    - Lisent mem[data-addr + i]
;;;    - Écrivent dans mem[code-addr + i]
;;;    - Incrémentent i
;;; 3. Retournent code-addr
;;;
;;; BUGS CORRIGÉS pour permettre cette compilation:
;;;
;;; 1. compile-comparison: Utilisait $S2/$S3 (registres paramètres) comme
;;;    temporaires, écrasant les paramètres dans les comparaisons.
;;;    → Correction: utilise $T0/$T1
;;;
;;; 2. compile-let/compile-setq: Offsets relatifs à $SP qui change pendant
;;;    les opérations arithmétiques, causant des boucles infinies.
;;;    → Correction: utilise $FP (frame pointer) avec offsets fixes
;;;
;;; 3. compile-mem-write-prim: Adresse destination dans $T0 écrasée lors
;;;    de la compilation de la valeur (mem-read utilise aussi $T0).
;;;    → Correction: sauvegarde l'adresse sur la pile avant compilation
;;;
;;; Performance: ~10 secondes pour charger et exécuter fibonacci(20)
;;; ============================================================================
