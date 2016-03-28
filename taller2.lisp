;Edgar Manuel Amezquita
;Octavo Semestre
;Taller 2 Inteligencia Artificial

;Ejemplos
;'(forall (x y) (imp (and (P x) (Q y)) (forall x (R x))))
; 
;Funcion principal donde llama las funciones necesarias para poder pasar la formula a FNC
(defun main(exp res)
	(disAndOr (applyNeg (resolveImp (resolveDoubleI (resolveEx (resolveFA exp res () )))))))
	)

(defun resolveFA(exp vars)
	(cond ((null exp) exp)
		((equal (car exp) 'forall) (resolveFA (removeOp exp ()) (addvars (cdar exp) vars)))
		)
	)

(defun in(c lista)
	(cond ((null lista) nil)
		((equal c (car lista)) t)
		(t (in c (cdr lista)))
		)
)