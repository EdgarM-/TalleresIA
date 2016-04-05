;Edgar Manuel Amezquita
;Octavo Semestre
;Taller 2 Inteligencia Artificial

;Ejemplos
;'(forall (x y) (imp (and (P x) (Q y)) (forall x (R x))))
; 
;Funcion principal donde llama las funciones necesarias para poder pasar la formula a FNC
(defun main(exp)
	(disAndOr (applyNeg (resolveImp (resolveDoubleI (resolveEx (resolveFA exp () ))))))
)
;Resuelve el para todo haciendo el renombramiento de variable cuando es necesario, revisando las variable que se usaron previamente 
(defun resolveFA(exp vars)
	(cond ((null exp) exp)
		((equal (car exp) 'forall) (resolveFA (removeOp exp ()) (addvars (cdar exp) vars)))
		)	
	)
;Revisa si existe un elemento en una lista
(defun in(c lista)
	(cond ((null lista) nil)
		((equal c (car lista)) t)
		(t (in c (cdr lista)))
		)
)
;Resuelve la doble implicacion cambiando la por dos implicaciones en ambos sentidos.
(defun resolveDoubleI (exp)
	(cond ((equal (car exp) 'sys) (cons 'and (list (list 'imp  (resolveDoubleI (cadr exp)) (resolveDoubleI (caddr exp))) (list 'imp  (resolveDoubleI (caddr exp)) (resolveDoubleI (cadr exp))))))
		((not (in (car exp) '(sys imp or and not))) (append exp))
		(t (append (car exp)))

		)
	)
;Resuelve la implicacion cambiandola por no p o q.
(defun resolveImp (exp)
	(cond ((equal (car exp) 'imp) (cons 'or (list (list 'not  (resolveImp (cadr exp))) (resolveImp (caddr exp)))) )
		((not (in (car exp) '(imp or and not))) (append exp))
		(t (append (list (car exp) (list (resolveImp (cadr exp)) (resolveImp (caddr exp))))))
		)
	)

;Aplica la negacion donde esta se encuentre y la distribuye
(defun applyNeg (exp flag)
	
)

(defun newapplyNeg (exp)
	(cond ((equal (car exp) 'not) (append (newapplyNeg (cadr exp))))
		((not (in (car exp) '(or and not))) (append exp))
		(t (append (list (car exp) (newapplyNeg (cadr exp)))))

		)
	)
;Devuelve el largo de la lista l dadas
(defun len (l n)
	(cond ((null l) n)
		(t (len (cdr l) (+ 1 n)))
	)
	)