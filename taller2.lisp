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
(defun resolveDoubleI (exp res)
	(cond ((null exp) res)
		((equal (car exp) 'sys) (resolveDoubleI (cdr exp) (append res (list 'and))))
		((and (listp (car exp)) (equal (caar exp) 'sys)) (resolveDoubleI (append (car exp) (cdr exp)) res))
		((equal (len exp 0) 2) (resolveDoubleI (cdr exp) (append res (append (list 'imp) (list (car exp)))) ))
		(t (resolveDoubleI (cdr exp) (append res (list (car exp)))))
	)
	)
;Resuelve la implicacion cambiandola por no p o q.
(defun resolveImp (exp res)
	(cond ((null exp) res)
		((equal (car exp) 'imp) (resolveImp (cdr exp) (append res (list 'or))))
		((and (listp (car exp)) (equal (caar exp) 'imp)) (resolveImp (append (car exp) (cdr exp)) res))
		((>= (len exp 0) 2) (resolveImp (cdr exp) (append res (list (append (list 'not) (list (car exp)))) )))
		(t (resolveImp (cdr exp) (append res (list (car exp)))))
	)
	)

;Aplica la negacion donde esta se encuentre y la distribuye con ayuda de dos funciones
(defun applyNeg (exp res)
	(cond ((null exp) res)
		((equal (car exp) 'not) (append res (list (append (list 'not) (car exp)))))
		((and (listp (car exp)) (equal (caar exp) 'or)) (applyNeg (cdr exp) (disNegOr (car exp) res)))
		((and (listp (car exp)) (equal (caar exp) 'and)) (applyNeg (cdr exp) (disNegAnd (car exp) res)))
		(t (applyNeg (cdr exp) (append res (car exp))))
	)
)
;Devuelve el largo de la lista l dadas
(defun len (l n)
	(cond ((null l) n)
		(t (len (cdr l) (+ 1 n)))
	)
	)